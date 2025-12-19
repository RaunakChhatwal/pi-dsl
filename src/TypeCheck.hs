module TypeCheck where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (forM_)
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal qualified
import PrettyPrint (D(DS, DD), Disp (disp), ppr, paramsToPi)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), render)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.String.Interpolate (i)
import Data.Functor ((<&>))

betaReduceTyPi :: (Type, Unbound.Bind TName Type) -> Term -> TcMonad Type
betaReduceTyPi (paramType, returnType) arg = do
  checkType arg paramType
  return $ Unbound.instantiate returnType [arg]

-- Infer/synthesize the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> Env.lookupType var

  TyType -> return TyType

  TyPi tyA bnd -> do
    (x, tyB) <- Unbound.unbind bnd
    checkType tyA TyType
    Env.addLocal x tyA $ checkType tyB TyType
    return TyType

  App func arg -> do
    funcType <- inferType func
    Equal.whnf funcType >>= \case
      TyPi paramType returnType -> betaReduceTyPi (paramType, returnType) arg

      typeCtor@(TyCon _) -> inferType typeCtor >>= \case
        TyType -> undefined -- TODO: throw error message
        TyPi paramType returnType -> betaReduceTyPi (paramType, returnType) arg
        _ -> undefined

      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann a tyA -> do
    checkType tyA TyType
    checkType a tyA
    return tyA

  -- Type constructor application
  TyCon typeName -> do
    (typeParams, _) <- Env.lookupDataType typeName
    return $ paramsToPi typeParams TyType

  DataCon typeName ctorName -> Env.lookupCtor (typeName, ctorName) <&>
    \(_, CtorDef _ ctorParams returnType) -> paramsToPi ctorParams returnType

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that the given term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType tm ty = traceM "checkType" [ppr tm, ppr ty] (const "") $ do
  ty' <- Equal.whnf ty
  case tm of
    Lam bnd -> case ty' of
      TyPi tyA bnd2 -> do
        -- unbind the variables in the lambda expression and pi type
        (x, body, tyB) <- unbind2 bnd bnd2
        -- check the type of the body of the lambda expression
        Env.addLocal x tyA $ checkType body tyB
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD ty']

    TrustMe -> return ()

    _ -> do
      tyA <- inferType tm
      Equal.equate tyA ty'

-- | Check each sort of declaration in a module
tcEntry :: Entry -> TcMonad ()
tcEntry entry = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl var hint term -> Env.lookupDecl var >>= \case
    Just _ -> Env.err [DD var, DS "already defined"]
    Nothing -> checkType term hint `catchError` handler where
      handler (Env.Err msg) = throwError $ Env.Err (msg $$ msg')
      msg' = disp [DS "When checking the term", DD term, DS "against the type", DD hint]
  dataDecl@(Data typeName typeParams ctors) -> do
    checkType (paramsToPi typeParams TyType) TyType
    -- TODO: check strict positivity, returnType is self
    forM_ ctors $ \(CtorDef name params returnType) ->
      Env.addDataType typeName typeParams [] $ checkType (paramsToPi params returnType) TyType
