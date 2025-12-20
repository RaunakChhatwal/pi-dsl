module TypeCheck where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (forM_, when)
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal qualified
import PrettyPrint (D(DS, DD), Disp (disp), ppr)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), render)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.String.Interpolate (i)
import Control.Monad.Trans (lift)

reduceTyPi :: (Type, Unbound.Bind TName Type) -> Term -> TcMonad Type
reduceTyPi (paramType, returnType) arg = do
  checkType arg paramType
  return $ Unbound.instantiate returnType [arg]

-- Infer/synthesize the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> Env.lookupType var

  TyType -> return TyType

  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    checkType paramType TyType
    Env.addLocal paramName paramType $ checkType returnType TyType
    return TyType

  App func arg -> do
    funcType <- inferType func
    Equal.whnf funcType >>= \case
      Pi paramType bind -> reduceTyPi (paramType, bind) arg
      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann a tyA -> do
    checkType tyA TyType
    checkType a tyA
    return tyA

  TyCon typeName -> fst <$> Env.lookupDataType typeName

  DataCon typeName ctorName -> Env.lookupCtor (typeName, ctorName)

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that the given term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType tm ty = traceM "checkType" [ppr tm, ppr ty] (const "") $ do
  ty' <- Equal.whnf ty
  case tm of
    Lam bodyBind -> case ty' of
      Pi paramType typeBind -> do
        -- unbind the variables in the lambda expression and pi type
        (var, body, _, type') <- lift $ Unbound.unbind2Plus bodyBind typeBind
        -- check the type of the body of the lambda expression
        Env.addLocal var paramType $ checkType body type'
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD ty']

    TrustMe -> return ()

    _ -> do
      tyA <- inferType tm
      Equal.equate tyA ty'

unfoldPi :: Type -> [Type] -> TcMonad ([Type], Type)
unfoldPi (Pi paramType bind) paramTypes = do
  (_, returnType) <- Unbound.unbind bind
  unfoldPi returnType (paramType : paramTypes)
unfoldPi returnType paramTypes = return (reverse paramTypes, returnType)

checkCtorReturnsSelf :: CtorName -> TypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (TyCon typeName) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ =
  Env.err [DS "Constructor", DD ctorName, DS "must return", DD self]

throwIfFound :: TypeName -> Type -> TcMonad ()
throwIfFound typeName type' = Equal.whnf type' >>= \case
  TyType -> return ()
  Var _ -> return ()
  Lam bind -> do
    (_, body) <- Unbound.unbind bind
    throwIfFound typeName body
  App a b -> mapM_ (throwIfFound typeName) [a, b]
  Pi paramType bind -> do
    throwIfFound typeName paramType
    (_, returnType) <- Unbound.unbind bind
    throwIfFound typeName returnType
  Ann term _ -> throwIfFound typeName term
  TrustMe -> return ()
  TyCon name -> when (name == typeName) $
    Env.err [DS "Invalid occurence of data type", DD typeName, DS "being declared"]
  DataCon _ _ -> return ()

checkStrictPositivity :: TypeName -> Type -> TcMonad ()
checkStrictPositivity self paramType = Equal.whnf paramType >>= \case
  App a b -> do
    throwIfFound self b
    checkStrictPositivity self a
  Pi paramType bind -> do
    throwIfFound self paramType
    (_, returnType) <- Unbound.unbind bind
    checkStrictPositivity self returnType
  _ -> return ()

-- | Check each sort of declaration in a module
tcEntry :: Entry -> TcMonad ()
tcEntry entry = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl var hint term -> Env.lookupDecl var >>= \case
    Just _ -> Env.err [DD var, DS "already defined"]
    Nothing -> checkType term hint `catchError` handler where
      handler (Env.Err msg) = throwError $ Env.Err (msg $$ msg')
      msg' = disp [DS "When checking the term", DD term, DS "against the type", DD hint]
  dataDecl@(Data typeName typeParams ctors) -> do
    checkType typeParams TyType
    forM_ ctors $ \(ctorName, ctorType) -> do
      Env.addDataType typeName typeParams [] $ checkType ctorType TyType
      (paramTypes, returnType) <- unfoldPi ctorType []
      checkCtorReturnsSelf ctorName typeName =<< Equal.whnf returnType
      mapM (checkStrictPositivity typeName) paramTypes

tcEntries :: [Entry] -> TcMonad ()
tcEntries [] = return ()
tcEntries (entry : rest) = tcEntry entry >> case entry of
  Decl var type' def -> Env.addDecl var type' def (tcEntries rest)
  Data name params ctors -> Env.addDataType name params ctors (tcEntries rest)