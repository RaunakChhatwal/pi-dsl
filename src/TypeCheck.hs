module TypeCheck where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans (lift)
import Text.PrettyPrint.HughesPJ (($$), render)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal (equate, whnf)
import PrettyPrint (D(DS, DD), Disp (disp), ppr)
import Syntax (Entry(Data, Decl), Term(..), Type)
import Inductive (checkDataTypeDecl, synthesizeRecursorType, unfoldPi)

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
    whnf funcType >>= \case
      Pi paramType bind -> do
        checkType arg paramType
        return $ Unbound.instantiate bind [arg]
      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann term type' -> do
    checkType type' TyType
    checkType term type'
    return type'

  DataType typeName -> do
    typeSignature <- fst <$> Env.lookupDataType typeName
    checkType typeSignature TyType
    return typeSignature

  Ctor typeName ctorName -> do
    ctorType <- Env.lookupCtor (typeName, ctorName)
    checkType ctorType TyType
    return ctorType

  Rec typeName -> synthesizeRecursorType typeName

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that the given term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType term type' = traceM "checkType" [ppr term, ppr type'] (const "") $
  case term of
    Lam bodyBind -> whnf type' >>= \case
      Pi paramType typeBind -> do
        -- unbind the variables in the lambda expression and pi type
        (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBind typeBind
        -- check the type of the body of the lambda expression
        Env.addLocal var paramType $ checkType body returnType
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD type']

    TrustMe -> return ()

    _ -> flip equate type' =<< inferType term

tcEntry :: Entry -> TcMonad ()
tcEntry entry = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl var hint term -> Env.lookupDecl var >>= \case
    Just _ -> Env.err [DD var, DS "already defined"]
    Nothing -> checkType term hint `catchError` handler where
      handler (Env.Err msg) = throwError $ Env.Err (msg $$ msg')
      msg' = disp [DS "When checking the term", DD term, DS "against the type", DD hint]
  dataDecl@(Data typeName typeParams ctors) -> checkDataTypeDecl typeName typeParams ctors

withEntries :: [Entry] -> TcMonad a -> TcMonad a
withEntries [] monad = monad
withEntries (entry : rest) monad = tcEntry entry >> case entry of
  Decl var type' def -> Env.addDecl var type' def (withEntries rest monad)
  Data name params ctors -> Env.addDataType name params ctors (withEntries rest monad)

tcEntries :: [Entry] -> TcMonad ()
tcEntries = flip withEntries $ return ()