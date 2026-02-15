module TypeCheck where

import Control.Monad (join, when)
import Control.Monad.Trans (lift)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Bind qualified as Unbound
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal (unify, whnf)
import PrettyPrint (D(DS, DD), ppr)
import Syntax
import Inductive (checkDataTypeDecl, synthesizeRecursorType)
import Control.Monad.Reader (local)

elaborate :: Term -> TcMonad Term
elaborate term = traceM "elaborate" [ppr term] ppr $ case term of
  App func arg -> do
    elaboratedFunc <- elaborate func
    funcType <- inferType elaboratedFunc
    go elaboratedFunc funcType where
      go func funcType = whnf funcType >>= \case
        Pi Implicit paramType binder -> do
          mvar <- Env.newMVar paramType
          go (App func mvar) (Unbound.instantiate binder [mvar])
        Pi Explicit paramType _ -> App func <$> elaborateAgainst arg paramType
        _ -> App func <$> elaborate arg

  Pi binderInfo paramType binder -> do
    elaboratedParamType <- elaborate paramType
    (paramName, returnType) <- Unbound.unbind binder
    elaboratedReturnType <- Env.addLocal paramName elaboratedParamType $ elaborate returnType
    return $ Pi binderInfo elaboratedParamType $ Unbound.bind paramName elaboratedReturnType

  Ann term' type' -> do
    elaboratedType <- elaborate type'
    elaboratedTerm <- elaborateAgainst term' elaboratedType
    return $ Ann elaboratedTerm elaboratedType

  Lam _ _ -> Env.err [DS "Unguided lambda elaboration not implemented"]

  _ -> return term

-- Elaborate a term against an expected type.
-- This is primarily used to eta-expand implicit binders (so the term arity matches its type)
-- and to elaborate inside lambda bodies under the right local context.
elaborateAgainst :: Term -> Type -> TcMonad Term
elaborateAgainst term expectedType = traceM "elaborateAgainst" [ppr term, ppr expectedType] ppr $
  whnf expectedType >>= \case
    Pi Implicit paramType returnTypeBinder -> case term of
      Lam Implicit bodyBinder -> do
        (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBinder returnTypeBinder
        elaboratedBody <- Env.addLocal var paramType $ elaborateAgainst body returnType
        return $ Lam Implicit (Unbound.bind var elaboratedBody)
      Lam Explicit _ -> do
        (var, returnType) <- Unbound.unbind returnTypeBinder
        elaboratedBody <- Env.addLocal var paramType $ elaborateAgainst term returnType
        return $ Lam Implicit (Unbound.bind var elaboratedBody)
      _ -> elaborate term

    Pi Explicit paramType typeBinder -> case term of
      Lam Explicit termBinder -> do
        (var, body, _, returnType) <- lift $ Unbound.unbind2Plus termBinder typeBinder
        elaboratedBody <- Env.addLocal var paramType $ elaborateAgainst body returnType
        return $ Lam Explicit (Unbound.bind var elaboratedBody)
      Lam Implicit _ -> Env.err [DS "Expected explicit parameter but received implicit lambda"]
      _ -> elaborate term

    _ -> elaborate term

-- Check that a term is a type and return its universe level
ensureType :: Term -> TcMonad Level
ensureType term = inferType term >>= whnf >>= \case
  Sort u -> return u
  type' -> Env.err [DS "Expected type but got", DD term, DS "with type", DD type']

-- Infer the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> Env.lookUpType var

  Sort level -> return $ Sort $ Succ level

  Lam _ _ -> Env.err [DS "Lambda inference not implemented (add a type annotation)"]

  Pi _ paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    paramSortLevel <- ensureType paramType
    returnSortLevel <- Env.addLocal paramName paramType $ ensureType returnType
    return $ Sort $ maxLevel paramSortLevel returnSortLevel

  App func arg -> do
    funcType <- inferType func
    whnf funcType >>= \case
      Pi _ paramType binder -> do
        checkType arg paramType
        return $ Unbound.instantiate binder [arg]
      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann term type' -> do
    checkType term type'
    return type'

  DataType typeName -> Env.lookUpTypeOfDataType typeName

  Ctor typeName ctorName -> Env.lookUpCtor (typeName, ctorName)

  Rec typeName -> synthesizeRecursorType typeName

-- Check that a term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType term type' = traceM "checkType" [ppr term, ppr type'] (const "") $
  case term of
    Lam lamBinderInfo bodyBinder -> whnf type' >>= \case
      Pi piBinderInfo paramType returnTypeBinder -> case (lamBinderInfo, piBinderInfo) of
        (Explicit, Implicit) -> do
          mvar <- Env.newMVar paramType
          checkType term $ Unbound.instantiate returnTypeBinder [mvar]
        _ | lamBinderInfo == piBinderInfo -> do
          (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBinder returnTypeBinder
          Env.addLocal var paramType $ checkType body returnType
        _ -> Env.err [DS "Expected explicit parameter but received implicit lambda"]
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD type']

    _ -> do
      inferredType <- inferType term
      unify inferredType type'

-- Verify that a term has no free/meta variables
checkClosed :: Term -> TcMonad ()
checkClosed (Var (Local var)) = when (Unbound.isFreeName var) $
  Env.err [DS "Expected closed expressions as input but found free variable:", DD var]
checkClosed mvar@(Var (Meta _)) =
  Env.err [DS "Expected closed expressions as input but found metavariable:", DD mvar]
checkClosed (Lam _ (Unbound.B _ body)) = checkClosed body
checkClosed (App func arg) = checkClosed func >> checkClosed arg
checkClosed (Pi _ paramType (Unbound.B _ returnType)) =
  checkClosed paramType >> checkClosed returnType
checkClosed (Ann term type') = checkClosed term >> checkClosed type'
checkClosed _ = return ()

-- Type-check a single top-level entry
addEntry :: Entry -> TcMonad a -> TcMonad (TcMonad a)
addEntry entry continuation = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl var type' term -> do
    checkClosed type'
    checkClosed term
    elaboratedType <- elaborate type'
    _ <- ensureType elaboratedType
    elaboratedTerm <- elaborateAgainst term elaboratedType
    checkType elaboratedTerm elaboratedType
    return $ Env.addDecl var elaboratedType elaboratedTerm continuation
  Data typeName typeSignature ctors -> do
    checkClosed typeSignature
    mapM_ (checkClosed . snd) ctors
    elaboratedTypeSignature <- elaborate typeSignature
    let addSelfToEnv env =
          env { Env.dataTypeBeingDeclared = Just (typeName, elaboratedTypeSignature) }
    elaboratedCtors <- sequence
      [(ctorName,) <$> local addSelfToEnv (elaborate ctorType) | (ctorName, ctorType) <- ctors]
    checkDataTypeDecl typeName elaboratedTypeSignature elaboratedCtors
    return $ Env.addDataType typeName elaboratedTypeSignature elaboratedCtors continuation

-- Run a computation with entries added to the environment
withEntries :: [Entry] -> TcMonad a -> TcMonad a
withEntries rest monad =
  foldr (\entry continuation -> join $ addEntry entry continuation) monad rest

-- Type-check a list of top-level entries
tcEntries :: [Entry] -> TcMonad ()
tcEntries = flip withEntries $ return ()
