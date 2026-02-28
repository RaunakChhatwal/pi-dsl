module TypeCheck where

import Control.Monad (when, unless)
import Control.Monad.Extra (whenM)
import Control.Monad.Reader (asks, local)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Bind qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Environment
import Equal
import PrettyPrint
import Syntax
import Inductive

elaborate :: Term -> TcMonad Term
elaborate term = traceM "elaborate" [ppr term] ppr $ case term of
  App func arg -> do
    func <- elaborate func
    funcType <- inferType func
    go func funcType where
      go func funcType = whnf funcType >>= \case
        Pi Implicit paramType binder -> do
          mvar <- newMVar paramType
          go (App func mvar) (Unbound.instantiate binder [mvar])
        Pi Explicit paramType _ -> App func <$> elaborateAgainst arg paramType
        _ -> err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Pi binderInfo paramType binder -> do
    paramType <- elaborate paramType
    (paramName, returnType) <- Unbound.unbind binder
    returnType <- addLocal paramName paramType $ elaborate returnType
    return $ Pi binderInfo paramType $ Unbound.bind paramName returnType

  Ann term type' -> do
    type' <- elaborate type'
    term <- elaborateAgainst term type'
    return $ Ann term type'

  Lam _ _ -> throwError "Unguided lambda elaboration not implemented"

  _ -> return term

-- Elaborate a term against an expected type
elaborateAgainst :: Term -> Type -> TcMonad Term
elaborateAgainst term expectedType = traceM "elaborateAgainst" [ppr term, ppr expectedType] ppr $
  case term of
    Lam lamBinderInfo bodyBinder -> whnf expectedType >>= \case
      Pi piBinderInfo paramType returnTypeBinder -> case (lamBinderInfo, piBinderInfo) of
        (Explicit, Implicit) -> do
          (paramName, returnType) <- Unbound.unbind returnTypeBinder
          term <- addLocal paramName paramType $ elaborateAgainst term returnType
          return $ Lam Implicit (Unbound.bind paramName term)

        _ | lamBinderInfo == piBinderInfo -> do
          (paramName, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBinder returnTypeBinder
          body <- addLocal paramName paramType $ elaborateAgainst body returnType
          return $ Lam lamBinderInfo $ Unbound.bind paramName body

        _ -> throwError "Expected explicit parameter but received implicit lambda"

      _ -> err [DS "Lambda expression should have a function type, not", DD expectedType]

    _ -> elaborate term

-- Remove implicit applications from an elaborated term
delaborate :: Term -> TcMonad Term
delaborate term = traceM "delaborate" [ppr term] ppr $ case term of
  App func arg -> do
    funcType <- inferType func
    whnf funcType >>= \case
      Pi Implicit _ _ -> delaborate func
      Pi Explicit paramType _ -> App <$> delaborate func <*> delaborateAgainst arg paramType
      _ -> err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    Pi binderInfo <$> delaborate paramType <*>
      (Unbound.bind paramName <$> addLocal paramName paramType (delaborate returnType))

  Ann term type' -> Ann <$> delaborateAgainst term type' <*> delaborate type'

  Lam _ _ -> throwError "Unguided lambda delaboration not implemented"

  _ -> return term

-- Delaborate a term against an expected type
delaborateAgainst :: Term -> Type -> TcMonad Term
delaborateAgainst term expectedType = traceM "delaborateAgainst" [ppr term, ppr expectedType] ppr $
  case term of
    Lam lamBinderInfo bodyBinder -> whnf expectedType >>= \case
      Pi piBinderInfo paramType returnTypeBinder -> do
        unless (lamBinderInfo == piBinderInfo) $
          throwError "Lambda binder does not match expected function binder"
        (paramName, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBinder returnTypeBinder
        body <- addLocal paramName paramType $ delaborateAgainst body returnType
        if lamBinderInfo == Implicit && paramName `notElem` toListOf Unbound.fv body
          then return body
          else return $ Lam lamBinderInfo $ Unbound.bind paramName body

      _ -> err [DS "Lambda expression should have a function type, not", DD expectedType]

    _ -> delaborate term

-- Check that a term is a type and return its universe level
ensureType :: Term -> TcMonad Level
ensureType term = inferType term >>= whnf >>= \case
  Sort u -> return u
  type' -> err [DS "Expected type but got", DD term, DS "with type", DD type']

-- Infer the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> lookUpType var

  Sort level -> return $ Sort $ Succ level

  Lam _ _ -> throwError "Lambda inference not implemented (add a type annotation)"

  Pi _ paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    paramSortLevel <- ensureType paramType
    returnSortLevel <- addLocal paramName paramType $ ensureType returnType
    return $ Sort $ maxLevel paramSortLevel returnSortLevel

  App func arg -> do
    funcType <- inferType func
    whnf funcType >>= \case
      Pi _ paramType binder -> do
        checkType arg paramType
        return $ Unbound.instantiate binder [arg]
      _ -> err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann term type' -> do
    _ <- ensureType type'
    checkType term type'
    return type'

  DataType typeName -> lookUpTypeOfDataType typeName

  Ctor typeName ctorName -> lookUpCtor (typeName, ctorName)

  Rec typeName -> synthesizeRecursorType typeName

-- Check that a term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType term type' = traceM "checkType" [ppr term, ppr type'] (const "") $
  case term of
    Lam lamBinderInfo bodyBinder -> whnf type' >>= \case
      Pi piBinderInfo paramType returnTypeBinder -> case (lamBinderInfo, piBinderInfo) of
        (Explicit, Implicit) -> do
          (paramName, returnType) <- Unbound.unbind returnTypeBinder
          addLocal paramName paramType $ checkType term returnType
        _ | lamBinderInfo == piBinderInfo -> do
          (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBinder returnTypeBinder
          addLocal var paramType $ checkType body returnType
        _ -> throwError "Expected explicit parameter but received implicit lambda"
      _ -> err [DS "Lambda expression should have a function type, not", DD type']

    _ -> do
      inferredType <- inferType term
      unify inferredType type'

-- Verify that a term has no free/meta variables
checkClosed :: Term -> TcMonad ()
checkClosed (Var (Local var)) = when (Unbound.isFreeName var) $
  throwError [i|Closed check failed: free local #{ppr var}|]
checkClosed (Var (Meta id)) = throwError [i|Closed check failed: unresolved meta ?#{id}|]
checkClosed (Lam _ (Unbound.B _ body)) = checkClosed body
checkClosed (App func arg) = checkClosed func >> checkClosed arg
checkClosed (Pi _ paramType (Unbound.B _ returnType)) =
  checkClosed paramType >> checkClosed returnType
checkClosed (Ann term type') = checkClosed term >> checkClosed type'
checkClosed _ = return ()

-- Type-check a single top-level entry and return its elaborated, instantiated form
checkEntry :: Entry -> TcMonad Entry
checkEntry entry = traceM "tcEntry" [ppr entry] ppr $ case entry of
  Decl var type' term -> do
    whenM (isJust <$> lookUpDecl var) $ throwError [i|Name conflict when declaring variable #{var}|]

    checkClosed type'
    checkClosed term

    type' <- elaborate type'
    _ <- ensureType type'
    term <- elaborateAgainst term type'
    checkType term type'

    type' <- instantiateMVars type'
    term <- instantiateMVars term
    checkClosed type'
    checkClosed term

    return $ Decl var type' term

  Data typeName typeSignature ctors -> do
    whenM (asks $ Map.member typeName . datatypes) $
      throwError [i|Name conflict when declaring data type #{typeName}|]

    checkClosed typeSignature
    mapM_ (checkClosed . snd) ctors

    typeSignature <- elaborate typeSignature
    let addSelfToEnv env = env { dataTypeBeingDeclared = Just (typeName, typeSignature) }
    ctors <- sequence
      [(ctorName,) <$> local addSelfToEnv (elaborate ctorType) | (ctorName, ctorType) <- ctors]
    checkDataTypeDecl typeName typeSignature ctors

    typeSignature <- instantiateMVars typeSignature
    ctors <- sequence [(ctorName,) <$> instantiateMVars ctorType | (ctorName, ctorType) <- ctors]
    checkClosed typeSignature
    mapM_ (checkClosed . snd) ctors

    return $ Data typeName typeSignature ctors
