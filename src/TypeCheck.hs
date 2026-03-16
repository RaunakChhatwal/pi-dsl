module TypeCheck where

import Control.Monad (unless, when, replicateM)
import Control.Monad.Extra (whenM)
import Control.Monad.Reader (asks, local, ask)
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
          go (App func mvar) $ Unbound.instantiate binder [mvar]
        Pi Explicit paramType _ -> App func <$> elaborateAgainst arg paramType
        _ -> err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Const constant levels -> do
    (univParams, _) <- lookUpConst constant
    levelMVars <- replicateM (length univParams - length levels) newLevelMVar
    return $ Const constant $ levels ++ levelMVars

  Pi binderInfo paramType binder -> do
    paramType <- elaborate paramType
    (paramName, returnType) <- Unbound.unbind binder
    returnType <- addLocal paramName paramType $ elaborate returnType
    return $ Pi binderInfo paramType $ Unbound.bind paramName returnType

  Ann term type' -> do
    type' <- elaborate type'
    term <- elaborateAgainst term type'
    return $ Ann term type'

  Lam binderInfo binder -> do
    type' <- newMVar . Sort =<< newLevelMVar
    (name, body) <- Unbound.unbind binder
    body <- addLocal name type' $ elaborate body
    return $ Lam binderInfo $ Unbound.bind name body

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
          return $ Lam Implicit $ Unbound.bind paramName term

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
  Sort level -> return level
  type' -> err [DS "Expected type but got", DD term, DS "with type", DD type']

-- Infer the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  LVar name -> lookUpLVarType name

  MVar id -> lookUpMVarType id

  Sort level -> return $ Sort $ Succ level

  Lam binderInfo binder -> do
    paramType <- newMVar . Sort =<< newLevelMVar
    (paramName, body) <- Unbound.unbind binder
    returnType <- addLocal paramName paramType $ inferType body
    return $ Pi binderInfo paramType $ Unbound.bind paramName returnType

  Pi _ paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    paramSortLevel <- ensureType paramType
    returnSortLevel <- addLocal paramName paramType $ ensureType returnType
    return $ Sort $ Max paramSortLevel returnSortLevel

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

  Const constant levels -> do
    (univParams, type') <- lookUpConst constant
    unless (length levels == length univParams) $
      throwError [i|Expected #{length univParams} universe arguments for #{ppr term}|]
    return $ substLevels (zip univParams levels) type'

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

checkClosedLevel :: [UnivParamName] -> Level -> TcMonad ()
checkClosedLevel univParams = \case
  Zero -> return ()
  Succ level -> checkClosedLevel univParams level
  Max level1 level2 -> checkClosedLevel univParams level1 >> checkClosedLevel univParams level2
  Param univParamName -> unless (univParamName `elem` univParams) $
    throwError [i|Closed check failed: free universe parameter #{univParamName}|]
  LMVar id -> throwError [i|Closed check failed: unresolved universe meta ?u#{id}|]

-- Verify that a term has no free/meta variables
checkClosed :: [UnivParamName] -> Term -> TcMonad ()
checkClosed univParams = \case
  Sort level -> checkClosedLevel univParams level
  LVar var -> when (Unbound.isFreeName var) $
    throwError [i|Closed check failed: free local #{ppr var}|]
  MVar id -> throwError [i|Closed check failed: unresolved meta ?#{id}|]
  Const _ levels -> mapM_ (checkClosedLevel univParams) levels
  Lam _ (Unbound.B _ body) -> checkClosed univParams body
  App func arg -> checkClosed univParams func >> checkClosed univParams arg
  Pi _ paramType (Unbound.B _ returnType) ->
    checkClosed univParams paramType >> checkClosed univParams returnType
  Ann term type' -> checkClosed univParams term >> checkClosed univParams type'

-- Type-check a single top-level entry and return its elaborated, instantiated form
addEntry :: Entry -> TcMonad Env
addEntry entry = traceM "addEntry" [ppr entry] (const "") $ case entry of
  Decl var univParams type' term -> do
    whenM (isJust <$> lookUpDecl var) $ throwError [i|Name conflict when declaring variable #{var}|]

    checkClosed univParams type'
    checkClosed univParams term

    type' <- elaborate type'
    _ <- ensureType type'
    term <- elaborateAgainst term type'
    checkType term type'

    type' <- instantiateMVars type'
    term <- instantiateMVars term
    checkClosed univParams type'
    checkClosed univParams term

    addDecl var (DeclInfo univParams type' term) ask

  Data typeName univParams signature ctors -> do
    whenM (asks $ Map.member typeName . (.datatypes)) $
      throwError [i|Name conflict when declaring data type #{typeName}|]

    checkClosed univParams signature
    mapM_ (checkClosed univParams . snd) ctors

    signature <- elaborate signature
    let addSelfToEnv env = env { dataTypeBeingDeclared = Just (typeName, (univParams, signature)) }
    ctors <- sequence
      [(ctorName,) <$> local addSelfToEnv (elaborate ctorType) | (ctorName, ctorType) <- ctors]
    checkDataTypeDecl typeName signature ctors addSelfToEnv

    signature <- instantiateMVars signature
    ctors <- sequence [(ctorName,) <$> instantiateMVars ctorType | (ctorName, ctorType) <- ctors]
    checkClosed univParams signature
    mapM_ (checkClosed univParams . snd) ctors

    let candidateNames = ("u" :) $ map (\i -> "u" ++ show i) [1 :: Int ..]
    let recursorUnivParam = head $ filter (not . (`elem` univParams)) candidateNames
    let motiveReturnType = Sort $ Param recursorUnivParam
    recursorType <- synthesizeRecursorType typeName univParams signature ctors motiveReturnType
    checkClosed (recursorUnivParam : univParams) recursorType

    let dataTypeInfo = DataTypeInfo univParams signature ctors recursorUnivParam recursorType
    addDataType typeName dataTypeInfo ask
