module Inductive (checkDataTypeDecl, reduceRecursor, synthesizeRecursorType) where

import Control.Applicative (Alternative(empty))
import Control.Exception (assert)
import Control.Monad ((>=>), forM, forM_, guard, when)
import Control.Monad.Reader (local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment (err, lookUpDataType, newMVar, TC, TcMonad, traceM, Env(dataTypeBeingDeclared))
import {-# SOURCE #-} Equal (whnf)
import PrettyPrint (D(DS, DD), ppr)
import Syntax
import {-# SOURCE #-} TypeCheck (ensureType)


-- Generate a fresh variable name from a string prefix
freshName :: String -> TcMonad TermName
freshName s = Unbound.fresh (Unbound.string2Name s)

-- Prepend a Pi-bound parameter to a type
addParam :: (BinderInfo, TermName, Type) -> Type -> Type
addParam (binderInfo, paramName, paramType) = Pi binderInfo paramType . Unbound.bind paramName

-- Derive the motive type from a data type's signature
motiveFromTypeSignature :: TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromTypeSignature hole typeName paramNames = whnf >=> \case
  Pi binderInfo paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    addParam (binderInfo, paramName, paramType) <$>
      motiveFromTypeSignature hole typeName (paramName : paramNames) returnType
  returnType -> do
    let fullyApplied = foldl App (DataType typeName) $ map lVar $ reverse paramNames
    case returnType of
      Sort u -> return $ addParam (Explicit, hole, fullyApplied) $ Sort u
      _ -> err [DS "Expected data type signature to return a Sort, but got", DD returnType]

-- Generate induction hypothesis type from a constructor parameter
motiveFromCtorParam ::
  TermName -> DataTypeName -> [TermName] -> [Term] -> TermName -> Type -> TcMonad (Maybe Type)
motiveFromCtorParam motive self paramNames args name = whnf >=> \case
  Pi binderInfo paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (addParam (binderInfo, paramName, paramType)) <$>
      motiveFromCtorParam motive self (paramName : paramNames) [] name returnType
  App f arg -> motiveFromCtorParam motive self paramNames (arg : args) name f
  DataType typeName | typeName == self ->
    return $ Just $ App motiveApplied paramApplied where
    motiveApplied = foldl App (lVar motive) args
    paramApplied = foldl App (lVar name) $ map lVar (reverse paramNames)
  _ -> return Nothing

-- Compute the return type of a recursor case from constructor return type
motiveFromCtorReturnType :: TermName -> Term -> [Term] -> Type -> TcMonad Type
motiveFromCtorReturnType motive ctor args = whnf >=> \case
  App f arg -> motiveFromCtorReturnType motive ctor (arg : args) f
  _ -> return $ App (foldl App (lVar motive) args) ctor

-- Build the type of a recursor case from a constructor definition
recursorCaseFromCtor
  :: TermName -> TermName -> CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor motive hole ctorName typeName ctor = whnf >=> \case
  Pi binderInfo paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    let ctorApplied = if binderInfo == Explicit then App ctor (lVar paramName) else ctor
    rest <- recursorCaseFromCtor motive hole ctorName typeName ctorApplied returnType
    motiveFromCtorParam motive typeName [] [] paramName paramType <&> \case
      Nothing -> addParam (binderInfo, paramName, paramType) rest
      Just hypothesis ->
        addParam (binderInfo, paramName, paramType) $
          addParam (Explicit, hole, hypothesis) rest
  returnType -> motiveFromCtorReturnType motive ctor [] returnType

-- Derive motive application type for the scrutinee parameter
motiveFromSelf :: TermName -> TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromSelf motive selfName typeName paramNames = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    addParam (binderInfo, paramName, paramType) <$>
      motiveFromSelf motive selfName typeName (paramName : paramNames) returnType
  _ -> do
    let selfApplied = foldl App (DataType typeName) $ map lVar $ reverse paramNames
    let motiveApplied =
          App (foldl App (lVar motive) $ map lVar $ reverse paramNames) (lVar selfName)
    return $ addParam (Explicit, selfName, selfApplied) motiveApplied

-- Synthesize the full type of a recursor for a data type
synthesizeRecursorType :: DataTypeName -> TcMonad Type
synthesizeRecursorType typeName = do
  (signature, ctorDefs) <- lookUpDataType typeName

  -- Use fresh names to avoid capturing user variables like `motive`/`self`.
  motive <- freshName "motive"
  selfName <- freshName "self"
  hole <- freshName "_"

  motiveType <- motiveFromTypeSignature hole typeName [] signature
  cases <- forM ctorDefs $ \(ctorName, ctorType) ->
    recursorCaseFromCtor motive hole ctorName typeName (Ctor typeName ctorName) ctorType

  motiveParam <- motiveFromSelf motive selfName typeName [] signature
  return $ addParam (Explicit, motive, motiveType) $
    foldr (addParam . (Explicit, hole,)) motiveParam cases

-- Unfold a nested Pi type into parameter types and return type
unfoldPi :: TC m => Type -> m ([Type], Type)
unfoldPi (Pi _ paramType binder) = do
  (_, returnType) <- Unbound.unbind binder
  first (paramType :) <$> unfoldPi returnType
unfoldPi returnType = return ([], returnType)

-- Split recursor arguments into cases, scrutinee, and extra args
splitReducerArgs :: Int -> Int -> [Term] -> MaybeT TcMonad ([Term], (Term, [Term]))
splitReducerArgs 0 0 (scrutinee : rest) = return ([], (scrutinee, rest))
splitReducerArgs 0 m (_ : rest) = splitReducerArgs 0 (m - 1) rest
splitReducerArgs n m (arg : rest) = first (arg :) <$> splitReducerArgs (n - 1) m rest
splitReducerArgs _ _ _ = empty

-- Generate induction hypothesis term for a recursive parameter
hypothesisForParam ::
  DataTypeName -> Term -> Type -> [(TermName, Term)] -> Term -> TcMonad (Maybe Term)
hypothesisForParam typeName partialRecursor paramType prevCtorArgs ctorArg = go paramType [] []
  where
    go paramType typeArgs paramNames = whnf paramType >>= \case
      Pi binderInfo _ binder -> do
        (paramName, returnType) <- Unbound.unbind binder
        fmap (Lam binderInfo . Unbound.bind paramName) <$>
          go returnType typeArgs (paramName : paramNames)
      App typeCtor typeArg ->
        go typeCtor (Unbound.substs prevCtorArgs typeArg : typeArgs) paramNames
      DataType name | name == typeName -> return $ Just recursor where
        recursor = App recursorWithTypeArgs scrutinee
        scrutinee = foldl App ctorArg $ map lVar $ reverse paramNames
        recursorWithTypeArgs = foldl App partialRecursor typeArgs
      _ -> return Nothing

-- Build arguments to pass to a recursor case during reduction
argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName partialRecursor ctorArgs ctorType = go ctorType ctorArgs [] where
  go ctorType ctorArgs prevCtorArgs = do
    ctorTypeNF <- whnf ctorType
    case (ctorTypeNF, ctorArgs) of
      (Pi Explicit paramType bind, ctorArg : restCtorArgs) -> do
        (paramName, returnType) <- Unbound.unbind bind
        restArgs <- go returnType restCtorArgs $ (paramName, ctorArg) : prevCtorArgs
        hypothesisForParam typeName partialRecursor paramType prevCtorArgs ctorArg <&> \case
          Nothing -> ctorArg : restArgs
          Just hypothesis -> ctorArg : hypothesis : restArgs
      (Pi Implicit paramType bind, args) -> do
        (paramName, returnType) <- Unbound.unbind bind
        mvar <- newMVar paramType
        (mvar :) <$> go returnType args ((paramName, mvar) : prevCtorArgs)
      (_, args) -> assert (null args) $ return []

-- Attempt to reduce a recursor application to a case
reduceRecursor :: DataTypeName -> [Term] -> MaybeT TcMonad Term
reduceRecursor _ [] = empty
reduceRecursor typeName (motive:args) =
  traceM "reduceRecursor" (typeName : ppr motive : map ppr args) ppr $ do
    (typeSignature, ctors) <- lookUpDataType typeName
    (params, _) <- unfoldPi typeSignature
    let numTypeArgs = length params
    (cases, (scrutinee, extraArgs)) <- splitReducerArgs (length ctors) numTypeArgs args
    (Ctor scrutineeTypeName ctorName, ctorArgs) <- unfoldApps <$> lift (whnf scrutinee)
    guard $ scrutineeTypeName == typeName
    let partialRecursor = foldl App (Rec typeName) (motive : cases)
    let ((_, ctorType), case') = fromJust $ find ((ctorName ==) . fst . fst) (zip ctors cases)
    caseArgs <- lift $ argsForCase typeName partialRecursor ctorArgs ctorType
    return $ foldl App case' $ caseArgs ++ extraArgs

-- Verify that a constructor returns the data type being defined
checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (DataType typeName) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ =
  err [DS "Constructor", DD ctorName, DS "must return", DD self]

-- Error if the data type name appears in the given type
throwIfFound :: DataTypeName -> Type -> TcMonad ()
throwIfFound typeName type' = whnf type' >>= \case
  Lam _ binder -> do
    (_, body) <- Unbound.unbind binder
    throwIfFound typeName body
  App a b -> mapM_ (throwIfFound typeName) [a, b]
  Pi _ paramType binder -> do
    throwIfFound typeName paramType
    (_, returnType) <- Unbound.unbind binder
    throwIfFound typeName returnType
  DataType name -> when (name == typeName) $
    err [DS "Invalid recursive occurrence of data type", DD typeName, DS "during declaration"]
  _ -> return ()

-- Check that the data type occurs only strictly positively in a type
checkStrictPositivity :: DataTypeName -> Type -> TcMonad ()
checkStrictPositivity self paramType = whnf paramType >>= \case
  App a b -> do
    throwIfFound self b
    checkStrictPositivity self a
  Pi _ paramType bind -> do
    throwIfFound self paramType
    (_, returnType) <- Unbound.unbind bind
    checkStrictPositivity self returnType
  _ -> return ()

-- Type-check a data type declaration and its constructors
checkDataTypeDecl :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad ()
checkDataTypeDecl typeName typeSignature ctors = do
  _ <- ensureType typeSignature
  (_, returnType) <- unfoldPi typeSignature
  whnf returnType >>= \case
    Sort _ -> return ()
    _ -> err [DS "Expected", DD typeName, DS "signature to return a Sort, but got", DD returnType]
  forM_ ctors $ \(ctorName, ctorType) -> do
    let addSelfToEnv env = env { dataTypeBeingDeclared = Just (typeName, typeSignature) }
    _ <- local addSelfToEnv $ ensureType ctorType
    (paramTypes, returnType) <- unfoldPi ctorType
    checkCtorReturnsSelf ctorName typeName =<< Equal.whnf returnType
    mapM (checkStrictPositivity typeName) paramTypes
