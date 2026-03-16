module Inductive where

import Control.Applicative (empty)
import Control.Exception (assert)
import Control.Monad ((>=>), forM, forM_, guard, when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment
import {-# SOURCE #-} Equal
import PrettyPrint
import Syntax
import {-# SOURCE #-} TypeCheck

-- Prepend a Pi-bound parameter to a type
addParam :: (BinderInfo, TermName, Type) -> Type -> Type
addParam (binderInfo, paramName, paramType) = Pi binderInfo paramType . Unbound.bind paramName

-- Derive the motive type from a data type's signature
motiveTypeFromTypeSignature :: TermName -> DataTypeName -> [Level] -> Type -> Type -> TcMonad Type
motiveTypeFromTypeSignature hole typeName levels motiveReturnType = go [] where
  go paramNames = whnf >=> \case
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (binderInfo, paramName, paramType) <$> go (paramName : paramNames) returnType
    _ -> do
      let dataType = Const (DataType typeName) levels
      let fullyApplied = foldl App dataType $ map LVar $ reverse paramNames
      return $ addParam (Explicit, hole, fullyApplied) motiveReturnType

-- Generate induction hypothesis type from a constructor parameter
hypothesisTypeFromCtorParam :: TermName -> DataTypeName -> TermName -> Type -> MaybeT TcMonad Type
hypothesisTypeFromCtorParam motive self name = go [] [] where
  go paramNames args = lift . whnf >=> \case
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (binderInfo, paramName, paramType) <$> go (paramName : paramNames) [] returnType
    App func arg -> go paramNames (arg : args) func
    Const (DataType typeName) _ | typeName == self ->
      return $ App motiveApplied paramApplied where
      motiveApplied = foldl App (LVar motive) args
      paramApplied = foldl App (LVar name) $ map LVar (reverse paramNames)
    _ -> empty

-- Compute the return type of a recursor case from constructor return type
caseReturnTypeFromCtorReturnType :: TermName -> Term -> [Term] -> Type -> TcMonad Type
caseReturnTypeFromCtorReturnType motive ctor args = whnf >=> \case
  App func arg -> caseReturnTypeFromCtorReturnType motive ctor (arg : args) func
  _ -> return $ App (foldl App (LVar motive) args) ctor

-- Build the type of a recursor case from a constructor definition
recursorCaseFromCtor ::
  TermName -> TermName -> CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor motive hole ctorName typeName ctor = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    let ctorApplied = if binderInfo == Explicit then App ctor (LVar paramName) else ctor
    rest <- recursorCaseFromCtor motive hole ctorName typeName ctorApplied returnType
    runMaybeT (hypothesisTypeFromCtorParam motive typeName paramName paramType) <&> \case
      Nothing -> addParam (binderInfo, paramName, paramType) rest
      Just hypothesisType ->
        addParam (binderInfo, paramName, paramType) $ addParam (Explicit, hole, hypothesisType) rest
  returnType -> caseReturnTypeFromCtorReturnType motive ctor [] returnType

recursorReturnType :: TermName -> TermName -> DataTypeName -> [Level] -> Type -> TcMonad Type
recursorReturnType motive scrutinee typeName levels = go [] where
  go paramNames = whnf >=> \case
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (binderInfo, paramName, paramType) <$> go (paramName : paramNames) returnType
    _ -> return $ addParam (Explicit, scrutinee, selfApplied) motiveApplied where
      motiveApplied = App (foldl App (LVar motive) $ map LVar $ reverse paramNames) (LVar scrutinee)
      selfApplied = foldl App dataType $ map LVar $ reverse paramNames
      dataType = Const (DataType typeName) levels

-- Synthesize the full type of a recursor for a data type
synthesizeRecursorType ::
  DataTypeName -> [UnivParamName] -> Type -> [(CtorName, Type)] -> Type -> TcMonad Type
synthesizeRecursorType typeName univParams signature ctors motiveReturnType = do
  -- Use fresh names to avoid capturing user variables like `motive`/`x`.
  motive <- Unbound.fresh $ Unbound.string2Name "motive"
  scrutinee <- Unbound.fresh $ Unbound.string2Name "x"
  hole <- Unbound.fresh $ Unbound.string2Name "_"

  let levels = map Param univParams
  motiveType <- motiveTypeFromTypeSignature hole typeName levels motiveReturnType signature

  cases <- forM ctors $ \(ctorName, ctorType) ->
    let ctor = Const (Ctor typeName ctorName) levels
    in recursorCaseFromCtor motive hole ctorName typeName ctor ctorType

  let addMotive = addParam (Explicit, motive, motiveType)
  returnType <- recursorReturnType motive scrutinee typeName levels signature
  return $ addMotive $ foldr (addParam . (Explicit, hole,)) returnType cases

-- Unfold a nested Pi type into parameter types and return type
unfoldPi :: Type -> TcMonad ([Type], Type)
unfoldPi = whnf >=> \case
  Pi _ paramType binder -> do
    (_, returnType) <- Unbound.unbind binder
    first (paramType :) <$> unfoldPi returnType
  returnType -> return ([], returnType)

-- Split recursor arguments into cases, scrutinee, and extra args
splitRecursorArgs :: Int -> Int -> [Term] -> MaybeT TcMonad ([Term], (Term, [Term]))
splitRecursorArgs 0 0 (scrutinee : rest) = return ([], (scrutinee, rest))
splitRecursorArgs 0 m (_ : rest) = splitRecursorArgs 0 (m - 1) rest
splitRecursorArgs n m (arg : rest) = first (arg :) <$> splitRecursorArgs (n - 1) m rest
splitRecursorArgs _ _ _ = empty

-- Generate induction hypothesis term for a recursive parameter
hypothesisForParam ::
  DataTypeName -> Term -> Type -> [(TermName, Term)] -> Term -> TcMonad (Maybe Term)
hypothesisForParam typeName recursorApplied paramType prevCtorArgs ctorArg = go paramType [] []
  where
    go paramType typeArgs paramNames = whnf paramType >>= \case
      Pi binderInfo _ binder -> do
        (paramName, returnType) <- Unbound.unbind binder
        fmap (Lam binderInfo . Unbound.bind paramName) <$>
          go returnType typeArgs (paramName : paramNames)
      App typeCtor typeArg ->
        go typeCtor (Unbound.substs prevCtorArgs typeArg : typeArgs) paramNames
      Const (DataType name) _ | name == typeName -> return $ Just recursor where
        recursor = App recursorWithTypeArgs scrutinee
        scrutinee = foldl App ctorArg $ map LVar $ reverse paramNames
        recursorWithTypeArgs = foldl App recursorApplied typeArgs
      _ -> return Nothing

-- Build arguments to pass to a recursor case during reduction
argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName recursorApplied ctorArgs ctorType = go ctorType ctorArgs [] where
  go ctorType ctorArgs prevCtorArgs = do
    ctorType <- whnf ctorType
    case (ctorType, ctorArgs) of
      (Pi _ paramType binder, ctorArg : restCtorArgs) -> do
        (paramName, returnType) <- Unbound.unbind binder
        restArgs <- go returnType restCtorArgs $ (paramName, ctorArg) : prevCtorArgs
        hypothesisForParam typeName recursorApplied paramType prevCtorArgs ctorArg <&> \case
          Nothing -> ctorArg : restArgs
          Just hypothesis -> ctorArg : hypothesis : restArgs
      (_, args) -> assert (null args) $ return []

-- Attempt to reduce a recursor application to a case
reduceRecursor :: DataTypeName -> Term -> [Term] -> MaybeT TcMonad Term
reduceRecursor _ _ [] = empty
reduceRecursor typeName recursor (motive:args) =
  traceM "reduceRecursor" (typeName : ppr motive : map ppr args) ppr $ do
    DataTypeInfo _ signature ctors _ _ <- lookUpDataTypeInfo typeName
    (params, _) <- lift $ unfoldPi signature
    let numTypeArgs = length params
    (cases, (scrutinee, extraArgs)) <- splitRecursorArgs (length ctors) numTypeArgs args
    (Const (Ctor scrutineeTypeName ctorName) _, ctorArgs) <- unfoldApps <$> lift (whnf scrutinee)
    guard $ scrutineeTypeName == typeName
    let recursorApplied = foldl App recursor $ motive : cases
    let ((_, ctorType), case') = fromJust $ find ((ctorName ==) . fst . fst) (zip ctors cases)
    caseArgs <- lift $ argsForCase typeName recursorApplied ctorArgs ctorType
    return $ foldl App case' $ caseArgs ++ extraArgs

-- Verify that a constructor returns the data type being defined
checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (Const (DataType typeName) _) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ =
  throwError [i|Constructor #{ctorName} must return #{self}|]

-- Error if the data type name appears in the given type
throwIfFound :: DataTypeName -> Type -> TcMonad ()
throwIfFound typeName type' = whnf type' >>= \case
  Lam _ binder -> do
    (_, body) <- Unbound.unbind binder
    throwIfFound typeName body
  App func arg -> mapM_ (throwIfFound typeName) [func, arg]
  Pi _ paramType binder -> do
    throwIfFound typeName paramType
    (_, returnType) <- Unbound.unbind binder
    throwIfFound typeName returnType
  Const (DataType name) _ -> when (name == typeName) $
    throwError [i|Invalid recursive occurrence of data type #{typeName} during declaration|]
  _ -> return ()

-- Check that the data type occurs only strictly positively in a type
checkStrictPositivity :: DataTypeName -> Type -> TcMonad ()
checkStrictPositivity self paramType = whnf paramType >>= \case
  App func arg -> do
    throwIfFound self arg
    checkStrictPositivity self func
  Pi _ paramType binder -> do
    throwIfFound self paramType
    (_, returnType) <- Unbound.unbind binder
    checkStrictPositivity self returnType
  _ -> return ()

-- Type-check a data type declaration and its constructors
checkDataTypeDecl :: DataTypeName -> Type -> [(CtorName, Type)] -> (Env -> Env) -> TcMonad ()
checkDataTypeDecl typeName signature ctors addSelfToEnv = do
  _ <- ensureType signature
  (_, returnType) <- unfoldPi signature
  whnf returnType >>= \case
    Sort _ -> return ()
    _ -> err [DS "Expected", DD typeName, DS "signature to return a Sort, but got", DD returnType]

  unless (length ctors == length (nub $ map fst ctors)) $
    throwError [i|Duplicate constructor(s) found in #{typeName} definition|]
  forM_ ctors $ \(ctorName, ctorType) -> do
    _ <- local addSelfToEnv $ ensureType ctorType
    (paramTypes, returnType) <- unfoldPi ctorType
    checkCtorReturnsSelf ctorName typeName =<< whnf returnType
    mapM (checkStrictPositivity typeName) paramTypes
