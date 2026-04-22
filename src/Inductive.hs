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
import Data.List (nub, elemIndex)
import Data.Maybe (fromMaybe)
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

addParams :: [(BinderInfo, TermName, Type)] -> Type -> Type
addParams params returnType = foldr addParam returnType params

-- Derive the motive type from a data type's signature
motiveTypeFromTypeSignature :: TermName -> DataTypeName -> [Level] -> Type -> [TermName] -> Type -> TcMonad Type
motiveTypeFromTypeSignature hole typeName levels motiveReturnType = go [] where
  go scrutineeTypeArgs paramNames = whnf >=> \case
    Pi _ _ binder | (paramName : rest) <- paramNames -> do
      let returnType = Unbound.instantiate binder [LVar paramName]
      go (paramName : scrutineeTypeArgs) rest returnType
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (binderInfo, paramName, paramType) <$> go (paramName : scrutineeTypeArgs) [] returnType
    _ -> do
      let dataType = Const (DataType typeName) levels
      let fullyApplied = foldl App dataType $ map LVar $ reverse scrutineeTypeArgs
      return $ addParam (Explicit, hole, fullyApplied) motiveReturnType

-- Generate induction hypothesis type from a constructor parameter
hypothesisTypeFromCtorParam :: Int -> TermName -> DataTypeName -> TermName -> Type -> MaybeT TcMonad Type
hypothesisTypeFromCtorParam numParams motive self name = go [] [] where
  go paramNames args = lift . whnf >=> \case
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (binderInfo, paramName, paramType) <$> go (paramName : paramNames) [] returnType
    App func arg -> go paramNames (arg : args) func
    Const (DataType typeName) _ | typeName == self ->
      return $ App motiveApplied paramApplied where
      motiveApplied = foldl App (LVar motive) $ drop numParams args
      paramApplied = foldl App (LVar name) $ map LVar (reverse paramNames)
    _ -> empty

-- Compute the return type of a recursor case from constructor return type
caseReturnTypeFromCtorReturnType :: Int -> TermName -> Term -> [Term] -> Type -> TcMonad Type
caseReturnTypeFromCtorReturnType numParams motive ctor args = whnf >=> \case
  App func arg -> caseReturnTypeFromCtorReturnType numParams motive ctor (arg : args) func
  _ -> return $ App (foldl App (LVar motive) $ drop numParams args) ctor

-- Build the type of a recursor case from a constructor definition
recursorCaseTypeFromCtor :: TermName -> TermName -> DataTypeName -> [TermName] -> Term -> Type -> TcMonad Type
recursorCaseTypeFromCtor motive hole typeName paramNames = go paramNames where
  go paramNames ctor = whnf >=> \case
    Pi _ _ binder | (paramName : rest) <- paramNames -> do
      let returnType = Unbound.instantiate binder [LVar paramName]
      go rest (App ctor $ LVar paramName) returnType
    Pi binderInfo paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      rest <- go [] (App ctor $ LVar paramName) returnType
      runMaybeT (hypothesisTypeFromCtorParam numParams motive typeName paramName paramType) <&> \case
        Nothing -> addParam (binderInfo, paramName, paramType) rest
        Just hypothesisType ->
          addParam (binderInfo, paramName, paramType) $ addParam (Explicit, hole, hypothesisType) rest
    returnType -> caseReturnTypeFromCtorReturnType numParams motive ctor [] returnType
  numParams = length paramNames

recursorReturnType :: TermName -> TermName -> DataTypeName -> [Level] -> [TermName] -> Type -> TcMonad Type
recursorReturnType motive scrutinee typeName levels paramNames = go [] paramNames where
  go typeArgs paramNames = whnf >=> \case
    Pi _ _ binder | (paramName : rest) <- paramNames -> do
      let returnType = Unbound.instantiate binder [LVar paramName]
      go (paramName : typeArgs) rest returnType
    Pi _ paramType binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      addParam (Implicit, paramName, paramType) <$> go (paramName : typeArgs) [] returnType
    _ -> return $ addParam (Explicit, scrutinee, selfApplied) motiveApplied where
      motiveApplied = App (foldl App (LVar motive) $ map LVar $ drop numParams $ reverse typeArgs) (LVar scrutinee)
      selfApplied = foldl App dataType $ map LVar $ reverse typeArgs
      dataType = Const (DataType typeName) levels
  numParams = length paramNames

-- Synthesize the full type of a recursor for a data type
synthesizeRecursorType :: DataTypeName -> [UnivParamName] -> Type -> Int -> [(CtorName, Type)] -> Type -> TcMonad Type
synthesizeRecursorType typeName univParams signature numParams ctors motiveReturnType =
  let args = [typeName, ppr univParams, ppr signature, show numParams, ppr ctors, ppr motiveReturnType]
  in traceM "synthesizeRecursorType" args ppr $ do
    -- Use fresh names to avoid capturing user variables like `motive`/`x`.
    motive <- Unbound.fresh $ Unbound.string2Name "motive"
    scrutinee <- Unbound.fresh $ Unbound.string2Name "x"
    hole <- Unbound.fresh $ Unbound.string2Name "_"

    params <- take numParams . fst <$> unfoldPi signature
    let paramNames = [paramName | (_, paramName, _) <- params]
    let levels = map Param univParams
    motiveType <- motiveTypeFromTypeSignature hole typeName levels motiveReturnType paramNames signature

    cases <- forM ctors $ \(ctorName, ctorType) ->
      let ctor = Const (Ctor typeName ctorName) levels
      in recursorCaseTypeFromCtor motive hole typeName paramNames ctor ctorType

    let addMotive = addParam (Explicit, motive, motiveType)
    returnType <- recursorReturnType motive scrutinee typeName levels paramNames signature
    let implicitParams = [(Implicit, paramName, paramType) | (_, paramName, paramType) <- params]
    return $ addParams implicitParams $ addMotive $ addParams [(Explicit, hole, case') | case' <- cases] returnType

-- Unfold a nested Pi type into parameter types and return type
unfoldPi :: Type -> TcMonad ([(BinderInfo, TermName, Type)], Type)
unfoldPi = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    first ((binderInfo, paramName, paramType) :) <$> unfoldPi returnType
  returnType -> return ([], returnType)

-- Generate induction hypothesis term for a recursive parameter
hypothesisForParam :: Int -> DataTypeName -> Term -> Type -> Term -> TcMonad (Maybe Term)
hypothesisForParam numParams typeName recursorApplied paramType ctorArg = go paramType [] [] where
  go paramType typeArgs paramNames = whnf paramType >>= \case
    Pi binderInfo _ binder -> do
      (paramName, returnType) <- Unbound.unbind binder
      fmap (Lam binderInfo . Unbound.bind paramName) <$> go returnType typeArgs (paramName : paramNames)
    App typeCtor typeArg -> go typeCtor (typeArg : typeArgs) paramNames
    Const (DataType name) _ | name == typeName -> return $ Just recursor where
      recursor = App recursorWithTypeArgs scrutinee
      scrutinee = foldl App ctorArg $ map LVar $ reverse paramNames
      recursorWithTypeArgs = foldl App recursorApplied $ drop numParams typeArgs
    _ -> return Nothing

-- Build arguments to pass to a recursor case during reduction
argsForCase :: DataTypeName -> Term -> [Term] -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName recursorApplied paramArgs = go paramArgs where
  numParams = length paramArgs
  go paramArgs ctorArgs ctorType = do
    ctorType <- whnf ctorType
    case (ctorType, ctorArgs) of
      (Pi _ _ binder, _ : restCtorArgs) | (paramArg : rest) <- paramArgs ->
        go rest restCtorArgs (Unbound.instantiate binder [paramArg])
      (Pi _ paramType binder, ctorArg : restCtorArgs) -> do
        restArgs <- go [] restCtorArgs (Unbound.instantiate binder [ctorArg])
        hypothesisForParam numParams typeName recursorApplied paramType ctorArg <&> \case
          Nothing -> ctorArg : restArgs
          Just hypothesis -> ctorArg : hypothesis : restArgs
      (_, args) -> assert (null args) $ return []

-- Attempt to reduce a recursor application to a case
reduceRecursor :: DataTypeName -> Term -> [Term] -> MaybeT TcMonad Term
reduceRecursor typeName recursor args =
  traceM "reduceRecursor" (typeName : map ppr args) ppr $ do
    DataTypeInfo _ signature numParams ctors _ _ <- lookUpDataTypeInfo typeName

    (params, indices) <- splitAt numParams . fst <$> lift (unfoldPi signature)
    let numParams = length params
    let (paramArgs, rest1) = splitAt numParams args
    (motive, rest2) <- case rest1 of
      [] -> empty
      (motive : rest2) -> return (motive, rest2)
    let (cases, rest3) = splitAt (length ctors) rest2
    let (_, rest4) = splitAt (length indices) rest3
    (scrutinee, extraArgs) <- case rest4 of
      [] -> empty
      (scrutinee : extraArgs) -> return (scrutinee, extraArgs)

    (Const (Ctor scrutineeTypeName ctorName) _, ctorArgs) <- unfoldApps <$> lift (whnf scrutinee)
    guard $ scrutineeTypeName == typeName
    let recursorApplied = foldl App recursor $ paramArgs ++ [motive] ++ cases
    let Just ((_, ctorType), case') = find ((ctorName ==) . fst . fst) (zip ctors cases)
    caseArgs <- lift $ argsForCase typeName recursorApplied paramArgs ctorArgs ctorType
    return $ foldl App case' $ caseArgs ++ extraArgs

unfoldCommonParams :: Type -> Type -> TcMonad ([(TermName, Type)], (Type, Type))
unfoldCommonParams type1 type2 = (,) <$> whnf type1 <*> whnf type2 >>= \case
  (Pi binderInfo1 paramType1 binder1, Pi binderInfo2 paramType2 binder2) -> do
    (paramName, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus binder1 binder2
    whetherMatches <- (binderInfo1 == binderInfo2 &&) <$> isDefEq paramType1 paramType2
    if whetherMatches
      then addLocal paramName paramType1 $
        first ((paramName, paramType1) :) <$> unfoldCommonParams returnType1 returnType2
      else return ([], (type1, type2))
  _ -> return ([], (type1, type2))

numParamsInCtor :: Type -> Type -> TcMonad Int
numParamsInCtor signature ctorType = do
  (commonParams, (_, remainingCtorType)) <- unfoldCommonParams signature ctorType
  (_, ctorReturnType) <- unfoldPi remainingCtorType
  (_, args) <- unfoldApps <$> whnf ctorReturnType
  areParamsAppliedDirectly <- forM (zip commonParams args) $ \((paramName, _), arg) -> isDefEq (LVar paramName) arg
  return $ fromMaybe (length commonParams) $ elemIndex False areParamsAppliedDirectly

inferNumParams :: Type -> [Type] -> TcMonad Int
inferNumParams signature ctorTypes =
  traceM "inferNumParams" [ppr signature, ppr ctorTypes] show $ if null ctorTypes
    then length . fst <$> unfoldPi signature
    else minimum <$> mapM (numParamsInCtor signature) ctorTypes

-- Verify that a constructor returns the data type being defined
checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (Const (DataType typeName) _) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ = throwError [i|Constructor #{ctorName} must return #{self}|]

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
  Const (DataType name) _ ->
    when (name == typeName) $ throwError [i|Invalid recursive occurrence of data type #{typeName} during declaration|]
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
checkDataTypeDecl typeName signature ctors addSelfToEnv =
  traceM "checkDataTypeDecl" [typeName, ppr signature, ppr ctors] (const "") $ do
    _ <- ensureType signature
    (_, returnType) <- unfoldPi signature
    whnf returnType >>= \case
      Sort _ -> return ()
      _ -> err [DS "Expected", DD typeName, DS "signature to return a Sort, but got", DD returnType]

    unless (length ctors == length (nub $ map fst ctors)) $
      throwError [i|Duplicate constructor(s) found in #{typeName} definition|]
    forM_ ctors $ \(ctorName, ctorType) -> do
      _ <- local addSelfToEnv $ ensureType ctorType
      (params, returnType) <- unfoldPi ctorType
      checkCtorReturnsSelf ctorName typeName =<< whnf returnType
      mapM (checkStrictPositivity typeName) [paramType | (_, _, paramType) <- params]
