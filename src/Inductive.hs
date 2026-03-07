module Inductive where

import Control.Applicative (Alternative(empty))
import Control.Exception (assert)
import Control.Monad ((>=>), forM, forM_, guard, when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
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
motiveTypeFromTypeSignature :: TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveTypeFromTypeSignature hole typeName paramNames = whnf >=> \case
  Pi binderInfo paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    addParam (binderInfo, paramName, paramType) <$>
      motiveTypeFromTypeSignature hole typeName (paramName : paramNames) returnType
  returnType -> do
    let fullyApplied = foldl App (Const $ DataType typeName) $ map LVar $ reverse paramNames
    case returnType of
      Sort level -> return $ addParam (Explicit, hole, fullyApplied) $ Sort level
      _ -> err [DS "Expected data type signature to return a Sort, but got", DD returnType]

-- Generate induction hypothesis type from a constructor parameter
hypothesisTypeFromCtorParam ::
  TermName -> DataTypeName -> [TermName] -> [Term] -> TermName -> Type -> TcMonad (Maybe Type)
hypothesisTypeFromCtorParam motive self paramNames args name = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    fmap (addParam (binderInfo, paramName, paramType)) <$>
      hypothesisTypeFromCtorParam motive self (paramName : paramNames) [] name returnType
  App func arg -> hypothesisTypeFromCtorParam motive self paramNames (arg : args) name func
  Const (DataType typeName) | typeName == self ->
    return $ Just $ App motiveApplied paramApplied where
    motiveApplied = foldl App (LVar motive) args
    paramApplied = foldl App (LVar name) $ map LVar (reverse paramNames)
  _ -> return Nothing

-- Compute the return type of a recursor case from constructor return type
motiveFromCtorReturnType :: TermName -> Term -> [Term] -> Type -> TcMonad Type
motiveFromCtorReturnType motive ctor args = whnf >=> \case
  App func arg -> motiveFromCtorReturnType motive ctor (arg : args) func
  _ -> return $ App (foldl App (LVar motive) args) ctor

-- Build the type of a recursor case from a constructor definition
recursorCaseFromCtor ::
  TermName -> TermName -> CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor motive hole ctorName typeName ctor = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    let ctorApplied = if binderInfo == Explicit then App ctor (LVar paramName) else ctor
    rest <- recursorCaseFromCtor motive hole ctorName typeName ctorApplied returnType
    hypothesisTypeFromCtorParam motive typeName [] [] paramName paramType <&> \case
      Nothing -> addParam (binderInfo, paramName, paramType) rest
      Just hypothesisType ->
        addParam (binderInfo, paramName, paramType) $
          addParam (Explicit, hole, hypothesisType) rest
  returnType -> motiveFromCtorReturnType motive ctor [] returnType

recursorReturnType :: TermName -> TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
recursorReturnType motive selfName typeName paramNames = whnf >=> \case
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    addParam (binderInfo, paramName, paramType) <$>
      recursorReturnType motive selfName typeName (paramName : paramNames) returnType
  _ -> return $ addParam (Explicit, selfName, selfApplied) motiveApplied where
    motiveApplied = App (foldl App (LVar motive) $ map LVar $ reverse paramNames) (LVar selfName)
    selfApplied = foldl App (Const $ DataType typeName) $ map LVar $ reverse paramNames

-- Synthesize the full type of a recursor for a data type
synthesizeRecursorType :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad Type
synthesizeRecursorType typeName signature ctors = do
  -- Use fresh names to avoid capturing user variables like `motive`/`self`.
  motive <- Unbound.fresh $ Unbound.string2Name "motive"
  selfName <- Unbound.fresh $ Unbound.string2Name "self"
  hole <- Unbound.fresh $ Unbound.string2Name "_"

  motiveType <- motiveTypeFromTypeSignature hole typeName [] signature
  cases <- forM ctors $ \(ctorName, ctorType) ->
    recursorCaseFromCtor motive hole ctorName typeName (Const $ Ctor typeName ctorName) ctorType

  let addMotive = addParam (Explicit, motive, motiveType)
  returnType <- recursorReturnType motive selfName typeName [] signature
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
hypothesisForParam typeName partialRecursor paramType prevCtorArgs ctorArg = go paramType [] []
  where
    go paramType typeArgs paramNames = whnf paramType >>= \case
      Pi binderInfo _ binder -> do
        (paramName, returnType) <- Unbound.unbind binder
        fmap (Lam binderInfo . Unbound.bind paramName) <$>
          go returnType typeArgs (paramName : paramNames)
      App typeCtor typeArg ->
        go typeCtor (Unbound.substs prevCtorArgs typeArg : typeArgs) paramNames
      Const (DataType name) | name == typeName -> return $ Just recursor where
        recursor = App recursorWithTypeArgs scrutinee
        scrutinee = foldl App ctorArg $ map LVar $ reverse paramNames
        recursorWithTypeArgs = foldl App partialRecursor typeArgs
      _ -> return Nothing

-- Build arguments to pass to a recursor case during reduction
argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName partialRecursor ctorArgs ctorType = go ctorType ctorArgs [] where
  go ctorType ctorArgs prevCtorArgs = do
    ctorTypeNF <- whnf ctorType
    case (ctorTypeNF, ctorArgs) of
      (Pi _ paramType bind, ctorArg : restCtorArgs) -> do
        (paramName, returnType) <- Unbound.unbind bind
        restArgs <- go returnType restCtorArgs $ (paramName, ctorArg) : prevCtorArgs
        hypothesisForParam typeName partialRecursor paramType prevCtorArgs ctorArg <&> \case
          Nothing -> ctorArg : restArgs
          Just hypothesis -> ctorArg : hypothesis : restArgs
      (_, args) -> assert (null args) $ return []

-- Attempt to reduce a recursor application to a case
reduceRecursor :: DataTypeName -> [Term] -> MaybeT TcMonad Term
reduceRecursor _ [] = empty
reduceRecursor typeName (motive:args) =
  traceM "reduceRecursor" (typeName : ppr motive : map ppr args) ppr $ do
    dataTypeInfo <- lookUpDataType typeName
    let typeSignature = dataTypeInfo.signature
    let ctors = dataTypeInfo.ctors
    (params, _) <- lift $ unfoldPi typeSignature
    let numTypeArgs = length params
    (cases, (scrutinee, extraArgs)) <- splitRecursorArgs (length ctors) numTypeArgs args
    (Const (Ctor scrutineeTypeName ctorName), ctorArgs) <- unfoldApps <$> lift (whnf scrutinee)
    guard $ scrutineeTypeName == typeName
    let partialRecursor = foldl App (Const $ Rec typeName) (motive : cases)
    let ((_, ctorType), case') = fromJust $ find ((ctorName ==) . fst . fst) (zip ctors cases)
    caseArgs <- lift $ argsForCase typeName partialRecursor ctorArgs ctorType
    return $ foldl App case' $ caseArgs ++ extraArgs

-- Verify that a constructor returns the data type being defined
checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (Const (DataType typeName)) | typeName == self = return ()
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
  Const (DataType name) -> when (name == typeName) $
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
checkDataTypeDecl :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad ()
checkDataTypeDecl typeName typeSignature ctors = do
  _ <- ensureType typeSignature
  (_, returnType) <- unfoldPi typeSignature
  whnf returnType >>= \case
    Sort _ -> return ()
    _ -> err [DS "Expected", DD typeName, DS "signature to return a Sort, but got", DD returnType]

  unless (length ctors == length (nub $ map fst ctors)) $
    throwError [i|Duplicate constructor(s) found in #{typeName} definition|]
  forM_ ctors $ \(ctorName, ctorType) -> do
    let addSelfToEnv env = env { dataTypeBeingDeclared = Just (typeName, typeSignature) }
    _ <- local addSelfToEnv $ ensureType ctorType
    (paramTypes, returnType) <- unfoldPi ctorType
    checkCtorReturnsSelf ctorName typeName =<< whnf returnType
    mapM (checkStrictPositivity typeName) paramTypes
