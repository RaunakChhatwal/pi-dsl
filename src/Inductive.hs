module Inductive
  (checkDataTypeDecl, reduceRecursor, synthesizeRecursorType, unfoldApps, unfoldPi) where

import Control.Exception (assert)
import Control.Monad ((>=>), forM, when, forM_)
import Control.Monad.Reader (local)
import Data.Bifunctor (second, first)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment (addDataType, err, lookUpDataType, TcMonad, traceM, Env(dataTypeBeingDeclared))
import {-# SOURCE #-} Equal (whnf)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (CtorName, DataTypeName, Term(..), TermName, Type, lVar)
import {-# SOURCE #-} TypeCheck (checkType, ensureType)


freshName :: String -> TcMonad TermName
freshName s = Unbound.fresh (Unbound.string2Name s)

addParam :: (TermName, Type) -> Type -> Type
addParam (paramName, paramType) = Pi paramType . Unbound.bind paramName

motiveFromTypeSignature :: TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromTypeSignature hole typeName paramNames = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    addParam (paramName, paramType) <$>
      motiveFromTypeSignature hole typeName (paramName : paramNames) returnType
  returnType -> do
    let fullyApplied = foldl App (DataType typeName) $ map lVar $ reverse paramNames
    case returnType of
      Sort u -> return $ addParam (hole, fullyApplied) $ Sort u
      _ -> err [DS "Expected data type signature to return a Sort, but got", DD returnType]

motiveFromCtorParam ::
  TermName -> DataTypeName -> [TermName] -> [Term] -> TermName -> Type -> TcMonad (Maybe Type)
motiveFromCtorParam motive self paramNames args name = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (addParam (paramName, paramType)) <$>
      motiveFromCtorParam motive self (paramName : paramNames) [] name returnType
  App f arg -> motiveFromCtorParam motive self paramNames (arg : args) name f
  DataType typeName | typeName == self ->
    return $ Just $ App motiveApplied paramApplied where
    motiveApplied = foldl App (lVar motive) args
    paramApplied = foldl App (lVar name) $ map lVar (reverse paramNames)
  _ -> return Nothing

motiveFromCtorReturnType :: TermName -> Term -> [Term] -> Type -> TcMonad Type
motiveFromCtorReturnType motive ctor args = whnf >=> \case
  App f arg -> motiveFromCtorReturnType motive ctor (arg : args) f
  _ -> return $ App (foldl App (lVar motive) args) ctor

recursorCaseFromCtor
  :: TermName -> TermName -> CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor motive hole ctorName typeName ctor = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    let ctorApplied = App ctor $ lVar paramName
    rest <- recursorCaseFromCtor motive hole ctorName typeName ctorApplied returnType
    motiveFromCtorParam motive typeName [] [] paramName paramType <&> \case
      Nothing -> addParam (paramName, paramType) rest
      Just hypothesis -> addParam (paramName, paramType) $ addParam (hole, hypothesis) rest
  returnType -> motiveFromCtorReturnType motive ctor [] returnType

motiveFromSelf :: TermName -> TermName -> DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromSelf motive selfName typeName paramNames = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    addParam (paramName, paramType) <$>
      motiveFromSelf motive selfName typeName (paramName : paramNames) returnType
  _ -> do
    let selfApplied = foldl App (DataType typeName) $ map lVar $ reverse paramNames
    let motiveApplied =
          App (foldl App (lVar motive) $ map lVar $ reverse paramNames) (lVar selfName)
    return $ addParam (selfName, selfApplied) motiveApplied

synthesizeRecursorType :: DataTypeName -> TcMonad Type
synthesizeRecursorType typeName = do
  (signature, ctorDefs) <- lookUpDataType typeName

  -- Use fresh binder names to avoid capturing user variables like `motive`/`self`.
  motive <- freshName "motive"
  selfName <- freshName "self"
  hole <- freshName "_"

  motiveType <- motiveFromTypeSignature hole typeName [] signature
  cases <- forM ctorDefs $ \(ctorName, ctorType) ->
    recursorCaseFromCtor motive hole ctorName typeName (Ctor typeName ctorName) ctorType

  motiveParam <- motiveFromSelf motive selfName typeName [] signature
  return $ addParam (motive, motiveType) $ foldr (addParam . (hole,)) motiveParam cases

unfoldApps :: Term -> TcMonad (Term, [Term])
unfoldApps term = second reverse <$> go term where
  go (App func arg) = second (arg :) <$> go func
  go term = return (term, [])

unfoldPi :: Type -> TcMonad ([Type], Type)
unfoldPi (Pi paramType bind) = do
  (_, returnType) <- Unbound.unbind bind
  first (paramType :) <$> unfoldPi returnType
unfoldPi returnType = return ([], returnType)

splitReducerArgs :: Int -> Int -> [Term] -> TcMonad (Maybe ([Term], ([Term], (Term, [Term]))))
splitReducerArgs 0 0 (scrutinee : rest) = return $ Just ([], ([], (scrutinee, rest)))
splitReducerArgs 0 m (arg : rest) = fmap (second $ first (arg:)) <$> splitReducerArgs 0 (m - 1) rest
splitReducerArgs n m (arg : rest) = fmap (first (arg :)) <$> splitReducerArgs (n - 1) m rest
splitReducerArgs _ _ _ = return Nothing

hypothesisForParam :: DataTypeName -> [TermName] -> Term -> Term -> Type -> TcMonad (Maybe Term)
hypothesisForParam typeName paramNames recursor ctorArg = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (Lam  . Unbound.bind paramName) <$>
      hypothesisForParam typeName (paramName : paramNames) recursor ctorArg returnType
  App typeCtor _ -> hypothesisForParam typeName paramNames recursor ctorArg typeCtor
  DataType name | name == typeName ->
    return $ Just $ App recursor $ foldl App ctorArg $ map lVar $ reverse paramNames
  _ -> return Nothing

argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName recursor ctorArgs ctorType = do
  ctorTypeNF <- whnf ctorType
  case (ctorTypeNF, ctorArgs) of
    (Pi paramType bind, ctorArg:restCtorArgs) -> do
      (_, returnType) <- Unbound.unbind bind
      restArgs <- argsForCase typeName recursor restCtorArgs returnType
      hypothesisForParam typeName [] recursor ctorArg paramType <&> \case
        Nothing -> ctorArg : restArgs
        Just hypothesis -> ctorArg : hypothesis : restArgs
    (_, args) -> assert (null args) $ return []

reduceRecursor :: DataTypeName -> [Term] -> TcMonad (Maybe Term)
reduceRecursor _ [] = return Nothing
reduceRecursor typeName (motive:args) =
  traceM "reduceRecursor" (typeName : ppr motive : map ppr args) ppr $ do
    (typeSignature, ctors) <- lookUpDataType typeName
    numTypeParams <- length . fst <$> unfoldPi typeSignature
    splitReducerArgs (length ctors) numTypeParams args >>= \case
      Nothing -> return Nothing
      Just (cases, (typeArgs, (scrutinee, extraArgs))) -> whnf scrutinee >>= unfoldApps >>= \case
        (Ctor scrutineeTypeName ctorName, ctorArgs) | scrutineeTypeName == typeName -> do
          let recursor = foldl App (Rec typeName) $ motive : cases ++ typeArgs
          let ((_, ctorType), case') = fromJust $ find ((ctorName ==) . fst . fst) (zip ctors cases)
          caseArgs <- argsForCase typeName recursor ctorArgs ctorType
          return $ Just $ foldl App case' $ caseArgs ++ extraArgs
        _ -> return Nothing

checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (DataType typeName) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ =
  err [DS "Constructor", DD ctorName, DS "must return", DD self]

throwIfFound :: DataTypeName -> Type -> TcMonad ()
throwIfFound typeName type' = whnf type' >>= \case
  Lam bind -> do
    (_, body) <- Unbound.unbind bind
    throwIfFound typeName body
  App a b -> mapM_ (throwIfFound typeName) [a, b]
  Pi paramType bind -> do
    throwIfFound typeName paramType
    (_, returnType) <- Unbound.unbind bind
    throwIfFound typeName returnType
  DataType name -> when (name == typeName) $
    err [DS "Invalid recursive occurrence of data type", DD typeName, DS "during declaration"]
  _ -> return ()

checkStrictPositivity :: DataTypeName -> Type -> TcMonad ()
checkStrictPositivity self paramType = whnf paramType >>= \case
  App a b -> do
    throwIfFound self b
    checkStrictPositivity self a
  Pi paramType bind -> do
    throwIfFound self paramType
    (_, returnType) <- Unbound.unbind bind
    checkStrictPositivity self returnType
  _ -> return ()

checkDataTypeDecl :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad ()
checkDataTypeDecl typeName typeSignature ctors = do
  _ <- ensureType typeSignature
  _ <- ensureType . snd =<< unfoldPi typeSignature
  forM_ ctors $ \(ctorName, ctorType) -> do
    let addSelfToEnv env = env { dataTypeBeingDeclared = Just (typeName, typeSignature) }
    _ <- local addSelfToEnv $ ensureType ctorType
    (paramTypes, returnType) <- unfoldPi ctorType
    checkCtorReturnsSelf ctorName typeName =<< Equal.whnf returnType
    mapM (checkStrictPositivity typeName) paramTypes
