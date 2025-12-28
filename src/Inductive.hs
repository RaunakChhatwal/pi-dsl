{-# HLINT ignore "Redundant <$>" #-}
module Inductive
  (checkDataTypeDecl, reduceRecursor, synthesizeRecursorType, unfoldApps, unfoldPi) where

import Control.Exception (assert)
import Control.Monad ((>=>), forM, when, forM_)
import Data.Bifunctor (second, first)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment (addDataType, err, lookupDataType, TcMonad, traceM)
import {-# SOURCE #-} Equal (whnf)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (CtorName, DataTypeName, Term(..), TermName, Type)
import {-# SOURCE #-} TypeCheck (checkType, ensureType)

hole :: TermName
hole = Unbound.string2Name "_"

addParam :: (TermName, Type) -> Type -> Type
addParam (paramName, paramType) = Pi paramType . Unbound.bind paramName

motiveFromTypeSignature :: DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromTypeSignature typeName paramNames (Pi paramType bind) = do
  (paramName, returnType) <- Unbound.unbind bind
  addParam (paramName, paramType) <$>
    motiveFromTypeSignature typeName (paramName : paramNames) returnType
motiveFromTypeSignature typeName paramNames returnType = do
  let fullyApplied = foldl App (DataType typeName) $ map Var $ reverse paramNames
  whnf returnType >>= \case
    Sort u -> return $ addParam (hole, fullyApplied) $ Sort u
    _ -> err [DS "Expected data type signature to end in a Sort, but got", DD returnType]

motive :: TermName
motive = Unbound.string2Name "motive"

motiveFromCtorParam ::
  DataTypeName -> [TermName] -> [Term] -> TermName -> Type -> TcMonad (Maybe Type)
motiveFromCtorParam self paramNames args name = whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (addParam (paramName, paramType)) <$>
      motiveFromCtorParam self (paramName : paramNames) [] name returnType
  App f arg -> motiveFromCtorParam self paramNames (arg : args) name f
  DataType typeName | typeName == self ->
    return $ Just $ App motiveApplied paramApplied where
    motiveApplied = foldl App (Var motive) args
    paramApplied = foldl App (Var name) $ map Var (reverse paramNames)
  _ -> return Nothing

motiveFromCtorReturnType :: Term -> [Term] -> Type -> TcMonad Type
motiveFromCtorReturnType ctorApplied args = whnf >=> \case
  App f arg -> motiveFromCtorReturnType ctorApplied (arg : args) f
  _ -> return $ App (foldl App (Var motive) args) ctorApplied

recursorCaseFromCtor :: CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor ctorName typeName ctorApplied = whnf >=> \case
  (Pi paramType bind) -> do
    (paramName, returnType) <- Unbound.unbind bind
    rest <- recursorCaseFromCtor ctorName typeName (App ctorApplied $ Var paramName) returnType
    motiveFromCtorParam typeName [] [] paramName paramType <&> \case
      Nothing -> addParam (paramName, paramType) rest
      Just motive -> addParam (paramName, paramType) $ addParam (hole, motive) rest
  returnType -> motiveFromCtorReturnType ctorApplied [] returnType

motiveFromSelf :: DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromSelf typeName paramNames (Pi paramType bind) = do -- TODO: whnf?
  (paramName, returnType) <- Unbound.unbind bind
  addParam (paramName, paramType) <$>
    motiveFromSelf typeName (paramName : paramNames) returnType
motiveFromSelf typeName paramNames _ = do
  let self = Unbound.string2Name "self"
  let selfApplied = foldl App (DataType typeName) $ map Var $ reverse paramNames
  let motiveApplied = App (foldl App (Var motive) $ map Var $ reverse paramNames) (Var self)
  return $ addParam (self, selfApplied) motiveApplied

synthesizeRecursorType :: DataTypeName -> TcMonad Type
synthesizeRecursorType typeName = do
  (signature, ctorDefs) <- lookupDataType typeName
  motiveType <- motiveFromTypeSignature typeName [] signature
  cases <- forM ctorDefs $ \(ctorName, ctorType) ->
    recursorCaseFromCtor ctorName typeName (Ctor typeName ctorName) ctorType
  addParam (motive, motiveType) <$>
    (foldr (addParam . (hole,)) <$> motiveFromSelf typeName [] signature <*> pure cases)

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
    return $ Just $ App recursor $ foldl App ctorArg $ map Var $ reverse paramNames
  _ -> return Nothing

argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName recursor ctorArgs ctorType = (, ctorArgs) <$> whnf ctorType >>= \case
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
  (typeSignature, ctors) <- lookupDataType typeName
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
  Sort _ -> return ()
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
  DataType name -> when (name == typeName) $
    err [DS "Invalid recursive occurrence of data type", DD typeName, DS "during declaration"]
  Ctor _ _ -> return ()
  Rec name -> when (name == typeName) $
    err [DS "Recursor of", DD typeName, DS "not valid during its declaration"]

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
    let message = "Unreachable: attempted access to data type definition during declaration"
    _ <- addDataType typeName typeSignature (error message) $ ensureType ctorType
    (paramTypes, returnType) <- unfoldPi ctorType
    checkCtorReturnsSelf ctorName typeName =<< Equal.whnf returnType
    mapM (checkStrictPositivity typeName) paramTypes
