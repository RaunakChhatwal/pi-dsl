{-# HLINT ignore "Redundant <$>" #-}
module Equal (whnf, equate, unfoldPi) where

import Syntax
import Environment (TcMonad)
import PrettyPrint (D(DS, DD), ppr)
import qualified Environment as Env
import qualified Unbound.Generics.LocallyNameless as Unbound

import Control.Monad.Except (catchError)
import Control.Monad (unless, zipWithM, zipWithM_, guard, (>=>))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Bifunctor (second, first)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative (empty)
import Data.List (find, intercalate)
import Data.Functor ((<&>))
import Control.Exception (assert)

-- compare two expressions for equality, only accepts well typed arguments
equate :: Term -> Term -> TcMonad ()
equate term1 term2 = Env.traceM "equate" [ppr term1, ppr term2] (const "") $
  liftA2 (,) (whnf term1) (whnf term2) >>= \case
    (Lam bind1, Lam bind2) -> do
      (var, body1, _, body2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate body1 body2
    (Lam bind1, nf2) -> do
      (var, body) <- lift $ Unbound.unbind bind1
      equate body (App nf2 (Var var))
    (nf1, Lam bind2) -> do
      (var, body) <- lift $ Unbound.unbind bind2
      equate (App nf1 (Var var)) body
    (App func1 arg1, App func2 arg2) ->
      equate func1 func2 >> equate arg1 arg2
    (Pi paramType1 bind1, Pi paramType2 bind2) -> do
      equate paramType1 paramType2
      (paramName, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate returnType1 returnType2
    (nf1, nf2) | Unbound.aeq nf1 nf2 -> return ()
    (nf1, nf2) -> Env.err [DS "Expected", DD nf2,  DS "but found", DD nf1]

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
hypothesisForParam typeName paramNames recursor ctorArg = Equal.whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (Lam  . Unbound.bind paramName) <$>
      hypothesisForParam typeName (paramName : paramNames) recursor ctorArg returnType
  App typeCtor _ -> hypothesisForParam typeName paramNames recursor ctorArg typeCtor
  DataType name | name == typeName ->
    return $ Just $ App recursor $ foldl App ctorArg $ map Var $ reverse paramNames
  _ -> return Nothing

argsForCase :: DataTypeName -> Term -> [Term] -> Type -> TcMonad [Term]
argsForCase typeName recursor ctorArgs ctorType = (, ctorArgs) <$> Equal.whnf ctorType >>= \case
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
  Env.traceM "reduceRecursor" (typeName : ppr motive : map ppr args) ppr $ do
  (typeSignature, ctors) <- Env.lookupDataType typeName
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

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf (Var var) = Env.lookupDecl var >>= \case
  (Just (_, def)) -> whnf def
  _ -> return (Var var)

-- TODO: optimize this, fix redundant traversals
whnf term@(App _ _) = do
  (func, args) <- unfoldApps term
  funcNF <- whnf func
  case funcNF of
    Lam bind -> whnf $ foldl App (Unbound.instantiate bind [head args]) (tail args)
    Rec typeName -> reduceRecursor typeName args >>= \case
      Nothing -> return $ foldl App funcNF args
      Just reduced -> whnf reduced
    _ | Unbound.aeq func funcNF -> return term
    _ -> whnf $ foldl App funcNF args

-- ignore/remove type annotations when normalizing  
whnf (Ann term _) = whnf term  
-- all other terms are already in WHNF, don't do anything special for them
whnf term = return term
