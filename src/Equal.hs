module Equal (whnf, unify) where

import Control.Monad (zipWithM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Foldable (foldrM)
import Data.List (nub)
import Data.String.Interpolate (i)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment
import Inductive
import PrettyPrint
import Syntax

millerPatternCheck :: Term -> TcMonad (Maybe (Int, [TermName]))
millerPatternCheck term = case unfoldApps term of
  (MVar id, args) -> return $ do
    let prependIfLocal (LVar name) rest = return $ name : rest
        prependIfLocal _ _ = Nothing
    localArgs <- foldrM prependIfLocal [] args
    unless (length localArgs == length (nub localArgs)) Nothing
    return (id, localArgs)
  _ -> return Nothing

-- Solve a metavariable application ?m x1 ... xn
solveMVar :: Int -> [TermName] -> Term -> TcMonad ()
solveMVar id args term = lookUpMVarSolution id >>= \case
  Just soln -> unify (foldl App soln $ map LVar args) term
  Nothing -> assignMVar id $ foldr (\arg body -> Lam Explicit $ Unbound.bind arg body) term args

unify :: Term -> Term -> TcMonad ()
unify term1 term2 = traceM "unify" [ppr term1, ppr term2] (const "") $ do
  term1 <- whnf term1
  term2 <- whnf term2
  (,) <$> millerPatternCheck term1 <*> millerPatternCheck term2 >>= \case
    (Just (id1, args1), Just (id2, args2)) | id1 == id2 && length args1 == length args2 ->
      zipWithM_ unify (map LVar args1) (map LVar args2)
    (Just (id, args), _) -> solveMVar id args term2
    (_, Just (id, args)) -> solveMVar id args term1

    _ -> case (term1, term2) of
      (Lam binderInfo1 binder1, Lam binderInfo2 binder2) | binderInfo1 == binderInfo2 -> do
        (_, body1, _, body2) <- lift $ Unbound.unbind2Plus binder1 binder2
        unify body1 body2

      (App func1 arg1, App func2 arg2) ->
        unify func1 func2 >> unify arg1 arg2

      (Pi binderInfo1 paramType1 binder1, Pi binderInfo2 paramType2 binder2)
        | binderInfo1 == binderInfo2 -> do
          unify paramType1 paramType2
          (_, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus binder1 binder2
          unify returnType1 returnType2

      _ | Unbound.aeq term1 term2 -> return ()

      _ -> err [DS "Expected", DD term2, DS "but found", DD term1]

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf term = traceM "whnf" [ppr term] ppr $ case term of
  Const (GVar var) -> lookUpDecl var >>= \case
    Just (_, def) -> whnf def
    Nothing -> throwError [i|Global variable #{var} not found|]

  MVar id -> lookUpMVarSolution id >>= \case
    Just soln -> whnf soln
    Nothing -> return term

  term@(App _ _) -> do
    let (func, args) = unfoldApps term
    funcNF <- whnf func
    case funcNF of
      Lam _ binder ->
        whnf $ foldl App (Unbound.instantiate binder [arg]) rest where (arg : rest) = args
      Const (Rec typeName) -> runMaybeT (reduceRecursor typeName args) >>= \case
        Nothing -> return $ foldl App funcNF args
        Just reduced -> whnf reduced
      _ | Unbound.aeq func funcNF -> return term
      _ -> whnf $ foldl App funcNF args

  Ann term _ -> whnf term

  term -> return term
