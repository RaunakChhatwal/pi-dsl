module Equal (whnf, unify) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Environment (assignMVar, err, lookUpDecl, lookUpMVarSolution, TcMonad, traceM)
import Inductive (reduceRecursor)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (Term(..), unfoldApps, Var(Global, Meta))

mvarOccursCheck :: Int -> Term -> TcMonad Bool
mvarOccursCheck id term = case term of
  Var (Meta id2) -> return $ id == id2
  App func arg -> (||) <$> mvarOccursCheck id func <*> mvarOccursCheck id arg
  Lam _ binder -> do
    (_, body) <- Unbound.unbind binder
    mvarOccursCheck id body
  Pi _ paramType binder -> do
    (_, returnType) <- Unbound.unbind binder
    (||) <$> mvarOccursCheck id paramType <*> mvarOccursCheck id returnType
  Ann inner hint -> (||) <$> mvarOccursCheck id inner <*> mvarOccursCheck id hint
  _ -> return False

solveMVar :: Int -> Term -> TcMonad ()
solveMVar id term = lookUpMVarSolution id >>= \case
  Just soln -> unify soln term
  Nothing -> case term of
    Var (Meta id2) | id == id2 -> return ()
    _ -> mvarOccursCheck id term >>= \case
      True -> err [DS $ "Occurs check failed for ?" ++ show id ++ " in", DD term]
      False -> assignMVar id term

unify :: Term -> Term -> TcMonad ()
unify term1 term2 = traceM "unify" [ppr term1, ppr term2] (const "") $ do
  nf1 <- whnf term1
  nf2 <- whnf term2
  case (nf1, nf2) of
    (Var (Meta id), term) -> solveMVar id term
    (term, Var (Meta id)) -> solveMVar id term

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

    (nf1, nf2) | Unbound.aeq nf1 nf2 -> return ()

    (nf1, nf2) -> err [DS "Expected", DD nf2, DS "but found", DD nf1]

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf term = traceM "whnf" [ppr term] ppr $ case term of
  Var (Global var) -> lookUpDecl var >>= \case
    Just (_, def) -> whnf def
    Nothing -> err [DS "Global variable", DD var, DS "not found"]

  Var (Meta id) -> lookUpMVarSolution id >>= \case
    Just soln -> whnf soln
    Nothing -> return term

  term@(App _ _) -> do
    let (func, args) = unfoldApps term
    funcNF <- whnf func
    case funcNF of
      Lam _ binder ->
        whnf $ foldl App (Unbound.instantiate binder [arg]) rest where (arg : rest) = args
      Rec typeName -> runMaybeT (reduceRecursor typeName args) >>= \case
        Nothing -> return $ foldl App funcNF args
        Just reduced -> whnf reduced
      _ | Unbound.aeq func funcNF -> return term
      _ -> whnf $ foldl App funcNF args

  Ann term _ -> whnf term

  term -> return term
