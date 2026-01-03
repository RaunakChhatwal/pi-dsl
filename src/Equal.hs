module Equal (whnf, equate) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Environment (err, lookUpDecl, TcMonad, traceM)
import Inductive (reduceRecursor)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (Term(..), unfoldApps, Var(Global))

-- compare two expressions for equality, only accepts well typed arguments
equate :: Term -> Term -> TcMonad ()
equate term1 term2 = traceM "equate" [ppr term1, ppr term2] (const "") $
  liftA2 (,) (whnf term1) (whnf term2) >>= \case
    (Lam bind1, Lam bind2) -> do
      (_, body1, _, body2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate body1 body2
    (App func1 arg1, App func2 arg2) ->
      equate func1 func2 >> equate arg1 arg2
    (Pi paramType1 bind1, Pi paramType2 bind2) -> do
      equate paramType1 paramType2
      (_, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate returnType1 returnType2
    (nf1, nf2) | Unbound.aeq nf1 nf2 -> return ()
    (nf1, nf2) -> err [DS "Expected", DD nf2,  DS "but found", DD nf1]

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf term = traceM "whnf" [ppr term] ppr $ case term of
  Var (Global var) -> lookUpDecl var >>= \case
    Just (_, def) -> whnf def
    Nothing -> err [DS "Global variable", DD var, DS "not found"]

  term@(App _ _) -> do
    let (func, args) = unfoldApps term
    funcNF <- whnf func
    case funcNF of
      Lam bind -> case args of
        [] -> error "Unreachable"
        (arg : rest) -> whnf $ foldl App (Unbound.instantiate bind [arg]) rest
      Rec typeName -> runMaybeT (reduceRecursor typeName args) >>= \case
        Nothing -> return $ foldl App funcNF args
        Just reduced -> whnf reduced
      _ | Unbound.aeq func funcNF -> return term
      _ -> whnf $ foldl App funcNF args

  Ann term _ -> whnf term

  term -> return term
