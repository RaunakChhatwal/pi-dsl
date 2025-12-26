module Equal (whnf, equate) where

import Control.Monad.Trans (lift)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Environment (err, lookupDecl, TcMonad, traceM)
import Inductive (reduceRecursor, unfoldApps)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (Term(..))

-- compare two expressions for equality, only accepts well typed arguments
equate :: Term -> Term -> TcMonad ()
equate term1 term2 = traceM "equate" [ppr term1, ppr term2] (const "") $
  liftA2 (,) (whnf term1) (whnf term2) >>= \case
    (Lam bind1, Lam bind2) -> do
      (var, body1, _, body2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate body1 body2
    (Lam bind1, nf2) -> do
      (var, body) <- Unbound.unbind bind1
      equate body (App nf2 (Var var))
    (nf1, Lam bind2) -> do
      (var, body) <- Unbound.unbind bind2
      equate (App nf1 (Var var)) body
    (App func1 arg1, App func2 arg2) ->
      equate func1 func2 >> equate arg1 arg2
    (Pi paramType1 bind1, Pi paramType2 bind2) -> do
      equate paramType1 paramType2
      (paramName, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus bind1 bind2
      equate returnType1 returnType2
    (nf1, nf2) | Unbound.aeq nf1 nf2 -> return ()
    (nf1, nf2) -> err [DS "Expected", DD nf2,  DS "but found", DD nf1]

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf (Var var) = lookupDecl var >>= \case
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
