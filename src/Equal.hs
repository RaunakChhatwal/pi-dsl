{- pi-forall language -}

-- | Compare two terms for equality
module Equal (whnf, equate, unify) where

import Syntax
import Environment (TcMonad)
import PrettyPrint (D(DS, DD))
import qualified Environment as Env
import qualified Unbound.Generics.LocallyNameless as Unbound

import Control.Monad.Except (catchError)
import Control.Monad (unless, zipWithM, zipWithM_)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map

-- | compare two expressions for equality
-- first check if they are alpha equivalent then
-- if not, weak-head normalize and compare
-- throw an error if they cannot be matched up
equate :: Term -> Term -> TcMonad ()
equate t1 t2 | Unbound.aeq t1 t2 = return () 
equate t1 t2 = do
  n1 <- whnf t1  
  n2 <- whnf t2
  case (n1, n2) of 
    (TyType, TyType) -> return ()
    (Var x,  Var y) | x == y -> return ()
    (Lam bnd1, Lam bnd2) -> do
      (_, b1, b2) <- unbind2 bnd1 bnd2
      equate b1 b2
    (App a1 a2, App b1 b2) ->
      equate a1 b1 >> equate a2 b2
    (TyPi tyA1 bnd1, TyPi tyA2 bnd2) -> do 
      (_, tyB1, tyB2) <- unbind2 bnd1 bnd2
      equate tyA1 tyA2                                             
      equate tyB1 tyB2

    (TrustMe, TrustMe) ->  return ()
    (TyCon c1, TyCon c2) | c1 == c2 -> return ()
    (DataCon type1 ctor1, DataCon type2 ctor2) | type1 == type2 && ctor1 == ctor2 -> return ()   
    (_,_) -> Env.err [DS "Expected", DD n2,  DS "but found", DD n1]

-------------------------------------------------------
-- | Convert a term to its weak-head normal form.
whnf :: Term -> TcMonad Term  
whnf (Var var) = Env.lookupDecl var >>= \case
  (Just (_, def)) -> whnf def
  _ -> return (Var var)
        
whnf (App t1 t2) = do
  nf <- whnf t1 
  case nf of 
    (Lam bnd) -> do
      whnf (Unbound.instantiate bnd [t2])
    _ -> do
      return (App nf t2)

-- ignore/remove type annotations when normalizing  
whnf (Ann tm _) = whnf tm  
-- all other terms are already in WHNF, don't do anything special for them
whnf tm = return tm

-- | 'Unify' the two terms, producing a list of definitions that 
-- must hold for the terms to be equal
-- If the terms are already equal, succeed with an empty list
-- If there is an obvious mismatch, fail with an error
-- If either term is "ambiguous" (i.e. neutral), give up and 
-- succeed with an empty list
unify :: [TName] -> Term -> Term -> TcMonad [(TName, Term)]
unify ns tx ty = do
  txnf <- whnf tx
  tynf <- whnf ty
  if Unbound.aeq txnf tynf
    then return []
    else case (txnf, tynf) of
      (Var x, Var y) | x == y -> return []
      (Var y, yty) | y `notElem` ns -> return [(y, yty)]
      (yty, Var y) | y `notElem` ns -> return [(y, yty)]
      (TyCon s1, TyCon s2) | s1 == s2 -> return []
      (DataCon type1 ctor1, DataCon type2 ctor2) | type1 == type2 && ctor1 == ctor2 -> return []
      (Lam bnd1, Lam bnd2) -> do
        (x, b1, b2) <- unbind2 bnd1 bnd2
        unify (x:ns) b1 b2
      (TyPi tyA1 bnd1, TyPi tyA2 bnd2) -> do
        (x, tyB1, tyB2) <- unbind2 bnd1 bnd2
        ds1 <- unify ns tyA1 tyA2
        ds2 <- unify (x:ns) tyB1 tyB2
        return (ds1 ++ ds2)
      (App _ _, _) -> return []
      (_, App _ _) -> return []
      _ -> Env.err [DS "Cannot equate", DD txnf, DS "and", DD tynf]
