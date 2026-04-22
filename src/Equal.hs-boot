module Equal where

import Syntax (Term)
import Environment (TC, TcMonad)

isDefEq :: Term -> Term -> TcMonad Bool
unify :: Term -> Term -> TcMonad ()
whnf :: Term -> TcMonad Term
