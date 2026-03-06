module Equal where

import Syntax (Term)
import Environment (TcMonad)

unify :: Term -> Term -> TcMonad ()
whnf :: Term -> TcMonad Term
