module Equal where

import Syntax (Term)
import Environment (TcMonad)

whnf :: Term -> TcMonad Term