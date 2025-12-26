module TypeCheck where

import Syntax (Term, Type)
import Environment (TcMonad)

checkType :: Term -> Type -> TcMonad ()