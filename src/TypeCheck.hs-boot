module TypeCheck where

import Syntax (Level, Term, Type)
import Environment (TcMonad)

checkType :: Term -> Type -> TcMonad ()
ensureType :: Term -> TcMonad Level
