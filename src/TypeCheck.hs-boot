module TypeCheck where

import Syntax (Level, Term, Type)
import Environment (TcMonad)

checkType :: Term -> Type -> TcMonad ()
elaborateAgainst :: Term -> Type -> TcMonad Term
ensureType :: Term -> TcMonad Level
