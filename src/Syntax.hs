module Syntax where

import Data.Bifunctor (second)
import GHC.Generics (Generic, from)
import Unbound.Generics.LocallyNameless qualified as Unbound

import Data.Function (on)

-- Term name alias used by Unbound
type TermName = Unbound.Name Term

-- Because types and terms use the same AST, we define the following
-- type synonym so that we can hint whether a piece of syntax is being used
-- as a type or as a term.
type Type = Term

-- Universe level
data Level = Zero | Succ Level
  deriving Generic deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- Maximum of two universe levels
maxLevel :: Level -> Level -> Level
maxLevel Zero b = b
maxLevel a Zero = a
maxLevel (Succ a) (Succ b) = Succ $ maxLevel a b

-- Convert a Level to an Int
levelToInt :: Level -> Int
levelToInt Zero = 0
levelToInt (Succ level) = 1 + levelToInt level

-- Show instance for Level via Int conversion
instance Show Level where
  show = show . levelToInt

-- Variables (local binders or global names)
data Var = Local TermName | Global String | Meta Int
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- Build a local variable term
lVar :: TermName -> Term
lVar = Var . Local

data BinderInfo = Explicit | Implicit
  deriving (Show, Eq, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- pi-dsl abstract syntax tree
data Term
  = Sort Level
  | Var Var
  | Lam BinderInfo (Unbound.Bind TermName Term)
  | App Term Term
  | Pi BinderInfo Type (Unbound.Bind TermName Type)
  | Ann Term Type
  | DataType DataTypeName
  | Ctor DataTypeName CtorName
  | Rec DataTypeName -- recursors
  deriving (Show, Generic)

-- Data type names are represented as strings
type DataTypeName = String
-- Constructor names are represented as strings
type CtorName = String
-- Top-level environment entries (declarations and data types)
data Entry = Decl String Type Term | Data DataTypeName Type [(CtorName, Type)]
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- Unfold nested applications into (head, args)
unfoldApps :: Term -> (Term, [Term])
unfoldApps term = second reverse (go term) where
  go (App func arg) = second (arg :) (go func)
  go term = (term, [])

-- Alpha-equivalence instance for Term (ignores annotations)
instance Unbound.Alpha Term where
  aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
  aeq' ctx a b = (Unbound.gaeq ctx `on` from) (strip a) (strip b) where
    strip (Ann term _) = strip term
    strip term = term

-- Substitution instance for Term
instance Unbound.Subst Term Term where
  isvar (Var (Local name)) = Just (Unbound.SubstName name)
  isvar _ = Nothing
