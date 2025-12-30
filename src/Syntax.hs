module Syntax where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.Generics (Generic, from)
import Unbound.Generics.LocallyNameless qualified as Unbound

import Data.Function (on)

type TermName = Unbound.Name Term

-- | Because types and terms use the same AST, we define the following
-- type synonym so that we can hint whether a piece of syntax is being used
-- as a type or as a term.
type Type = Term

data Level = Zero | Succ Level
  deriving Generic deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

maxLevel :: Level -> Level -> Level
maxLevel Zero b = b
maxLevel a Zero = a
maxLevel (Succ a) (Succ b) = Succ $ maxLevel a b

levelToInt :: Level -> Int
levelToInt Zero = 0
levelToInt (Succ level) = 1 + levelToInt level

instance Show Level where
  show = show . levelToInt

data Var = Local TermName | Global String
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

lVar :: TermName -> Term
lVar = Var . Local

-- instance Unbound.Subst Term Var where
--   isvar (Local name) = Just $ Unbound.SubstName name
--   isvar _ = Nothing

-- | basic language
data Term
  = Sort Level
  | -- | variable `x`
    Var Var
  | -- | abstraction  `\x. a`
    Lam (Unbound.Bind TermName Term)
  | -- | application `a b`
    App Term Term
  | -- | function type `(x : A) -> B`
    Pi Type (Unbound.Bind TermName Type)
  | -- | annotated terms `( a : A )`
    Ann Term Type
  | -- | type constructors (fully applied)
    DataType DataTypeName
  | -- | term constructors (fully applied)
    Ctor DataTypeName CtorName
  | Rec DataTypeName -- recursors
  deriving (Show, Generic)

-- | A 'Match' represents a case alternative
newtype Match = Match (Unbound.Bind Pattern Term)
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data Pattern = PatVar TermName | PatCon DataTypeName [Pattern]
  deriving (Show, Eq, Generic, Unbound.Alpha, Unbound.Subst Term)

type DataTypeName = String
type CtorName = String
data Entry = Decl String Type Term | Data DataTypeName Type [(CtorName, Type)]
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- * `Alpha` class instances

-- The Unbound library's `Alpha` class enables the `aeq`, `fv`,
-- `instantiate` and `unbind` functions, and also allows some
-- specialization of their generic behavior.

-- For `Term`, we'd like Alpha equivalence to ignore
-- source positions and type annotations. So we make sure to
-- remove them before calling the generic operation.

instance Unbound.Alpha Term where
  aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
  aeq' ctx a b = (Unbound.gaeq ctx `on` from) (strip a) (strip b) where
    strip (Ann term _) = strip term
    strip term = term

---------------

-- * Substitution

-- The Subst class derives capture-avoiding substitution.
-- It has two parameters because the type of thing we are substituting
-- for may not be the same as what we are substituting into.

-- The `isvar` function identifies the variables in the term that
-- should be substituted for.
instance Unbound.Subst Term Term where
  isvar (Var (Local name)) = Just (Unbound.SubstName name)
  isvar _ = Nothing
