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

-- | basic language
data Term
  = -- | type of types, concretely `Type`
    TyType
  | -- | variable `x`
    Var TermName
  | -- | abstraction  `\x. a`
    Lam (Unbound.Bind TermName Term)
  | -- | application `a b`
    App Term Term
  | -- | function type `(x : A) -> B`
    Pi Type (Unbound.Bind TermName Type)
  | -- | annotated terms `( a : A )`
    Ann Term Type
  | -- | an axiom 'TRUSTME', inhabits all types
    TrustMe
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
data Entry = Decl TermName Type Term | Data DataTypeName Type [(CtorName, Type)]
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
  isvar (Var x) = Just (Unbound.SubstName x)
  isvar _ = Nothing
