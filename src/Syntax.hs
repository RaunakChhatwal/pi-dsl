module Syntax where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, from)
import Unbound.Generics.LocallyNameless qualified as Unbound

import Data.Function (on)

type TName = Unbound.Name Term

-- | Because types and terms use the same AST, we define the following
-- type synonym so that we can hint whether a piece of syntax is being used
-- as a type or as a term.
type Type = Term

-- | basic language
data Term
  = -- | type of types, concretely `Type`
    TyType
  | -- | variable `x`
    Var TName
  | -- | abstraction  `\x. a`
    Lam (Unbound.Bind TName Term)
  | -- | application `a b`
    App Term Term
  | -- | function type `(x : A) -> B`
    Pi Type (Unbound.Bind TName Type)
  | -- | annotated terms `( a : A )`
    Ann Term Type
  | -- | an axiom 'TRUSTME', inhabits all types
    TrustMe
  | -- | type constructors (fully applied)
    TyCon TypeName
  | -- | term constructors (fully applied)
    DataCon TypeName CtorName
  deriving (Show, Generic)

-- | A 'Match' represents a case alternative
newtype Match = Match (Unbound.Bind Pattern Term)
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data Pattern = PatVar TName | PatCon TypeName [Pattern]
  deriving (Show, Eq, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

-- | Entries are the components of modules
data Entry = Decl TName Type Term | Data TypeName Type [(CtorName, Type)]
  deriving (Show, Generic, Typeable) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

type TypeName = String
type CtorName = String

-- | Is this the syntax of a literal (natural) number
isNumeral :: Term -> Maybe Int
isNumeral (DataCon "Nat" "zero") = Just 0
isNumeral (App (DataCon "Nat" "succ") predTerm) | Just pred <- isNumeral predTerm = Just (pred + 1)
isNumeral _ = Nothing

-- | Remove source positions and type annotations from the top level of a term
strip :: Term -> Term
strip (Ann tm _) = strip tm
strip tm = tm

-- * `Alpha` class instances

-- The Unbound library's `Alpha` class enables the `aeq`, `fv`,
-- `instantiate` and `unbind` functions, and also allows some 
-- specialization of their generic behavior.

-- For `Term`, we'd like Alpha equivalence to ignore 
-- source positions and type annotations. So we make sure to 
-- remove them before calling the generic operation.

instance Unbound.Alpha Term where
  aeq' :: Unbound.AlphaCtx -> Term -> Term -> Bool
  aeq' ctx a b = (Unbound.gaeq ctx `on` from) (strip a) (strip b)

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
