
-- | The abstract syntax of the simple dependently typed language
-- See the comment at the top of 'Parser' for the concrete syntax of this language
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
  | -- | function type   `(x : A) -> B`
    TyPi Type (Unbound.Bind TName Type)
  | -- | annotated terms `( a : A )`
    Ann Term Type
  | -- | an axiom 'TRUSTME', inhabits all types
    TrustMe
  | -- | type constructors (fully applied)
    TyCon TyConName
  | -- | term constructors (fully applied)
    DataCon TyConName DataConName
  deriving (Show, Generic)

-- | A 'Match' represents a case alternative
newtype Match = Match (Unbound.Bind Pattern Term)
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data Pattern = PatVar TName | PatCon DataConName [Pattern]
  deriving (Show, Eq, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

type Param = (TName, Type)

-- | Entries are the components of modules
data Entry = Decl TName Type Term | Data TyConName [(TName, Type)] [CtorDef]
  deriving (Show, Generic, Typeable) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- | type constructor names
type TyConName = String

-- | data constructor names
type DataConName = String

-- | A Data constructor has a name and a telescope of arguments
data CtorDef = CtorDef DataConName [(TName, Type)] Type
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- ** Telescopes

-- | A telescope is like a first class context. It is a list of 
-- assumptions, binding each variable in terms that appear
-- later in the list.
-- type Telescope = [TypeDecl]

-----------------------------------------
-- Definitions related to datatypes

-- | Is this the syntax of a literal (natural) number
isNumeral :: Term -> Maybe Int
isNumeral (DataCon "Nat" "zero") = Just 0
isNumeral (App (DataCon "Nat" "succ") predTerm) | Just pred <- isNumeral predTerm = Just (pred + 1)
isNumeral _ = Nothing

-- | Remove source positions and type annotations from the top level of a term
strip :: Term -> Term
strip (Ann tm _) = strip tm
strip tm = tm

-- | in binders `x.a1` and `x.a2` replace `x` with a fresh name in both terms
-- TODO: test this
unbind2 :: (Unbound.Fresh m) => Unbound.Bind TName Term -> Unbound.Bind TName Term -> m (TName, Term, Term)
unbind2 b1 b2 = Unbound.unbind2 b1 b2 >>= \case 
  Just (x, t, _, u) -> return (x, t, u)
  Nothing -> error "impossible" 

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
