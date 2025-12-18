
-- | The abstract syntax of the simple dependently typed language
-- See the comment at the top of 'Parser' for the concrete syntax of this language
module Syntax where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic,from)
import GHC.Base (MonadPlus)
-- import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos, newPos)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound

import Data.Function (on)

-----------------------------------------

-- * Names

-----------------------------------------

-- | For variable names, we use the `Unbound` library to
-- automatically generate free variable, substitution,
-- and alpha-equality function. The abstract type `Name` from 
-- this library is indexed by the AST type that this variable 
-- is a name for. 

type TName = Unbound.Name Term

-----------------------------------------

-- * Core language of pi-forall (Combined syntax for types and terms)

-----------------------------------------

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
    DataCon DataConName
  deriving (Show, Generic)

-- | A 'Match' represents a case alternative
newtype Match = Match (Unbound.Bind Pattern Term)
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- data PatCtorArg = RelArg Pattern | IrrVar TName
--   deriving (Show, Eq, Generic, Unbound.Alpha, Unbound.Subst Term)

data Pattern = PatVar TName | PatCon DataConName [Pattern]
  deriving (Show, Eq, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

-- | A type declaration 
data TypeDecl = TypeDecl {declName :: TName , declType :: Type}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

-- -- | Declare the type of a term
-- mkDecl :: TName -> Type -> Entry
-- mkDecl n ty = Decl (TypeDecl n ty)

-- | Entries are the components of modules
data Entry
  = -- | Declaration for the type of a term  'x : A'
    Decl TypeDecl
  | -- | The definition of a particular name 'x = a'
    -- must already have a type declaration in scope
    Def TName Term
  | -- | Datatype definition (must be at the module level)
    Data TyConName Telescope [CtorDef]
  
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-----------------------------------------

-- * Datatypes

-----------------------------------------
-- | type constructor names
type TyConName = String

-- | data constructor names
type DataConName = String

-- | The names of type/data constructors used in the module
data ConstructorNames = ConstructorNames
  { tconNames :: Set String,
    dconNames :: Set String
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | A Data constructor has a name and a telescope of arguments
data CtorDef = CtorDef DataConName Telescope Type
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- ** Telescopes

-- | A telescope is like a first class context. It is a list of 
-- assumptions, binding each variable in terms that appear
-- later in the list. 
-- For example
--     Delta = [ x:Type , y:x, y = w ]
type Telescope = [TypeDecl]

  -- deriving (Show, Generic)
  -- deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-----------------------------------------
-- Definitions related to datatypes

-- | Is this the syntax of a literal (natural) number
isNumeral :: Term -> Maybe Int
isNumeral (DataCon "zero") = Just 0
isNumeral (App (DataCon "succ") predTerm) | Just pred <- isNumeral predTerm = Just (pred + 1)
isNumeral _ = Nothing

-- | Is this pattern a variable
isPatVar :: Pattern -> Bool
isPatVar (PatVar {}) = True
isPatVar _ = False

-----------------------------------------
-- * Auxiliary functions on syntax
-----------------------------------------


-- | Remove source positions and type annotations from the top level of a term
strip :: Term -> Term
strip (Ann tm _) = strip tm
strip tm = tm



-----------------------------------------
-- * Unbound library
-----------------------------------------

-- We use the unbound-generics library to mark the binding occurrences of
-- variables in the syntax. That allows us to automatically derive
-- functions for alpha-equivalence, free variables and substitution
-- using generic programming. 

-- The definitions below specialize the generic operations from the libary
-- to some of the common uses that we need in pi-forall

-- | Determine when two terms are alpha-equivalent (see below)
aeq :: Term -> Term -> Bool
aeq = Unbound.aeq

-- | Calculate the free variables of a term 
fv :: Term -> [Unbound.Name Term]
fv = Unbound.toListOf Unbound.fv

-- | `subst x b a` means to replace `x` with `b` in `a`
-- i.e.  a [ b / x ]
subst :: TName -> Term -> Term -> Term
subst = Unbound.subst

-- | in a binder `x.a` replace `x` with `b` 
-- instantiate :: Unbound.Bind TName Term -> Term -> Term
-- instantiate bnd a = Unbound.instantiate bnd [a]

-- -- | in a binder `x.a` replace `x` with a fresh name
-- unbind :: (Unbound.Fresh m) => Unbound.Bind TName Term -> m (TName, Term)
-- unbind = Unbound.unbind

-- | in binders `x.a1` and `x.a2` replace `x` with a fresh name in both terms
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

-----------------

-- * Constructor Names

-- ConstructorNames are sets, so they also do not have an instance of the 
-- Generic class available so we cannot automatically define their 
-- Alpha and Subst instances.
instance Unbound.Alpha ConstructorNames where
  aeq' _ a1 a2 = a1 == a2
  fvAny' _ _ = pure
  open _ _ = id
  close _ _ = id
  isPat _ = mempty
  isTerm _ = mempty
  nthPatFind _ = mempty
  namePatFind _ = mempty
  swaps' _ _ = id
  freshen' _ x = return (x, mempty)
  lfreshen' _ x cont = cont x mempty
  acompare' _ _ _ = EQ

