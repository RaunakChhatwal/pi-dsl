module Syntax where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic, from)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Bind qualified as Unbound

-- Term name alias used by Unbound
type TermName = Unbound.Name Term

-- Because types and terms use the same AST, we define the following
-- type synonym so that we can hint whether a piece of syntax is being used
-- as a type or as a term.
type Type = Term

type UnivParamName = String

-- Universe level
data Level
  = Zero
  | Param UnivParamName
  | LMVar Int
  | Succ Level
  | Max Level Level
  deriving (Eq, Ord, Generic, Show) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data Const
  = GVar String
  | DataType DataTypeName
  | Ctor DataTypeName CtorName
  | Rec DataTypeName
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

data BinderInfo = Explicit | Implicit
  deriving (Show, Eq, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- pi-dsl abstract syntax tree
data Term
  = Sort Level
  | LVar TermName
  | MVar Int
  | Const Const [Level]
  | Lam BinderInfo (Unbound.Bind TermName Term)
  | App Term Term
  | Pi BinderInfo Type (Unbound.Bind TermName Type)
  | Ann Term Type
  deriving (Show, Generic)

-- Data type names are represented as strings
type DataTypeName = String
-- Constructor names are represented as strings
type CtorName = String
-- Top-level environment entries (declarations and data types)
data Entry
  = Decl String [UnivParamName] Type Term
  | Data DataTypeName [UnivParamName] Type [(CtorName, Type)]
  deriving (Show, Generic) deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

levelToInt :: Level -> Maybe Int
levelToInt Zero = Just 0
levelToInt (Succ level) = (1 +) <$> levelToInt level
levelToInt _ = Nothing

unfoldSucc :: Level -> (Level, Int)
unfoldSucc (Succ level) = second (1 +) $ unfoldSucc level
unfoldSucc level = (level, 0)

foldSucc :: (Level, Int) -> Level
foldSucc (level, offset) = iterate Succ level !! offset

getOffset :: Level -> Int
getOffset = snd . unfoldSucc

substLevelsInLevel :: [(UnivParamName, Level)] -> Level -> Level
substLevelsInLevel substs = \case
  Zero -> Zero
  Succ level -> Succ $ substLevelsInLevel substs level
  Max level1 level2 -> Max (substLevelsInLevel substs level1) (substLevelsInLevel substs level2)
  Param univParamName -> fromMaybe (Param univParamName) $ lookup univParamName substs
  LMVar id -> LMVar id

substLevels :: [(UnivParamName, Level)] -> Term -> Term
substLevels substs = \case
  Sort level -> Sort $ substLevelsInLevel substs level
  Const constant levels -> Const constant $ map (substLevelsInLevel substs) levels
  App func arg -> App (substLevels substs func) (substLevels substs arg)
  Lam binderInfo (Unbound.B paramName body) ->
    Lam binderInfo $ Unbound.B paramName $ substLevels substs body
  Pi binderInfo paramType (Unbound.B paramName returnType) ->
    let binder = Unbound.B paramName $ substLevels substs returnType
    in Pi binderInfo (substLevels substs paramType) binder
  Ann term type' -> Ann (substLevels substs term) (substLevels substs type')
  term -> term

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
  isvar (LVar name) = Just $ Unbound.SubstName name
  isvar _ = Nothing
