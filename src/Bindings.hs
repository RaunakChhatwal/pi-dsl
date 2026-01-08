module Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings) where

import Language.Haskell.TH qualified as TH
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Functor ((<&>))
import Data.List (intercalate, elemIndex)
import Data.Bifunctor (second)
import Data.Maybe (fromJust, mapMaybe)
import Data.String.Interpolate (i)
import Control.Monad.Reader (asks, Reader, runReader)
import Control.Monad.State (StateT (runStateT), get, modify, lift)
import Control.Exception (assert)
import Data.Char (toLower, isUpper, toUpper)
import Control.Arrow ((&&&))
import Data.Either (fromRight, rights)
import Data.Bifoldable (biList)

-- Extract constructor name and parameter types from a TH constructor
ctorNameAndParams :: TH.Con -> (TH.Name, [TH.Type])
ctorNameAndParams (TH.NormalC name params) = (name, map snd params)
ctorNameAndParams (TH.RecC name params) = (name, [paramType | (_, _, paramType) <- params])
ctorNameAndParams ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

-- Compute declaration order from type info, collecting dependencies
declOrderFromTypeInfo :: TH.Info -> StateT (Set TH.Name) TH.Q [TH.Name]
declOrderFromTypeInfo (TH.TyConI (TH.DataD [] typeName _ Nothing ctors _)) = do
  let paramTypes = concatMap (snd . ctorNameAndParams) ctors
  prev <- concat <$> mapM declOrderHelper paramTypes
  return $ prev ++ [typeName]
declOrderFromTypeInfo (TH.TyConI (TH.TySynD typeName [] typeSynonym)) =
  (++ [typeName]) <$> declOrderHelper typeSynonym
declOrderFromTypeInfo (TH.TyConI (TH.NewtypeD [] typeName [] Nothing ctor _)) = do
  prev <- concat <$> mapM declOrderHelper (snd $ ctorNameAndParams ctor)
  return $ prev ++ [typeName]
declOrderFromTypeInfo typeInfo = error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Helper to traverse types and collect declaration dependencies
declOrderHelper :: TH.Type -> StateT (Set TH.Name) TH.Q [TH.Name]
declOrderHelper (TH.ConT typeName) = do
  vis <- get
  let visited = typeName `Set.member` vis
  let primitive = TH.nameBase typeName `elem` ["Bool", "Int", "Integer", "String"]
  if visited || primitive then return []
  else do
    typeInfo <- lift (TH.reify typeName)
    modify (Set.insert typeName)
    declOrderFromTypeInfo typeInfo
declOrderHelper (TH.AppT a b) = (++) <$> declOrderHelper a <*> declOrderHelper b
declOrderHelper TH.ListT = return []
declOrderHelper (TH.TupleT 2) = return []
declOrderHelper (TH.VarT {}) = return []
declOrderHelper type' = error [i|Type not implemented: #{TH.pprint type'}|]

-- Build topologically sorted declaration order starting from a root type
buildDeclOrder :: TH.Name -> TH.Q [TH.Name]
buildDeclOrder root = fst <$> runStateT (declOrderHelper $ TH.ConT root) Set.empty

-- Primitive C types that map to ctypes
data BaseType = Bool | Int | String deriving Show

-- Abstract representation of Python type expressions
data TypeBinding =
  List
  | Tuple
  | BaseType BaseType
  | TypeVar String
  | TypeArg Int
  | App TypeBinding TypeBinding

-- Python class field: (field_name, field_type)
type Field = (String, TypeBinding)

-- Python binding: function, type alias, or tagged union class
data Binding =
  Function String [(String, TypeBinding)] TypeBinding
  | TypeAlias String TypeBinding
  | TaggedUnion {
    name :: String,
    arity :: Int,
    ctors :: [Either String Field]
  }

-- Type parameter names for generic type resolution
type TypeParamName = TH.Name

-- Convert TH Type to TypeBinding, resolving type parameters
typeBinding :: TH.Type -> Reader [TypeParamName] TypeBinding
typeBinding (TH.ConT typeName) = return $ case TH.nameBase typeName of
  "Bool" -> BaseType Bool
  "Int" -> BaseType Int
  "Integer" -> BaseType Int
  "String" -> BaseType String
  typeName -> TypeVar typeName
typeBinding (TH.AppT a b) = App <$> typeBinding a <*> typeBinding b
typeBinding (TH.TupleT {}) = return Tuple
typeBinding TH.ListT = return List
typeBinding (TH.VarT name) = asks (TypeArg . fromJust . elemIndex name)
typeBinding type' = error [i|Type not implemented: #{show type'}|]

-- Convert CamelCase names to snake_case format
snakeCase :: TH.Name -> String
snakeCase typeName = map toLower $ go (TH.nameBase typeName) where
  go [] = []
  go (c1:c2:cs) | isUpper c2 = c1 : '_' : go (c2 : cs)
  go (c:cs) = c : go cs

-- Convert TH constructor to Either nullary name or field
ctorBinding :: TH.Con -> Reader [TypeParamName] (Either String Field)
ctorBinding (TH.NormalC ctorName []) = return $ Left (snakeCase ctorName)
ctorBinding (TH.NormalC ctorName [(_, paramType)]) =
  Right . (snakeCase ctorName,) <$> typeBinding paramType
ctorBinding (TH.NormalC ctorName paramTypes) =
  Right . (snakeCase ctorName,) . foldl App Tuple <$> mapM (typeBinding . snd) paramTypes
ctorBinding (TH.RecC ctorName params) = assert (length params >= 2) $
  let paramTypes = [paramType | (_, _, paramType) <- params]
  in Right . (snakeCase ctorName,) . foldl App Tuple <$> mapM typeBinding paramTypes
ctorBinding ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

-- Extract name from TH type parameter
typeParamName :: TH.TyVarBndr a -> TH.Name
typeParamName (TH.PlainTV name _) = name
typeParamName (TH.KindedTV name _ _) = name

-- Generate binding for data types with constructors
dataBinding :: TH.Name -> [TH.TyVarBndr a] -> [TH.Con] -> Binding
dataBinding typeName typeParams ctors = TaggedUnion name arity ctorBindings where
  ctorBindings = runReader (mapM ctorBinding ctors) typeParamNames
  typeParamNames = map typeParamName typeParams
  arity = length typeParams
  name = TH.nameBase typeName

-- Generate complete Python binding from Haskell type name
bindingFromName :: TH.Name -> TH.Q Binding
bindingFromName typeName = TH.reify typeName <&> \case
  TH.TyConI (TH.DataD [] _ typeParams Nothing ctors _) ->
    dataBinding typeName typeParams ctors
  TH.TyConI (TH.TySynD _ [] typeSynonym) -> TypeAlias (TH.nameBase typeName) typeDef
    where typeDef = runReader (typeBinding typeSynonym) []
  TH.TyConI (TH.NewtypeD [] _ [] Nothing ctor _) -> flip runReader [] $ do
    (_, typeDef) <- fromRight undefined <$> ctorBinding ctor
    return $ TypeAlias (TH.nameBase typeName) typeDef
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Create a function binding from name, parameter names, types, and return type
functionBinding :: String -> [String] -> [TH.Type] -> TH.Type -> Binding
functionBinding name paramNames paramTypes returnType =
  Function name (zip paramNames paramTypeBindings) returnTypeBinding where
  returnTypeBinding = runReader (typeBinding returnType) undefined
  paramTypeBindings = runReader (mapM typeBinding paramTypes) undefined

-- Unfold type application: F[A,B] -> (F, [A,B])
unfoldApp :: TypeBinding -> (TypeBinding, [TypeBinding])
unfoldApp (App a b) = second (++ [b]) (unfoldApp a)
unfoldApp fieldType = (fieldType, [])

-- Generate Python type expression string
genTypeBinding :: TypeBinding -> String
genTypeBinding List = "List"
genTypeBinding Tuple = "Tuple"
genTypeBinding (BaseType baseType) = show baseType
genTypeBinding (TypeVar name) = name
genTypeBinding (TypeArg index) = "T" ++ show (index + 1)
genTypeBinding (App a b) =
  [i|#{genTypeBinding typeCtor}[#{intercalate ", " (map genTypeBinding typeArgs)}]|]
  where (typeCtor, typeArgs) = unfoldApp (App a b)

-- Indent each line of a string by four spaces
indent :: String -> String
indent snippet = intercalate "\n" $ map ("    " ++) $ lines snippet

-- Generate Python function definition code from a Function binding
genFunction :: Binding -> Maybe String
genFunction (Function name paramBindingss returnTypeBinding) =
  Just $ intercalate "\n" [setSignature, funcDecl, indent funcBody] where
  funcDecl = [i|def #{name}(#{paramList}) -> #{returnType}:|]
  paramList = intercalate ", " [[i|#{paramName}: #{paramType}|] | (paramName, paramType) <- params]
  funcBody = [i|return call_export("#{name}", [#{intercalate ", " $ map fst params}])|]
  setSignature =
    [i|set_export_signature("#{name}", [#{intercalate ", " $ map snd params}], #{returnType})|]
  returnType = genTypeBinding returnTypeBinding
  params = map (second genTypeBinding) paramBindingss
genFunction _ = Nothing

-- Generate Python type alias code from a TypeAlias binding
genTypeAlias :: Binding -> Maybe String
genTypeAlias (TypeAlias typeName typeDef) = Just [i|#{typeName} = #{genTypeBinding typeDef}|]
genTypeAlias _ = Nothing

-- Generate Python tagged union class code from a TaggedUnion binding
genTaggedUnion :: Binding -> Maybe String
genTaggedUnion (TaggedUnion name arity ctors) = Just classDecl where
  classDecl = [i|class #{name}#{typeArgs}(TaggedUnion):|] ++ "\n" ++ indent classBody
  classBody = intercalate "\n\n" $ [kindHint, kindDecls] ++ accessors
  typeArgs = if arity == 0 then ""
    else [i|[#{intercalate ", " ["T" ++ show i | i <- [1..arity]]}]|]

  accessors =
    concatMap (biList . (genInitMethod &&& genGetMethod) . second paramHints) (rights ctors)
  paramHints fieldType = map genTypeBinding $ case unfoldApp fieldType of
    (Tuple, paramTypes) -> paramTypes
    _ -> [fieldType]
  genGetMethod (name, [paramHint]) = intercalate "\n"
    [[i|def get_#{name}(self) -> #{paramHint}:|],
     [i|    assert self.kind == self.#{kindConstant name}|],
     [i|    return self.get_field(#{paramHint})|]]
  genGetMethod (name, paramHints) = intercalate "\n"
    [[i|def get_#{name}(self) -> tuple[#{intercalate ", " paramHints}]:|],
     [i|    assert self.kind == self.#{kindConstant name}|],
     [i|    return self.get_field(Tuple[#{intercalate ", " paramHints}]).get()|]]
  genInitMethod (name, [paramHint]) = intercalate "\n"
    [[i|@classmethod|],
     [i|def init_#{name}(cls, value: #{paramHint}):|],
     [i|    return cls(cls.#{kindConstant name}, value)|]]
  genInitMethod (name, paramHints) = intercalate "\n"
    [[i|@classmethod|],
     [i|def init_#{name}(cls, *values: *tuple[#{intercalate ", " paramHints}]):|],
     [i|    return cls(cls.#{kindConstant name}, init_tuple(*values))|]]

  kindDecls = intercalate "\n" [[i|#{kindConstant ctorName} = #{index}|]
    | (index, ctorName) <- zip kindIndices $ map (either id fst) ctors]
  kindHint = [i|kind: Literal[#{intercalate ", " $ map show kindIndices}]|]
  kindIndices = [0..length ctors - 1]
  kindConstant ctorName = "KIND_" ++ map toUpper ctorName
genTaggedUnion _ = Nothing

-- Generate complete Python module with imports and class definitions
generateBindings :: [Binding] -> String
generateBindings bindings = intercalate "\n\n" (imports : classes ++ aliases ++ functions) where
  classes = mapMaybe genTaggedUnion bindings
  aliases = mapMaybe genTypeAlias bindings
  functions = mapMaybe genFunction bindings
  imports = intercalate "\n"
    ["from __future__ import annotations",
     "from typing import Literal",
     "from .base import *"]
