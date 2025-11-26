module Bindings where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Language.Haskell.TH qualified as TH
import Syntax (Epsilon, Term)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Functor ((<&>))
import Data.List (intercalate, elemIndex)
import Data.Bifunctor (second, first, bimap)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.String.Interpolate (i)
import Control.Monad.Reader (asks, Reader, runReader)
import Control.Monad.State (StateT (runStateT), get, modify, lift)
import Control.Exception (assert)
import Data.Char (toLower, isUpper, toUpper)
import Control.Monad (zipWithM, join)
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Environment (Env)

destructureCtor :: TH.Con -> (TH.Name, [TH.Type])
destructureCtor (TH.NormalC name params) = (name, map snd params)
destructureCtor (TH.RecC name params) = (name, [paramType | (_, _, paramType) <- params])
destructureCtor ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

declOrderFromTypeInfo :: TH.Info -> StateT (Set TH.Name) TH.Q [TH.Name]
declOrderFromTypeInfo (TH.TyConI (TH.DataD [] typeName _ Nothing ctors _)) = do
  let paramTypes = concatMap (snd . destructureCtor) ctors
  prev <- concat <$> mapM declOrderHelper paramTypes
  return $ prev ++ [typeName]
declOrderFromTypeInfo (TH.TyConI (TH.TySynD typeName [] typeSynonym)) =
  (++ [typeName]) <$> declOrderHelper typeSynonym
declOrderFromTypeInfo (TH.TyConI (TH.NewtypeD [] typeName [] Nothing ctor _)) = do
  prev <- concat <$> mapM declOrderHelper (snd $ destructureCtor ctor)
  return $ prev ++ [typeName]
declOrderFromTypeInfo typeInfo = error [i|Type info not implemented: #{TH.pprint typeInfo}|]

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

-- Base class type for Python ctypes
data Base = Union | Structure deriving Show

-- Python class field: (field_name, field_type)
type Field = (String, TypeBinding)

data Binding =
  Function String [(String, TypeBinding)] TypeBinding
  | TypeAlias String TypeBinding
  | TaggedUnion {
    name :: String,
    arity :: Int,
    unionFields :: [Field],
    kindConstants :: [String]
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

-- Convert TH constructor to Python union field
ctorBinding :: TH.Con -> Reader [TypeParamName] (Maybe Field)
ctorBinding (TH.NormalC _ []) = return Nothing
ctorBinding (TH.NormalC ctorName [(_, paramType)]) =
  Just . (snakeCase ctorName,) <$> typeBinding paramType
ctorBinding (TH.NormalC ctorName paramTypes) =
  Just . (snakeCase ctorName,) . foldl App Tuple <$> mapM (typeBinding . snd) paramTypes
ctorBinding (TH.RecC ctorName params) = assert (length params >= 2) $
  let paramTypes = [paramType | (_, _, paramType) <- params]
  in Just . (snakeCase ctorName,) . foldl App Tuple <$> mapM typeBinding paramTypes
ctorBinding ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

-- Extract name from TH type parameter
typeParamName :: TH.TyVarBndr a -> TH.Name
typeParamName (TH.PlainTV name _) = name
typeParamName (TH.KindedTV name _ _) = name

-- Generate binding for data types with constructors
dataBinding :: TH.Name -> [TH.TyVarBndr a] -> [TH.Con] -> Binding
dataBinding typeName typeParams ctors = TaggedUnion name arity unionFields kindConstants where
  kindConstants =
    [[i|KIND_#{map toUpper $ snakeCase ctorName}|] | (ctorName, _) <- map destructureCtor ctors]
  unionFields = runReader (catMaybes <$> mapM ctorBinding ctors) typeParamNames
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
    (_, typeDef) <- fromJust <$> ctorBinding ctor
    return $ TypeAlias (TH.nameBase typeName) typeDef
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

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

indentation :: String
indentation = "    "

genFunction :: Binding -> Maybe String
genFunction (Function name paramBindingss returnTypeBinding) =
  Just (setSignature ++ "\n" ++ funcDecl ++ "\n" ++ indentation ++ funcBody) where
  funcDecl = [i|def #{name}(#{paramList}) -> #{returnType}:|]
  paramList = intercalate ", " [[i|#{paramName}: #{paramType}|] | (paramName, paramType) <- params]
  funcBody = [i|return call_export("#{name}", [#{intercalate ", " $ map fst params}])|]
  setSignature =
    [i|set_export_signature("#{name}", [#{intercalate ", " $ map snd params}], #{returnType})|]
  returnType = genTypeBinding returnTypeBinding
  params = map (second genTypeBinding) paramBindingss
genFunction _ = Nothing

genTypeAlias :: Binding -> Maybe String
genTypeAlias (TypeAlias typeName typeDef) = Just [i|#{typeName} = #{genTypeBinding typeDef}|]
genTypeAlias _ = Nothing

genTaggedUnion :: Binding -> Maybe String
genTaggedUnion (TaggedUnion name arity unionFields kindConstants) = let
  kindIndices = [0..length kindConstants - 1]
  kindHint = [i|kind: Literal[#{intercalate ", " $ map show kindIndices}]|]
  kindDecls = [[i|#{constant} = #{index}|] | (index, constant) <- zip kindIndices kindConstants]
  fieldDecl = [
    "_fields_ = [",
    "    (\"kind\", c_int32),",
    "    (\"union\", c_void_p)",
    "]" ]
  genInitMethod (fieldName, fieldType) =
    [[i|@classmethod|],
     [i|def init_#{fieldName}(cls, value: #{fieldType}):|],
     [i|    return init_tagged_union(cls, cls.KIND_#{map toUpper fieldName}, value)|]]
  initMethods = map (genInitMethod . second genTypeBinding) unionFields
  in Just $ intercalate ('\n':indentation) $ case arity of
  0 -> classDecl : intercalate [""] decls where
    decls = [[kindHint], kindDecls, fieldDecl] ++ initMethods
    classDecl = [i|class #{name}(Structure):|]
  _ -> classDecl : intercalate [""] decls where
    decls = [[kindHint], kindDecls, fieldDecl, classGetItemImpl] ++ initMethods
    classDecl = [i|class #{name}[#{intercalate ", " typeArgs}](Structure):|]
    classGetItemImpl = ["@classmethod", "@cache",
      [i|def __class_getitem__(cls, type_args: tuple[#{tupleTypeArgs}]) -> type:|],
      [i|    return type("#{name}", (cls,), { "type_args": type_args })|]]
    tupleTypeArgs = intercalate ", " [[i|type[#{typeArg}]|] | typeArg <- typeArgs]
    typeArgs = ["T" ++ show i | i <- [1..arity]]
genTaggedUnion _ = Nothing


-- Generate complete Python module with imports and class definitions
generateBindings :: [Binding] -> String
generateBindings bindings = intercalate "\n\n" (imports : classes ++ aliases ++ functions) where
  classes = mapMaybe genTaggedUnion bindings
  aliases = mapMaybe genTypeAlias bindings
  functions = mapMaybe genFunction bindings
  imports = intercalate "\n"
    ["from __future__ import annotations",
     "from ctypes import c_int32, c_void_p, Structure",
     "from functools import cache",
     "from typing import Literal",
     "from .base import \\",
     "    Bool, call_export, init_tagged_union, Int, List, set_export_signature, String, Tuple"]

alignOffsetUp :: Int -> Int -> Int
alignOffsetUp offset alignment =
  let remainder = offset `mod` alignment
  in if remainder == 0 then offset else offset + (alignment - remainder)

sizeOf :: forall a. F.Storable a => Int
sizeOf = F.sizeOf (undefined :: a)

alignment :: forall a. F.Storable a => Int
alignment = F.alignment (undefined :: a)

offsetName :: Int -> TH.Name
offsetName i = TH.mkName ("offset" ++ show i)

fieldName :: Int -> TH.Name
fieldName i = TH.mkName ("field" ++ show i)

offsetDecls :: [TH.Type] -> TH.Q [TH.Dec]
offsetDecls [] = return []
offsetDecls types = (++) <$> [d| offset0 = 0 |] <*> zipWithM offsetDecl [1..] prevCurrPairs where
  offsetDecl i (prevType, currType) = let
    prevOffset = return $ TH.VarE $ offsetName (i - 1)
    currOffset = return $ TH.VarP $ offsetName i
    offsetDef = [| alignOffsetUp ($prevOffset + sizeOf @($prevType)) (alignment @($currType)) |]
    in head <$> [d| $currOffset = $offsetDef |]
  prevCurrPairs = zipWith (curry $ join bimap pure) types (tail types)

peekMatchStmt :: Integer -> (TH.Name, [TH.Type]) -> TH.Q TH.Match
peekMatchStmt kind (ctorName, paramTypes) = do
  let ctorArgs = [TH.VarE (fieldName i) | i <- [0..length paramTypes - 1]]
  let ctorApplied = pure $ foldl TH.AppE (TH.ConE ctorName) ctorArgs
  let fieldDecl i = TH.BindS field <$> [| F.peekByteOff unionPtr $offset |] where
        field = TH.VarP $ fieldName i
        offset = pure $ TH.VarE $ offsetName i
  offsetDecls <- map (TH.LetS . pure) <$> offsetDecls paramTypes
  fieldDecls <- mapM fieldDecl [0..length paramTypes - 1]
  returnStmt <- TH.NoBindS <$> [| return $ctorApplied |]
  let matchBody = TH.NormalB $ TH.DoE Nothing (offsetDecls ++ fieldDecls ++ [returnStmt])
  let kindPattern = TH.LitP $ TH.IntegerL kind
  return $ TH.Match kindPattern matchBody []

pokeMatchStmt :: Integer -> (TH.Name, [TH.Type]) -> TH.Q TH.Match
pokeMatchStmt kind (ctorName, paramTypes) = let
  ctorPattern = TH.ConP ctorName [] [TH.VarP (fieldName i) | i <- [0..length paramTypes - 1]]
  matchFromBody body = TH.Match ctorPattern (TH.NormalB body) []
  fieldAlmts = [[| alignment @($paramType) |] | paramType <- map pure paramTypes]
  fieldsAlmt = [| maximum $(TH.ListE <$> sequence fieldAlmts) |]
  finalOffset = pure $ TH.VarE $ offsetName (length paramTypes - 1)
  finalSize = [| sizeOf @($(pure $ last paramTypes)) |]
  fieldsSize = [| alignOffsetUp ($finalOffset + $finalSize) $fieldsAlmt |]
  pokeFieldStmt i = TH.NoBindS <$> [| F.pokeByteOff unionPtr $offset $field |]
    where (field, offset) = join bimap (pure . TH.VarE) $ (fieldName &&& offsetName) i
  in matchFromBody <$> case paramTypes of
  [] -> [| return (kind, F.nullPtr) |]
  _ -> do
    offsetDecls <- map (TH.LetS . pure) <$> offsetDecls paramTypes
    allocStmt <- TH.BindS <$> [p|unionPtr|] <*> [| F.mallocBytes $fieldsSize |]
    pokeFieldStmts <- mapM pokeFieldStmt [0..length paramTypes - 1]
    returnStmt <- TH.NoBindS <$> [| return (kind, unionPtr) |]
    return $ TH.DoE Nothing (offsetDecls ++ [allocStmt] ++ pokeFieldStmts ++ [returnStmt])

implMethod :: TH.Name -> [TH.Pat] -> TH.Exp -> TH.Dec
implMethod methodName args body = TH.FunD methodName [TH.Clause args (TH.NormalB body) []]

storableForData :: TH.Name -> [TH.Type] -> [(TH.Name, [TH.Type])] -> TH.Q TH.Dec
storableForData typeName typeParams ctors =
  TH.InstanceD Nothing constraints <$> storable <*> decls where
  decls = sequence [alignmentImpl, sizeOfImpl, peekImpl, pokeImpl]
  storable = TH.AppT (TH.ConT ''F.Storable) <$> selfType
  constraints = map (TH.AppT $ TH.ConT ''F.Storable) typeParams

  alignmentImpl = implMethod 'F.alignment [TH.WildP] <$>
    [| max (alignment @F.CInt) (alignment @(F.Ptr ())) |]

  sizeOfImpl = implMethod 'F.sizeOf [TH.WildP] <$>
    [| alignOffsetUp ($unionOffset + sizeOf @(F.Ptr ())) (alignment @($selfType)) |]
  selfType = pure $ foldl TH.AppT (TH.ConT typeName) typeParams

  peekImpl = implMethod 'F.peek [ptrArg] <$> [| do
    kind :: F.CInt <- F.peekByteOff $ptr 0
    unionPtr :: F.Ptr () <- F.peekByteOff $ptr $unionOffset
    $(TH.CaseE <$> [|kind|] <*> zipWithM peekMatchStmt [0..] ctors) |]

  pokeImpl = implMethod 'F.poke [ptrArg, selfArg] <$> [| do
    (kind :: F.CInt, unionPtr) <- $(TH.CaseE <$> self <*> zipWithM pokeMatchStmt [0..] ctors)
    F.pokeByteOff $ptr 0 kind
    F.pokeByteOff $ptr $unionOffset unionPtr |]

  (ptrArg, ptr) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "ptr")
  (selfArg, self) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "self")
  unionOffset = [| alignOffsetUp (sizeOf @F.CInt) (alignment @(F.Ptr ())) |]

storableForAlias :: TH.Name -> (TH.Name, [TH.Type]) -> TH.Q TH.Dec
storableForAlias typeName (ctorName, [wrappedType]) =
  TH.InstanceD Nothing [] storable <$> decls where
  decls = sequence [alignmentImpl, sizeOfImpl, peekImpl, pokeImpl]
  storable = TH.AppT (TH.ConT ''F.Storable) (TH.ConT typeName)

  alignmentImpl = implMethod 'F.alignment [TH.WildP] <$> [| alignment @($(pure wrappedType)) |]
  sizeOfImpl = implMethod 'F.sizeOf [TH.WildP] <$> [| sizeOf @($(pure wrappedType)) |]

  peekImpl = implMethod 'F.peek [ptrArg] <$>
    [| $(pure $ TH.ConE ctorName) <$> F.peek (F.castPtr $ptr) |]

  pokeImpl = implMethod 'F.poke [ptrArg, TH.ConP ctorName [] [xArg]] <$>
    [| F.poke (F.castPtr $ptr) $x |]

  (ptrArg, ptr) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "ptr")
  (xArg, x) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "x")
storableForAlias _ _ = undefined

implStorable :: TH.Name -> TH.Q (Maybe TH.Dec)
implStorable typeName = TH.reify typeName >>= \case
  TH.TyConI (TH.DataD [] _ typeParamBinds Nothing ctors _) ->
    Just <$> storableForData typeName typeParams (map destructureCtor ctors)
    where typeParams = map (TH.VarT . typeParamName) typeParamBinds
  TH.TyConI (TH.TySynD _ [] typeSynonym) -> return Nothing
  TH.TyConI (TH.NewtypeD [] _ [] Nothing ctor _) ->
    Just <$> storableForAlias typeName (destructureCtor ctor)
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

dbg :: TH.Q TH.Exp
dbg = do
  let stringify = TH.LitE . TH.StringL
  declOrder <- buildDeclOrder ''Env
  classBindings <- (:) <$> bindingFromName ''Either <*> mapM bindingFromName declOrder
  let paramTypeBindings = [("env", TypeVar "Env"), ("term", TypeVar "Term")]
  let returnType = App (App (TypeVar "Either") (BaseType String)) (TypeVar "Type")
  let bindings = classBindings ++ [Function "infer_type" paramTypeBindings returnType]
  return $ stringify $ generateBindings bindings