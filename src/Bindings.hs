module Bindings where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Language.Haskell.TH qualified as TH
import Syntax (Epsilon, Term, Type)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Functor ((<&>))
import Data.List (intercalate, elemIndex)
import Data.Bifunctor (second, first, bimap)
import Data.Maybe (catMaybes, fromJust, mapMaybe, maybeToList)
import Data.String.Interpolate (i)
import Control.Monad.Reader (asks, Reader, runReader)
import Control.Monad.State (StateT (runStateT), get, modify, lift)
import Control.Exception (assert)
import Data.Char (toLower, isUpper, toUpper)
import Control.Monad (zipWithM, join, replicateM)
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Either (partitionEithers, fromRight)
import Environment (Env)
import Data.Bifoldable (biList)

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

indent :: String -> String
indent snippet = intercalate "\n" $ map ("    " ++) $ lines snippet

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

genTypeAlias :: Binding -> Maybe String
genTypeAlias (TypeAlias typeName typeDef) = Just [i|#{typeName} = #{genTypeBinding typeDef}|]
genTypeAlias _ = Nothing

genTaggedUnion :: Binding -> Maybe String
genTaggedUnion (TaggedUnion name arity ctors) =
  Just $ classDecl ++ nullaryCtorInstances where
  classDecl = [i|class #{name}#{typeArgs}(TaggedUnion):|] ++ "\n" ++ indent classBody
  classBody = intercalate "\n\n" $ [kindHint, kindDecls] ++ nullaryCtorHints ++ accessors
  typeArgs = if arity == 0 then ""
    else [i|[#{intercalate ", " ["T" ++ show i | i <- [1..arity]]}]|]

  nullaryCtorInstances = if null nullaryCtors then ""
    else "\n\n" ++ intercalate "\n" (map nullaryCtorInstance nullaryCtors)
  nullaryCtorInstance ctorName =
    [i|#{name}.#{ctorName} = #{name}(#{name}.#{kindConstant ctorName}, None)|]
  nullaryCtorHints = maybeToList $ if null nullaryCtors then Nothing
    else Just $ intercalate "\n" [[i|#{ctorName}: ClassVar[Self]|] | ctorName <- nullaryCtors]

  accessors =
    concatMap (biList . (genInitMethod &&& genGetMethod) . second genTypeBinding) unionFields
  (nullaryCtors, unionFields) = partitionEithers ctors
  genGetMethod (fieldName, fieldType) = intercalate "\n"
    [[i|def get_#{fieldName}(self) -> #{fieldType}:|],
     [i|    assert self.kind == self.#{kindConstant fieldName}|],
     [i|    return self.get_field(#{fieldType})|]]
  genInitMethod (fieldName, fieldType) = intercalate "\n"
    [[i|@classmethod|],
     [i|def init_#{fieldName}(cls, value: #{fieldType}):|],
     [i|    return cls(cls.#{kindConstant fieldName}, value)|]]

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
     "from typing import ClassVar, Literal, Self",
     "from .base import \
     \Bool, call_export, Int, List, set_export_signature, String, TaggedUnion, Tuple"]

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

-- Generate FFI export wrapper for a lambda expression with Storable types
exportFunction :: String -> [TH.Type] -> TH.Type -> TH.Exp -> TH.Q [TH.Dec]
exportFunction exportName paramTypes returnType function = do
  name <- TH.newName exportName

  -- ffi wrapper signature: F.Ptr a -> F.Ptr b -> ... -> IO (F.Ptr result)
  wrapperParamTypes <- sequence [[t|F.Ptr $paramType|] | paramType <- map pure paramTypes]
  wrapperReturnType <- [t|IO (F.Ptr $(pure returnType))|]
  let wrapperType = foldr (TH.AppT . TH.AppT TH.ArrowT) wrapperReturnType wrapperParamTypes
  let signature = TH.SigD name wrapperType

  -- ffi wrapper definition
  ptrNames <- replicateM (length paramTypes) (TH.newName "ptr")
  argNames <- replicateM (length paramTypes) (TH.newName "arg")
  peekStmts <- sequence [TH.BindS (TH.VarP argName) <$> [| F.peek $ptrName |]
    | (argName, ptrName) <- zipWith (curry $ second $ pure . TH.VarE) argNames ptrNames]
  (resultPtrPat, resultPtr) <- (TH.VarP &&& pure . TH.VarE) <$> TH.newName "resultPtr"
  mallocStmt <- TH.BindS resultPtrPat <$> [| F.malloc |]
  let result = foldl TH.AppE function (map TH.VarE argNames)
  pokeStmt <- TH.NoBindS <$> [| F.poke $resultPtr =<< $(pure result) |]
  returnStmt <- TH.NoBindS <$> [| return $resultPtr |]
  let body = TH.NormalB $ TH.DoE Nothing $ peekStmts ++ [mallocStmt, pokeStmt, returnStmt]
  let definition = TH.FunD name [TH.Clause (map TH.VarP ptrNames) body []]

  -- ffi export declaration
  let exportDecl = TH.ForeignD (TH.ExportF TH.CCall exportName name wrapperType)
  return [signature, definition, exportDecl]

dbg :: TH.Q TH.Exp
dbg = do
  let stringify = TH.LitE . TH.StringL
  declOrder <- buildDeclOrder ''Env
  bindings <- (++) <$> mapM bindingFromName [''Maybe, ''Either] <*> mapM bindingFromName declOrder
  pprTermBinding <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  inferTypeBinding <- functionBinding "infer_type" ["env", "term"]
    <$> sequence [[t|Env|], [t|Term|]] <*> [t| Either String Type |]
  checkTypeBinding <- functionBinding "check_type" ["env", "term", "ty"]
    <$> sequence [[t|Env|], [t|Term|], [t|Type|]] <*> [t| Maybe String |]
  let functionBindings = [pprTermBinding, inferTypeBinding, checkTypeBinding]
  return $ stringify $ generateBindings (bindings ++ functionBindings)