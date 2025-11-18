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
import Data.Maybe (catMaybes, fromJust)
import Data.String.Interpolate (i)
import Control.Monad.Reader (asks, Reader, runReader)
import Control.Monad.State (StateT (runStateT), get, modify, lift)
import Control.Exception (assert)
import Data.Char (toLower, isUpper)
import Control.Monad (zipWithM, join)
import Control.Arrow ((&&&))
import Data.Bool (bool)

destructureCtor :: TH.Con -> (TH.Name, [TH.Type])
destructureCtor (TH.NormalC name params) = (name, map snd params)
destructureCtor (TH.RecC name params) = (name, [paramType | (_, _, paramType) <- params])
destructureCtor _ = undefined

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
data CType = CBool | CInt32 | CInt64 | CCharP

-- Abstract representation of Python type expressions
data TypeBinding =
  List
  | Tuple
  | CType CType
  | TypeVar String
  | TypeArg Int
  | Pointer TypeBinding
  | App TypeBinding TypeBinding

-- Base class type for Python ctypes
data Base = Union | Structure deriving Show

-- Python class field: (field_name, field_type)
type Field = (String, TypeBinding)

-- Python class definition with fields and generic parameters
data ClassBinding = ClassBinding {
  base :: Base,
  name :: String,
  arity :: Int,
  fields :: [Field]
}

-- Complete binding: tagged union or type alias
data Binding = TaggedUnion (Maybe ClassBinding) ClassBinding | TypeAlias String TypeBinding

-- Type parameter names for generic type resolution
type TypeParamName = TH.Name

-- Convert TH Type to TypeBinding, resolving type parameters
typeBinding :: TH.Type -> Reader [TypeParamName] TypeBinding
typeBinding (TH.ConT typeName) = return $ case TH.nameBase typeName of
  "Bool" -> CType CBool
  "Int" -> CType CInt64
  "Integer" -> CType CInt64
  "String" -> CType CCharP
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
  let paramTypes = map (\(_, _, type') -> type') params
  in Just . (snakeCase ctorName,) . foldl App Tuple <$> mapM typeBinding paramTypes
ctorBinding ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

-- Extract name from TH type parameter
typeParamName :: TH.TyVarBndr a -> TH.Name
typeParamName (TH.PlainTV name _) = name
typeParamName (TH.KindedTV name _ _) = name

-- Generate binding for data types with constructors
dataBinding :: TH.Name -> [TH.TyVarBndr a] -> [TH.Con] -> Binding
dataBinding typeName typeParams ctors = let
  name = TH.nameBase typeName
  arity = length typeParams
  typeParamNames = map typeParamName typeParams
  unionFields = runReader (catMaybes <$> mapM ctorBinding ctors) typeParamNames
  union = ClassBinding Union [i|#{name}Union|] arity unionFields
  kind = ("kind", CType CInt32)
  structFields = [kind, ([i|#{snakeCase typeName}_union|], Pointer (TypeVar [i|#{name}Union|]))]
  structure = ClassBinding Structure name arity structFields
  in case unionFields of
  [] -> TaggedUnion Nothing $ structure { fields = [kind] }
  [field] -> TaggedUnion Nothing $ structure { fields = [kind, second Pointer field] }
  _ -> TaggedUnion (Just union) structure

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
genTypeBinding (CType CBool) = "Bool"
genTypeBinding (CType CInt32) = "c_int32"
genTypeBinding (CType CInt64) = "Int"
genTypeBinding (CType CCharP) = "String"
genTypeBinding (TypeVar name) = name
genTypeBinding (TypeArg index) = [i|type_args[#{index}]|]
genTypeBinding (Pointer typeBinding) = [i|POINTER(#{genTypeBinding typeBinding})|]
genTypeBinding (App a b) =
  [i|#{genTypeBinding typeCtor}[#{intercalate ", " (map genTypeBinding typeArgs)}]|]
  where (typeCtor, typeArgs) = unfoldApp (App a b)

-- Generate Python field tuple, using c_void_p for pointers
genField :: (String, TypeBinding) -> String
genField (fieldName, Pointer fieldType) =
  [i|("#{fieldName}", c_void_p), \# #{genTypeBinding fieldType}|]
genField (fieldName, fieldType) = [i|("#{fieldName}", #{genTypeBinding fieldType}),|]

-- Generate complete Python class definition
genClassBinding :: ClassBinding -> String
genClassBinding (ClassBinding base name arity fields) = let
  classDecl = [i|class #{name}(#{base}):|]
  indentation = "    "
  fieldDecls = map ((indentation ++) . genField) fields
  in intercalate ('\n':indentation) $ case arity of
    0 -> [classDecl, "_fields_ = ["] ++ fieldDecls ++ ["]"]
    _ -> classDecl : methodDecl ++ map (indentation ++) methodBody where
      methodDecl = ["@classmethod", "@cache",
        [i|def __class_getitem__(cls, type_args: #{typeArgsHint}) -> type:|]]
      typeArgsHint :: String = [i|tuple[#{intercalate ", " (replicate arity "type")}]|]
      methodBody = ["fields: list[tuple[str, type]] = ["] ++ fieldDecls ++ ["]", methodReturn]
      methodReturn = [i|return type("#{name}", (cls,), { "_fields_": fields })|]

-- Generate complete Python module with imports and class definitions
generateBindings :: [Binding] -> String
generateBindings bindings = intercalate "\n\n" (imports : decls) where
  imports = intercalate "\n"
    ["from ctypes import c_int32, c_void_p, Structure, Union",
     "from functools import cache",
     "from .base import Bool, Int, List, String, Tuple"]
  decls = structures ++ typeAliases ++ unions
  structures = [genClassBinding structure | TaggedUnion _ structure <- bindings]
  typeAliases =
    [[i|#{typeName} = #{genTypeBinding typeDef}|] | TypeAlias typeName typeDef <- bindings]
  unions = [genClassBinding union | TaggedUnion (Just union) _ <- bindings]

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

  alignmentImpl = implMethod 'F.alignment [TH.WildP] <$> if kindOnly
    then [| alignment @F.CInt |]
    else [| max (alignment @F.CInt) (alignment @(F.Ptr ())) |]

  sizeOfImpl = implMethod 'F.sizeOf [TH.WildP] <$> if kindOnly
    then [| sizeOf @F.CInt |]
    else [| alignOffsetUp ($unionOffset + sizeOf @(F.Ptr ())) (alignment @($selfType)) |]
  selfType = pure $ foldl TH.AppT (TH.ConT typeName) typeParams

  peekImpl = implMethod 'F.peek [ptrArg] <$> [| do
    kind :: F.CInt <- F.peekByteOff $ptr 0
    unionPtr <- $(bool [| F.peekByteOff $ptr $unionOffset |] [| return undefined |] kindOnly)
    $(TH.CaseE <$> [|kind|] <*> zipWithM peekMatchStmt [0..] ctors) |]

  pokeImpl = implMethod 'F.poke [ptrArg, selfArg] <$> [| do
    (kind :: F.CInt, unionPtr) <- $(TH.CaseE <$> self <*> zipWithM pokeMatchStmt [0..] ctors)
    F.pokeByteOff $ptr 0 kind
    $(bool [| F.pokeByteOff $ptr $unionOffset unionPtr |] [| return () |] kindOnly) |]

  (ptrArg, ptr) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "ptr")
  (selfArg, self) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "self")
  unionOffset = [| alignOffsetUp (sizeOf @F.CInt) (alignment @(F.Ptr ())) |]
  kindOnly = all (null . snd) ctors

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
-- dbg = stringify . TH.pprint . (\(TH.ClassI (TH.ClassD _ _ _ [] decs)  _) -> decs) <$> TH.reify ''F.Storable
-- dbg = stringify . TH.pprint <$> instantiateStorable ''Epsilon
dbg = stringify . generateBindings <$> (mapM bindingFromName =<< buildDeclOrder ''Term)
-- dbg = stringify . show <$> inst
  where
        -- inst = [d|instance F.Storable a => F.Storable [a] where alignment _ = undefined|]
        stringify = TH.LitE . TH.StringL
-- dbg = stringify . show . instanceDecls <$> TH.reify ''F.Storable
--   where instanceDecls (TH.ClassI _ ((TH.InstanceD _ _ type' decls):_)) =
--           (TH.pprint type', show decls)
--         instanceDecls _ = undefined
--         stringify = TH.LitE . TH.StringL