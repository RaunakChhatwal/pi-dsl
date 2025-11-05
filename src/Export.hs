{-# LANGUAGE ForeignFunctionInterface #-}

module Export(collectBindings, generateBindings) where

import Foreign.C.Types qualified as F
import Foreign qualified as F
import Foreign.C.String qualified as F
import Language.Haskell.TH qualified as TH
import Syntax (Term)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Functor ((<&>))
import Data.List (intercalate, elemIndex)
import Data.Bifunctor (second)
import Data.Maybe (catMaybes, fromJust)
import Data.String.Interpolate (i)
import Control.Monad.Reader (asks, Reader, runReader)
import Control.Monad.State (StateT (runStateT), get, modify, lift)
import Control.Exception (assert)
import Data.Char (toLower, isUpper)

-- Extract parameter types from TH constructor
conParamTypes :: TH.Con -> [TH.Type]
conParamTypes (TH.NormalC _ params) = map snd params
conParamTypes (TH.RecC _ params) = map (\(_, _, paramType) -> paramType) params
conParamTypes constructor = error [i|Not implemented: #{TH.pprint constructor}|]

-- Convert TH type info into dependency forest, tracking visited types
graphFromTypeInfo :: TH.Info -> StateT (Set TH.Name) TH.Q (Tree.Forest TH.Name)
graphFromTypeInfo (TH.TyConI (TH.DataD [] typeName _ Nothing constructors _)) = do
  let paramTypes = concatMap conParamTypes constructors
  forest <- concat <$> mapM typeGraphHelper paramTypes
  return [Tree.Node typeName forest]
graphFromTypeInfo (TH.TyConI (TH.TySynD typeName [] typeSynonym)) = do
  forest <- typeGraphHelper typeSynonym
  return [Tree.Node typeName forest]
graphFromTypeInfo (TH.TyConI (TH.NewtypeD [] typeName [] Nothing constructor _)) = do
  forest <- concat <$> mapM typeGraphHelper (conParamTypes constructor)
  return [Tree.Node typeName forest]
graphFromTypeInfo typeInfo = error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Recursively build type dependency forest, avoiding cycles
typeGraphHelper :: TH.Type -> StateT (Set TH.Name) TH.Q (Tree.Forest TH.Name)
typeGraphHelper (TH.ConT typeName) = do
  vis <- get
  let visited = typeName `Set.member` vis
  let primitive = TH.nameBase typeName `elem` ["Bool", "Int", "Integer", "String"]
  if visited || primitive then return []
  else do
    typeInfo <- lift (TH.reify typeName)
    modify (Set.insert typeName)
    graphFromTypeInfo typeInfo
typeGraphHelper (TH.AppT a b) = (++) <$> typeGraphHelper a <*> typeGraphHelper b
typeGraphHelper TH.ListT = return []
typeGraphHelper (TH.TupleT {}) = return []
typeGraphHelper (TH.VarT {}) = return []
typeGraphHelper type' = error [i|Type not implemented: #{TH.pprint type'}|]

-- Build complete dependency graph starting from Term
buildTypeGraph :: TH.Q (Tree TH.Name)
buildTypeGraph = do
  ([graph], _) <- runStateT (typeGraphHelper (TH.ConT ''Term)) Set.empty
  return graph

-- Primitive C types that map to ctypes
data CType = CBool | CInt32 | CCharP

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
  "Int" -> CType CInt32
  "Integer" -> CType CInt32
  "String" -> CType CCharP
  typeName -> TypeVar typeName
typeBinding (TH.AppT a b) = App <$> typeBinding a <*> typeBinding b
typeBinding (TH.TupleT {}) = return Tuple
typeBinding TH.ListT = return List
typeBinding (TH.VarT name) = asks (TypeArg . fromJust . elemIndex name)
typeBinding type' = error [i|Type not implemented: #{show type'}|]

-- Convert CamelCase names to snake_case format
snakeCase :: TH.Name -> String
snakeCase typeName = map toLower $ go (TH.nameBase typeName)
  where
    go [] = []
    go (c1:c2:cs) | isUpper c2 = c1 : '_' : (c2 : cs)
    go (c:cs) = c : go cs

-- Convert TH constructor to Python union field
constructorBinding :: TH.Con -> Reader [TypeParamName] (Maybe Field)
constructorBinding (TH.NormalC _ []) = return Nothing
constructorBinding (TH.NormalC conName [(_, paramType)]) =
  Just . (snakeCase conName,) <$> typeBinding paramType
constructorBinding (TH.NormalC conName paramTypes) =
  Just . (snakeCase conName,) . foldl App Tuple <$> mapM (typeBinding . snd) paramTypes
constructorBinding (TH.RecC conName params) = assert (length params >= 2) $
  let paramTypes = map (\(_, _, type') -> type') params
  in Just . (snakeCase conName,) . foldl App Tuple <$> mapM typeBinding paramTypes
constructorBinding constructor = error [i|Constructor not implemented: #{TH.pprint constructor}|]

-- Extract name from TH type parameter
typeParamName :: TH.TyVarBndr a -> TH.Name
typeParamName (TH.PlainTV name _) = name
typeParamName (TH.KindedTV name _ _) = name

-- Generate binding for data types with constructors
dataBinding :: TH.Name -> [TH.TyVarBndr a] -> [TH.Con] -> Binding
dataBinding typeName typeParams constructors =
  let
    name = TH.nameBase typeName
    arity = length typeParams
    typeParamNames = map typeParamName typeParams
    unionFields = runReader (catMaybes <$> mapM constructorBinding constructors) typeParamNames
    union = ClassBinding Union [i|#{name}Union|] arity unionFields
    kind = ("kind", CType CInt32)
    structFields = [kind, ([i|#{snakeCase typeName}_union|], Pointer $ TypeVar [i|#{name}Union|])]
    structure = ClassBinding Structure name arity structFields
  in case unionFields of
    [] -> TaggedUnion Nothing $ structure { fields = [kind] }
    [field] -> TaggedUnion Nothing $ structure { fields = [kind, second Pointer field] }
    _ -> TaggedUnion (Just union) structure

-- Generate complete Python binding from Haskell type name
bindingFromName :: TH.Name -> TH.Q Binding
bindingFromName typeName = TH.reify typeName <&> \case
  TH.TyConI (TH.DataD [] _ typeParams Nothing constructors _) ->
    dataBinding typeName typeParams constructors
  TH.TyConI (TH.TySynD _ [] typeSynonym) -> TypeAlias (TH.nameBase typeName) typeDef
    where typeDef = runReader (typeBinding typeSynonym) []
  TH.TyConI (TH.NewtypeD [] _ [] Nothing constructor _) -> flip runReader [] $ do
    (_, typeDef) <- fromJust <$> constructorBinding constructor
    return $ TypeAlias (TH.nameBase typeName) typeDef
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Collect all bindings in dependency order
collectBindings :: TH.Q [Binding]
collectBindings = buildTypeGraph >>= go where
  go (Tree.Node typeName children) = do
    binding <- bindingFromName typeName
    childBindings <- concat <$> mapM go children
    return $ childBindings ++ [binding]

-- Unfold type application: F[A,B] -> (F, [A,B])
unfoldApp :: TypeBinding -> (TypeBinding, [TypeBinding])
unfoldApp (App a b) = second (++ [b]) $ unfoldApp a
unfoldApp fieldType = (fieldType, [])

-- Generate Python type expression string
genTypeBinding :: TypeBinding -> String
genTypeBinding List = "List"
genTypeBinding Tuple = "Tuple"
genTypeBinding (CType CBool) = "c_bool"
genTypeBinding (CType CInt32) = "c_int32"
genTypeBinding (CType CCharP) = "c_char_p"
genTypeBinding (TypeVar name) = name
genTypeBinding (TypeArg index) = [i|type_args[#{index}]|]
genTypeBinding (Pointer typeBinding) = [i|POINTER(#{genTypeBinding typeBinding})|]
genTypeBinding (App a b) =
  [i|#{genTypeBinding typeCon}[#{intercalate ", " $ map genTypeBinding typeArgs}]|]
  where (typeCon, typeArgs) = unfoldApp (App a b)

-- Generate Python field tuple, using c_void_p for pointers
genField :: (String, TypeBinding) -> String
genField (fieldName, Pointer fieldType) =
  [i|("#{fieldName}", c_void_p), \# #{genTypeBinding fieldType}|]
genField (fieldName, fieldType) = [i|("#{fieldName}", #{genTypeBinding fieldType}),|]

-- Generate complete Python class definition
genClassBinding :: ClassBinding -> String
genClassBinding (ClassBinding base name arity fields) =
  let
    classDecl = [i|class #{name}(#{base}):|]
    indentation = "    "
    fieldDecls = map ((indentation ++) . genField) fields
  in intercalate ('\n':indentation) $ case arity of
    0 -> [classDecl, "_fields_ = ["] ++ fieldDecls ++ ["]"]
    _ -> [classDecl, methodDecl] ++ map (indentation ++) methodBody where
      methodDecl = [i|def __class_getitem__(cls, type_args: #{typeArgsHint}) -> type:|]
      typeArgsHint :: String = [i|tuple[#{intercalate ", " $ replicate arity "type"}]|]
      methodBody = ["fields: list[tuple[str, type]] = ["] ++ fieldDecls ++ ["]", methodReturn]
      methodReturn = [i|return type("#{name}", (cls,), { "_fields_": fields })|]

-- Python List[T] class declaration
listDecl :: String
listDecl = "class List(Structure):\n\
\    def __class_getitem__(cls, type_arg: type) -> type:\n\
\        fields: list[tuple[str, type]] = [\n\
\            (\"length\", c_size_t),\n\
\            (\"data\", POINTER(type_arg))\n\
\        ]\n\
\        return type(\"List\", (cls,), { \"_fields_\": fields })"

-- Python Tuple[T1, T2, ...] class declaration
tupleDecl :: String  
tupleDecl = "class Tuple(Structure):\n\
\    def __class_getitem__(cls, type_args: tuple[type, ...]) -> type:\n\
\        fields = [(f\"field{i}\", type_args[i]) for i in range(len(type_args))]\n\
\        return type(\"Tuple\", (cls,), { \"_fields_\": fields })"

-- Generate complete Python module with imports and class definitions
generateBindings :: [Binding] -> String
generateBindings bindings = intercalate "\n\n" (ctypesImport : decls) where
  ctypesImport = [i|from ctypes import #{intercalate ", " importTypes}|]
  importTypes =
    ["c_bool", "c_int32", "c_char_p", "c_size_t", "c_void_p", "POINTER", "Structure", "Union"]
  decls = [listDecl, tupleDecl] ++ structures ++ typeAliases ++ unions
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

instance (F.Storable a, F.Storable b) => F.Storable (a, b) where
  alignment _ = max (alignment @a) (alignment @b)

  sizeOf _ = alignOffsetUp (bOffset + sizeOf @b) (alignment @(a, b))
    where bOffset = alignOffsetUp (sizeOf @a) (alignment @b)

  peek ptr = do
    let bOffset = alignOffsetUp (sizeOf @a) (alignment @b)
    a <- F.peekByteOff ptr 0
    b <- F.peekByteOff ptr bOffset
    return (a, b)

  poke ptr (a, b) = do
    let bOffset = alignOffsetUp (sizeOf @a) (alignment @b)
    F.pokeByteOff ptr 0 a
    F.pokeByteOff ptr bOffset b

addLength :: F.Ptr (F.CInt, F.CString) -> IO F.CInt
addLength ptr = do
  (n, cstr) <- F.peek ptr
  str <- F.peekCString cstr
  return $ n + fromIntegral (length str)

foreign export ccall "add_length" addLength :: F.Ptr (F.CInt, F.CString) -> IO F.CInt

instance F.Storable a => F.Storable [a] where
  alignment _ = max (alignment @F.CSize) (alignment @(F.Ptr a))

  sizeOf _ = alignOffsetUp (dataOffset + sizeOf @(F.Ptr a)) (alignment @[a])
    where dataOffset = alignOffsetUp (sizeOf @F.CSize) (alignment @(F.Ptr a))

  peek ptr = do
    let dataOffset = alignOffsetUp (sizeOf @F.CSize) (alignment @(F.Ptr a))
    len :: F.CSize <- F.peekByteOff ptr 0
    dataPtr <- F.peekByteOff ptr dataOffset
    F.peekArray (fromIntegral len) dataPtr

  poke ptr xs = do
    let dataOffset = alignOffsetUp (sizeOf @F.CSize) (alignment @(F.Ptr a))
    dataPtr <- F.mallocArray (length xs)
    F.pokeArray dataPtr xs
    let len :: F.CSize = fromIntegral (length xs)
    F.pokeByteOff ptr 0 len
    F.pokeByteOff ptr dataOffset dataPtr

sumCInts :: F.Ptr [F.CInt] -> IO F.CInt
sumCInts ptr = sum <$> F.peek ptr

foreign export ccall "sum" sumCInts :: F.Ptr [F.CInt] -> IO F.CInt