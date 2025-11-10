{-# LANGUAGE ForeignFunctionInterface #-}

module Export(dbg) where

import Foreign.C.Types qualified as F
import Foreign qualified as F
import Foreign.C.String qualified as F
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

-- Convert TH type info into dependency forest, tracking visited types
graphFromTypeInfo :: TH.Info -> StateT (Set TH.Name) TH.Q (Tree.Forest TH.Name)
graphFromTypeInfo (TH.TyConI (TH.DataD [] typeName _ Nothing ctors _)) = do
  let paramTypes = concatMap (snd . destructureCtor) ctors
  forest <- concat <$> mapM typeGraphHelper paramTypes
  return [Tree.Node typeName forest]
graphFromTypeInfo (TH.TyConI (TH.TySynD typeName [] typeSynonym)) = do
  forest <- typeGraphHelper typeSynonym
  return [Tree.Node typeName forest]
graphFromTypeInfo (TH.TyConI (TH.NewtypeD [] typeName [] Nothing ctor _)) = do
  forest <- concat <$> mapM typeGraphHelper (snd $ destructureCtor ctor)
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
typeGraphHelper (TH.TupleT 2) = return []   -- Storable only implemented for 2-tuples
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

-- Collect all bindings in dependency order
collectBindings :: TH.Q [Binding]
collectBindings = buildTypeGraph >>= go where
  go (Tree.Node typeName children) = do
    binding <- bindingFromName typeName
    childBindings <- concat <$> mapM go children
    return $ childBindings ++ [binding]

-- Unfold type application: F[A,B] -> (F, [A,B])
unfoldApp :: TypeBinding -> (TypeBinding, [TypeBinding])
unfoldApp (App a b) = second (++ [b]) (unfoldApp a)
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
    _ -> [classDecl, methodDecl] ++ map (indentation ++) methodBody where
      methodDecl = [i|def __class_getitem__(cls, type_args: #{typeArgsHint}) -> type:|]
      typeArgsHint :: String = [i|tuple[#{intercalate ", " (replicate arity "type")}]|]
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

offsetName :: Int -> TH.Name
offsetName i = TH.mkName ("offset" ++ show i)

fieldName :: Int -> TH.Name
fieldName i = TH.mkName ("field" ++ show i)

offsetDecls :: [TH.Type] -> TH.Q [TH.Dec]
offsetDecls [] = return []
offsetDecls types = (++) <$> [d| offset0 = 0 |] <*> zipWithM offsetDecl [1..] typePairs where
  offsetDecl i (prevType, currType) = let
    prevOffset = return $ TH.VarE $ offsetName (i - 1)
    currOffset = return $ TH.VarP $ offsetName i
    offsetDef = [| alignOffsetUp ($prevOffset + sizeOf @($prevType)) (alignment @($currType)) |]
    in head <$> [d| $currOffset = $offsetDef |]
  typePairs = zipWith (curry $ join bimap pure) types (tail types)

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

storableForData :: TH.Name -> [TH.Type] -> [(TH.Name, [TH.Type])] -> TH.Q TH.Dec
storableForData typeName typeParams ctors
  | kindOnly = TH.InstanceD Nothing constraints <$> storable <*> (fmap concat . sequence)
    [[d| alignment _ = alignment @F.CInt |], [d| sizeOf _ = sizeOf @F.CInt |], peekImpl, pokeImpl]
  | otherwise = TH.InstanceD Nothing constraints <$> storable <*> decls
  where
    decls = mapM (head <$>) [alignmentImpl, sizeOfImpl, peekImpl, pokeImpl]
    storable = TH.AppT (TH.ConT ''F.Storable) <$> self
    constraints = map (TH.AppT $ TH.ConT ''F.Storable) typeParams

    alignmentImpl = [d| alignment _ = max (alignment @F.CInt) (alignment @(F.Ptr ())) |]
    sizeOfImpl =
      [d| sizeOf _ = alignOffsetUp ($unionOffset + sizeOf @(F.Ptr ())) (alignment @($self)) |]
    self = pure $ foldl TH.AppT (TH.ConT typeName) typeParams

    peekImpl = [d|
      peek ptr = do
        kind :: F.CInt <- F.peekByteOff ptr 0
        unionPtr <- $(bool [| F.peekByteOff ptr $unionOffset |] [| return undefined |] kindOnly)
        $(TH.CaseE <$> [|kind|] <*> zipWithM peekMatchStmt [0..] ctors) |]

    pokeImpl = [d|
      poke ptr self = do
        (kind, unionPtr) <- $(TH.CaseE <$> [|self|] <*> zipWithM pokeMatchStmt [0..] ctors)
        F.pokeByteOff ptr 0 kind
        $(bool [| F.pokeByteOff ptr $unionOffset unionPtr |] [| return () |] kindOnly) |]

    unionOffset = [| alignOffsetUp (sizeOf @F.CInt) (alignment @(F.Ptr ())) |]
    kindOnly = all (null . snd) ctors

instantiateStorable :: TH.Name -> TH.Q TH.Dec
instantiateStorable typeName = TH.reify typeName >>= \case
  TH.TyConI (TH.DataD [] _ typeParamBinds Nothing ctors _) ->
    storableForData typeName typeParams (map destructureCtor ctors)
    where typeParams = map (TH.VarT . typeParamName) typeParamBinds
  TH.TyConI (TH.TySynD _ [] typeSynonym) -> undefined
  TH.TyConI (TH.NewtypeD [] _ [] Nothing ctor _) -> undefined
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

dbg :: TH.Q TH.Exp
-- dbg = stringify . TH.pprint . (\(TH.ClassI (TH.ClassD _ _ _ [] decs)  _) -> decs) <$> TH.reify ''F.Storable
dbg = stringify . TH.pprint <$> instantiateStorable ''Epsilon
-- dbg = stringify . generateBindings <$> collectBindings
-- dbg = stringify . show <$> inst
  where
        -- inst = [d|instance F.Storable a => F.Storable [a] where alignment _ = undefined|]
        stringify = TH.LitE . TH.StringL
-- dbg = stringify . show . instanceDecls <$> TH.reify ''F.Storable
--   where instanceDecls (TH.ClassI _ ((TH.InstanceD _ _ type' decls):_)) =
--           (TH.pprint type', show decls)
--         instanceDecls _ = undefined
--         stringify = TH.LitE . TH.StringL