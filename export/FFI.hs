module FFI where

import Control.Arrow ((&&&))
import Control.Monad (zipWithM, join, replicateM, when)
import Data.Bifunctor (second, bimap)
import Foreign qualified as F
import Foreign.C.Types qualified as F
import Language.Haskell.TH qualified as TH
import Data.String.Interpolate (i)

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
  offsetDecl i (prevType, currType) = head <$> [d| $currOffset = $offsetDef |] where
    offsetDef = [| alignOffsetUp ($prevOffset + sizeOf @($prevType)) (alignment @($currType)) |]
    currOffset = return $ TH.VarP $ offsetName i
    prevOffset = return $ TH.VarE $ offsetName (i - 1)
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

destructureCtor :: TH.Con -> (TH.Name, [TH.Type])
destructureCtor (TH.NormalC name params) = (name, map snd params)
destructureCtor (TH.RecC name params) = (name, [paramType | (_, _, paramType) <- params])
destructureCtor ctor = error [i|Constructor not implemented: #{TH.pprint ctor}|]

-- Extract name from TH type parameter
typeParamName :: TH.TyVarBndr a -> TH.Name
typeParamName (TH.PlainTV name _) = name
typeParamName (TH.KindedTV name _ _) = name

implStorable :: TH.Name -> TH.Q (Maybe TH.Dec)
implStorable typeName = TH.reify typeName >>= \case
  TH.TyConI (TH.DataD [] _ typeParamBinds Nothing ctors _) ->
    Just <$> storableForData typeName typeParams (map destructureCtor ctors)
    where typeParams = map (TH.VarT . typeParamName) typeParamBinds
  TH.TyConI (TH.TySynD _ [] _) -> return Nothing
  TH.TyConI (TH.NewtypeD [] _ [] Nothing ctor _) ->
    Just <$> storableForAlias typeName (destructureCtor ctor)
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Recursively free memory owned by an FFI-allocated C representation.
--
-- IMPORTANT OWNERSHIP NOTES
-- - `exportFunction` allocates the outer result pointer with `malloc`.
-- - Many `Storable` instances generated by `storableForData` allocate an
--   inner `unionPtr` with `mallocBytes`.
-- - Some `Storable` instances (e.g. `[a]` in Export.hs) allocate additional
--   buffers (e.g. `mallocArray` for list data).
--
-- The `Free` typeclass lets you describe how to free those nested allocations
-- given a pointer to the C representation. The default instance is a no-op for
-- types that don't own any nested allocations.
class F.Storable a => Free a where
  -- Free allocations owned *by the value stored at this pointer*.
  -- This must NOT free the pointer itself.
  freeInPlace :: F.Ptr a -> IO ()

  -- Free allocations owned by the value, then free the pointer itself.
  free :: F.Ptr a -> IO ()
  free ptr = when (ptr /= F.nullPtr) $ freeInPlace ptr >> F.free ptr

freeMatchStmt :: Integer -> (TH.Name, [TH.Type]) -> TH.Q TH.Match
freeMatchStmt kind (_, paramTypes) = do
  offsetDecls <- map (TH.LetS . pure) <$> offsetDecls paramTypes

  let freeFieldStmt i paramType = TH.NoBindS <$> [| freeInPlace $fieldPtr |] where
        fieldPtr = TH.SigE
          <$> [| F.plusPtr (F.castPtr unionPtr) $offset |] <*> [t| F.Ptr $(pure paramType) |]
        offset = pure $ TH.VarE (offsetName i)
  freeFieldStmts <- zipWithM freeFieldStmt [0..] paramTypes

  returnStmt <- TH.NoBindS <$> [| return () |]
  let matchBody = TH.NormalB $ TH.DoE Nothing (offsetDecls ++ freeFieldStmts ++ [returnStmt])
  let kindPattern = TH.LitP $ TH.IntegerL kind
  return $ TH.Match kindPattern matchBody []

freeForData :: TH.Name -> [TH.Type] -> [(TH.Name, [TH.Type])] -> TH.Q TH.Dec
freeForData typeName typeParams ctors =
  TH.InstanceD Nothing constraints <$> freeSelf <*> sequence [freeInPlaceImpl] where
  constraints = map (TH.AppT $ TH.ConT ''Free) typeParams

  freeSelf = pure $ TH.AppT (TH.ConT ''Free) self
  self = foldl TH.AppT (TH.ConT typeName) typeParams

  freeInPlaceImpl = implMethod 'freeInPlace [ptrArg] <$> [| do
    kind :: F.CInt <- F.peekByteOff $ptr 0
    unionPtr :: F.Ptr () <- F.peekByteOff $ptr $unionOffset
    $(TH.CaseE <$> [| kind |] <*> zipWithM freeMatchStmt [0..] ctors)
    when (unionPtr /= F.nullPtr) $ F.free unionPtr
    F.pokeByteOff $ptr $unionOffset $ F.nullPtr @() |]

  (ptrArg, ptr) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "ptr")
  unionOffset = [| alignOffsetUp (sizeOf @F.CInt) (alignment @(F.Ptr ())) |]

freeForAlias :: TH.Name -> (TH.Name, [TH.Type]) -> TH.Q TH.Dec
freeForAlias typeName (_, [wrappedType]) =
  TH.InstanceD Nothing [constraint] freeSelf <$> sequence [freeInPlaceImpl] where
  constraint = TH.AppT (TH.ConT ''Free) wrappedType
  freeSelf = TH.AppT (TH.ConT ''Free) (TH.ConT typeName)

  freeInPlaceImpl = implMethod 'freeInPlace [ptrArg] <$>
    [| freeInPlace (F.castPtr $ptr :: F.Ptr $(pure wrappedType)) |]
  (ptrArg, ptr) = (TH.VarP &&& pure . TH.VarE) (TH.mkName "ptr")
freeForAlias _ _ = undefined

implFree :: TH.Name -> TH.Q (Maybe TH.Dec)
implFree typeName = TH.reify typeName >>= \case
  TH.TyConI (TH.DataD [] _ typeParamBinds Nothing ctors _) ->
    Just <$> freeForData typeName typeParams (map destructureCtor ctors)
    where typeParams = map (TH.VarT . typeParamName) typeParamBinds
  TH.TyConI (TH.TySynD _ [] _typeSynonym) -> return Nothing
  TH.TyConI (TH.NewtypeD [] _ [] Nothing ctor _) ->
    Just <$> freeForAlias typeName (destructureCtor ctor)
  typeInfo -> error [i|Type info not implemented: #{TH.pprint typeInfo}|]

-- Generate FFI export wrapper to free an FFI-allocated pointer (and any nested allocations).
--
-- This is intended to be paired with `exportFunction`, which returns a freshly `malloc`'d
-- pointer. The generated wrapper calls `free` from the `Free` typeclass, which:
--   1) recursively frees nested allocations via `freeInPlace`, then
--   2) frees the outer pointer itself.
exportFree :: String -> TH.Type -> TH.Q [TH.Dec]
exportFree exportName freedType = do
  name <- TH.newName exportName

  -- ffi wrapper signature: F.Ptr a -> IO ()
  wrapperType <- [t|F.Ptr $(pure freedType) -> IO ()|]
  let signature = TH.SigD name wrapperType

  -- ffi wrapper definition
  ptrName <- TH.newName "ptr"
  let body = TH.NormalB (TH.AppE (TH.VarE 'free) (TH.VarE ptrName))
  let definition = TH.FunD name [TH.Clause [TH.VarP ptrName] body []]

  -- ffi export declaration
  let exportDecl = TH.ForeignD (TH.ExportF TH.CCall exportName name wrapperType)
  return [signature, definition, exportDecl]

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
  -- declare corresponding free function
  freeFunctionDecls <- exportFree [i|free_#{exportName}_result|] returnType
  return $ [signature, definition, exportDecl] ++ freeFunctionDecls
