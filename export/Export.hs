{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Bindings (buildDeclOrder)
import FFI
import Syntax(Entry, Term, TermName, Type)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes, fromJust)
import PrettyPrint (ppr)
import Environment (Env, Trace, traceTcMonad)
import TypeCheck (checkClosed, checkType, ensureType, inferType, tcEntries, withEntries)
import Control.Monad (join, forM_, when)
import Data.Bifunctor (first)
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

-- Storable instance for Integer using Int representation
instance F.Storable Integer where
  alignment _ = alignment @Int
  sizeOf _ = sizeOf @Int
  peek ptr = fromIntegral @Int <$> F.peekByteOff ptr 0
  poke ptr i = F.pokeByteOff ptr 0 (fromIntegral i :: Int)

-- Storable instance for pairs with proper alignment
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

-- Storable instance for lists as length + heap-allocated data pointer
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

-- Generate Storable instances for Either and Trace
$(mapM (fmap fromJust . implStorable) [''Either, ''Trace])
-- Generate Storable instances for all types reachable from Env
$(catMaybes <$> (mapM implStorable =<< buildDeclOrder ''Env))
-- Generate Storable instance for Entry
$(pure . fromJust <$> implStorable ''Entry)

-- Free instance for Char (no-op, no allocations)
instance Free Char where
  freeInPlace _ = return ()

-- Free instance for Int (no-op, no allocations)
instance Free Int where
  freeInPlace _ = return ()

-- Free instance for Integer (no-op, no allocations)
instance Free Integer where
  freeInPlace _ = return ()

-- Free instance for pointers, treating them as owning their pointee
instance Free a => Free (F.Ptr a) where
  freeInPlace ptrToPtr = do
    p <- F.peek ptrToPtr
    when (p /= F.nullPtr) $ free p >> F.poke ptrToPtr F.nullPtr

-- Free instance for lists, freeing each element then the data array
instance (F.Storable a, Free a) => Free [a] where
  freeInPlace ptr = do
    let dataOffset = alignOffsetUp (sizeOf @F.CSize) (alignment @(F.Ptr a))
    dataPtr :: F.Ptr a <- F.peekByteOff ptr dataOffset

    when (dataPtr /= F.nullPtr) $ do
      len <- fromIntegral @F.CSize @Int <$> F.peekByteOff ptr 0
      forM_ [0 .. len - 1] $ \i -> do
        let elemPtr = (F.plusPtr dataPtr (i * sizeOf @a) :: F.Ptr a)
        freeInPlace elemPtr

      F.free dataPtr
      F.pokeByteOff ptr dataOffset (F.nullPtr :: F.Ptr a)
      F.pokeByteOff ptr 0 (0 :: F.CSize)

-- Free instance for pairs, freeing both components
instance (F.Storable a, F.Storable b, Free a, Free b) => Free (a, b) where
  freeInPlace ptr = do
    freeInPlace (F.castPtr ptr :: F.Ptr a)
    let bOffset = alignOffsetUp (sizeOf @a) (alignment @b)
    freeInPlace (F.plusPtr (F.castPtr ptr) bOffset :: F.Ptr b)

-- Generate Free instances for Either and Trace
$(mapM (fmap fromJust . implFree) [''Either, ''Trace])
-- Generate Free instances for all types reachable from Env
$(catMaybes <$> (mapM implFree =<< buildDeclOrder ''Env))

-- FFI export: create a binding from a name and term
$(join $ exportFunction "bind"
  <$> sequence [[t|TermName|], [t|Term|]]
  <*> [t|Unbound.Bind TermName Term|]
  <*> [| \var body -> return (Unbound.bind var body) |])

-- FFI export: destructure a binding into name and term
$(join $ exportFunction "unbind"
  <$> sequence [[t|Unbound.Bind TermName Term|]]
  <*> [t| (TermName, Term) |]
  <*> [| return . unsafeUnbind |])

-- FFI export: pretty print a term to string
$(join $ exportFunction "ppr_term" <$> sequence [[t|Term|]] <*> [t|String|] <*> [| return . ppr |])

-- Convert Either to Maybe, discarding Right's unit value
eitherToMaybe :: Either String () -> Maybe String
eitherToMaybe (Left error) = Just error
eitherToMaybe (Right _) = Nothing

-- FFI export: type check a list of entries, returning error and traces
$(join $ exportFunction "type_check"
    <$> sequence [[t| [Entry] |]]
    <*> [t| (Maybe String, [Trace]) |]
    <*> [| return . first eitherToMaybe . traceTcMonad . tcEntries |])

-- FFI export: infer the type of a term in the given environment
$(join $ exportFunction "infer_type"
  <$> sequence [[t| [Entry] |], [t|Term|]]
  <*> [t| (Either String Type, [Trace]) |]
  <*> [| \entries term ->
            return $ traceTcMonad $ withEntries entries $ checkClosed term >> inferType term |])

-- FFI export: check that a term has the expected type in the given environment
$(join $ exportFunction "check_type"
  <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]]
  <*> [t| (Maybe String, [Trace]) |]
  <*> [| \entries term type' ->
            return $ first eitherToMaybe $ traceTcMonad $ withEntries entries $ do
              checkClosed type'
              _ <- ensureType type'
              checkClosed term
              checkType term type' |])
