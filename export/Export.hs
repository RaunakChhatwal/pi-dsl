{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Foreign.C.String qualified as F
import Bindings (instantiateStorable, alignOffsetUp, buildDeclOrder, sizeOf, alignment)
import Syntax(Epsilon, Term)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes)

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

addLength :: F.Ptr (F.CInt, F.CString) -> IO F.CInt
addLength ptr = do
  (n, cstr) <- F.peek ptr
  str <- F.peekCString cstr
  return $ n + fromIntegral (length str)

foreign export ccall "add_length" addLength :: F.Ptr (F.CInt, F.CString) -> IO F.CInt

$(catMaybes <$> (mapM instantiateStorable =<< buildDeclOrder ''Term))