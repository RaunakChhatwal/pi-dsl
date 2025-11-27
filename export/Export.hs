{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Bindings (implStorable, alignOffsetUp, buildDeclOrder, sizeOf, alignment, exportFunction)
import Syntax(Epsilon, Term, Type)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes, fromJust)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import PrettyPrint (ppr)
import Environment (Env, Err, runTcMonad)
import TypeCheck (inferType)
import Control.Monad (join)

instance F.Storable Integer where
  alignment _ = alignment @Int
  sizeOf _ = sizeOf @Int
  peek ptr = fromIntegral @Int <$> F.peekByteOff ptr 0
  poke ptr i = F.pokeByteOff ptr 0 (fromIntegral i :: Int)

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

addLength :: F.Ptr (Int, String) -> IO Int
addLength ptr = do
  (n, str) <- F.peek ptr
  return $ n + fromIntegral (length str)

foreign export ccall "add_length" addLength :: F.Ptr (Int, String) -> IO Int

concatInt :: F.Ptr (String, Int) -> IO (F.Ptr String)
concatInt ptr = do
  (str, int) <- F.peek ptr
  res <- F.malloc
  F.poke res (str ++ show int)
  return res

foreign export ccall "concat_int" concatInt :: F.Ptr (String, Int) -> IO (F.Ptr String)

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

sumTrue :: F.Ptr [(Bool, Int)] -> IO Int
sumTrue ptr = sum . map snd . filter fst <$> F.peek ptr

foreign export ccall "sum_true" sumTrue :: F.Ptr [(Bool, Int)] -> IO Int

$(pure . fromJust <$> implStorable ''Either)
$(catMaybes <$> (mapM implStorable =<< buildDeclOrder ''Env))

$(join $ exportFunction "ppr_term" <$> sequence [[t|Term|]] <*> [t|String|] <*> [| return . ppr |])

$(join $ exportFunction "infer_type"
    <$> sequence [[t|Env|], [t|Term|]]
    <*> [t|Either String Type|]
    <*> [|\env term -> runTcMonad env (inferType term)|])