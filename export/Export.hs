{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Bindings (implStorable, alignOffsetUp, buildDeclOrder, sizeOf, alignment, exportFunction)
import Syntax(Entry(Data, Decl), Term, TName)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes, fromJust)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import PrettyPrint (ppr)
import Environment (Env, Err, runTcMonad, TcMonad, Trace, traceTcMonad)
import TypeCheck (checkType, inferType, tcEntry)
import Control.Monad (join, foldM_, void)
import qualified Environment as Env
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)

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

$(mapM (fmap fromJust . implStorable) [''Maybe, ''Either, ''Trace])
$(catMaybes <$> (mapM implStorable =<< buildDeclOrder ''Env))
$(pure . fromJust <$> implStorable ''Entry)

$(join $ exportFunction "bind"
  <$> sequence [[t|TName|], [t|Term|]]
  <*> [t|Unbound.Bind TName Term|]
  <*> [| \var body -> return (Unbound.bind var body) |])

$(join $ exportFunction "ppr_term" <$> sequence [[t|Term|]] <*> [t|String|] <*> [| return . ppr |])

tcEntries :: [Entry] -> TcMonad ()
tcEntries = foldr tcNextEntry (return ()) where
  tcNextEntry curr next = tcEntry curr >> case curr of
    Decl var type' def -> Env.addDecl var type' def next
    Data name params ctors -> Env.addDataType name params ctors next

$(join $ exportFunction "type_check"
  <$> sequence [[t| [Entry] |]] <*> [t| Maybe String |] <*> [| return . runTcMonad . tcEntries |])

$(join $ exportFunction "trace_type_check"
    <$> sequence [[t| [Entry] |]]
    <*> [t| (Maybe String, [Trace]) |]
    <*> [| return . traceTcMonad . tcEntries |])
