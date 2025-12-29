{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Bindings (buildDeclOrder)
import FFI (implStorable, alignOffsetUp, sizeOf, alignment, exportFunction)
import Syntax(Entry(Data, Decl), Term, TermName, Type)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes, fromJust)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import PrettyPrint (ppr)
import Environment (Env, Err, runTcMonad, TcMonad, Trace, traceTcMonad)
import TypeCheck (checkType, ensureType, inferType, tcEntries, withEntries)
import Control.Monad (join, foldM_, void)
import qualified Environment as Env
import Control.Monad.Trans (liftIO)
import Data.List (intercalate)
import Data.Bifunctor (first)
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

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
  <$> sequence [[t|TermName|], [t|Term|]]
  <*> [t|Unbound.Bind TermName Term|]
  <*> [| \var body -> return (Unbound.bind var body) |])

$(join $ exportFunction "unbind"
  <$> sequence [[t|Unbound.Bind TermName Term|]]
  <*> [t| (TermName, Term) |]
  <*> [| return . unsafeUnbind |])

$(join $ exportFunction "ppr_term" <$> sequence [[t|Term|]] <*> [t|String|] <*> [| return . ppr |])

eitherToMaybe :: Either () String -> Maybe String
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right error) = Just error

$(join $ exportFunction "type_check"
  <$> sequence [[t| [Entry] |]]
  <*> [t| Maybe String |]
  <*> [| return . eitherToMaybe . runTcMonad . tcEntries |])

$(join $ exportFunction "trace_type_check"
    <$> sequence [[t| [Entry] |]]
    <*> [t| (Maybe String, [Trace]) |]
    <*> [| return . first eitherToMaybe . traceTcMonad . tcEntries |])

$(join $ exportFunction "infer_type"
  <$> sequence [[t| [Entry] |], [t|Term|]]
  <*> [t| Either Type String |]
  <*> [| \entries term -> return . runTcMonad $ withEntries entries (inferType term) |])

$(join $ exportFunction "check_type"
  <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]]
  <*> [t| Maybe String |]
  <*> [| \entries term type' -> do
            return . eitherToMaybe . runTcMonad $ withEntries entries $
              ensureType type' >> checkType term type' |])
