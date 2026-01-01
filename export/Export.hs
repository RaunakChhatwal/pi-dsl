{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Foreign qualified as F
import Foreign.C.Types qualified as F
import Bindings (buildDeclOrder)
import FFI
import Syntax(Entry(Data, Decl), Term, TermName, Type)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Maybe (catMaybes, fromJust)
import Data.Functor ((<&>))
import Data.String.Interpolate (i)
import PrettyPrint (ppr)
import Environment (Env, Err, runTcMonad, TcMonad, Trace, traceTcMonad)
import TypeCheck (checkType, ensureType, inferType, tcEntries, withEntries)
import Control.Monad (join, foldM_, forM_, void, when)
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

$(mapM (fmap fromJust . implStorable) [''Either, ''Trace])
$(catMaybes <$> (mapM implStorable =<< buildDeclOrder ''Env))
$(pure . fromJust <$> implStorable ''Entry)

instance Free Char where
  freeInPlace _ = return ()

instance Free Int where
  freeInPlace _ = return ()


instance Free Integer where
  freeInPlace _ = return ()

-- If a struct stores a pointer value, we treat it as owning its pointee.
-- This is useful for representations that contain `Ptr ...` fields.
instance Free a => Free (F.Ptr a) where
  freeInPlace ptrToPtr = do
    p <- F.peek ptrToPtr
    when (p /= F.nullPtr) $ free p >> F.poke ptrToPtr F.nullPtr

-- Mirror the `[a]` Storable layout defined in Export.hs:
-- struct { CSize length; Ptr a data; } where data is heap-allocated.
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

-- Mirror the `(a, b)` Storable layout defined in Export.hs.
instance (F.Storable a, F.Storable b, Free a, Free b) => Free (a, b) where
  freeInPlace ptr = do
    freeInPlace (F.castPtr ptr :: F.Ptr a)
    let bOffset = alignOffsetUp (sizeOf @a) (alignment @b)
    freeInPlace (F.plusPtr (F.castPtr ptr) bOffset :: F.Ptr b)

$(mapM (fmap fromJust . implFree) [''Either, ''Trace])
$(catMaybes <$> (mapM implFree =<< buildDeclOrder ''Env))

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
    <*> [t| (Maybe String, [Trace]) |]
    <*> [| return . first eitherToMaybe . traceTcMonad . tcEntries |])

$(join $ exportFunction "infer_type"
  <$> sequence [[t| [Entry] |], [t|Term|]]
  <*> [t| (Either Type String, [Trace]) |]
  <*> [| \entries term -> return . traceTcMonad $ withEntries entries (inferType term) |])

$(join $ exportFunction "check_type"
  <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]]
  <*> [t| (Maybe String, [Trace]) |]
  <*> [| \entries term type' -> do
            return . first eitherToMaybe . traceTcMonad $ withEntries entries $
              ensureType type' >> checkType term type' |])
