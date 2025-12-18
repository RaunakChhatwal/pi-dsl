{- pi-forall language -}

-- | Utilities for managing a typechecking context.
module Environment where

import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader(local), asks, ReaderT(runReaderT))
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe)
import PrettyPrint (SourcePos, render, D(..), Disp(..), Doc, ppr)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), nest, sep, text, vcat)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Data.Bifunctor (first)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import Control.Arrow ((&&&))

data Trace = Invoc String [String] | Event String | Result String

traceM :: String -> [String] -> (a -> String) -> TcMonad a -> TcMonad a
traceM funcName args toStr monad = do
  S.yield $ Invoc funcName args
  result <- monad
  S.yield $ Result $ toStr result
  return result

-- | The type checking Monad includes a reader (for the
-- environment), freshness state (for supporting locally-nameless
-- representations), error (for error reporting), and IO
-- (for e.g.  warning messages).
type TcMonad = Stream (Of Trace) (Unbound.FreshMT (ReaderT Env (Except Err)))

instance Unbound.Fresh TcMonad where
  fresh = lift . Unbound.fresh

runTcMonad :: TcMonad a -> Maybe String
runTcMonad stream = fst $ traceTcMonad stream
  -- first ppr $ runExcept $ runReaderT (Unbound.runFreshMT (S.effects stream)) emptyEnv

emptyEnv :: Env
emptyEnv = Env [] 0 []

traceTcMonad :: TcMonad a -> (Maybe String, [Trace])
traceTcMonad = go [] where
  go traces stream = case runExcept $ runReaderT (Unbound.runFreshMT $ S.next stream) emptyEnv of
    Left error -> (Just $ ppr error, reverse traces)
    Right (Left _) -> (Nothing, reverse traces)
    Right (Right (trace, rest)) -> go (trace : traces) rest

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations
    ctx :: [Entry],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations: it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [TypeDecl]
  }


instance Disp Env where
  disp :: Env -> Doc
  disp e = vcat [disp decl | decl <- ctx e]
  debugDisp :: Env -> Doc
  debugDisp e = vcat [debugDisp decl | decl <- ctx e]

-- | Find a name's user supplied type signature.
lookupHint :: (MonadReader Env m) => TName -> m (Maybe TypeDecl)
lookupHint v = do
  hints <- asks hints
  return $ listToMaybe [ sig | sig <- hints, v == declName sig]

-- | Find a name's type in the context.
lookupTyMaybe ::
  (MonadReader Env m) =>
  TName ->
  m (Maybe TypeDecl)
lookupTyMaybe v = do
  ctx <- asks ctx
  return $ go ctx where
    go [] = Nothing
    go (Decl sig : ctx)
      | v == declName sig = Just sig
      | otherwise = go ctx
    go (_ : ctx) = go ctx


-- | Find the type of a name specified in the context
-- throwing an error if the name doesn't exist
lookupTy ::
  TName -> TcMonad TypeDecl
lookupTy v =
  do
    x <- lookupTyMaybe v
    gamma <- getLocalCtx
    case x of
      Just res -> return res
      Nothing ->
        err
          [ DS ("The variable " ++ show v ++ " was not found."),
            DS "in context",
            DD gamma
          ]

-- | Find a name's def in the context.
lookupDef :: TName -> TcMonad (Maybe Term)
lookupDef v = do
  ctx <- asks ctx
  return $ listToMaybe [a | Def v' a <- ctx, v == v']

-- | Find a type constructor in the context
lookupTCon :: TyConName -> TcMonad (Telescope, [CtorDef])
lookupTCon v = do
  g <- asks ctx
  scanGamma g
  where
    scanGamma [] = do
      currentEnv <- asks ctx
      err [ DS "The type constructor", DD v, DS "was not found.",
            DS "The current environment is", DD currentEnv ]
    scanGamma ((Data v' delta cs) : g) =
      if v == v'
        then return (delta, cs)
        else scanGamma g
    scanGamma (_ : g) = scanGamma g

-- Find a data constructor in the context, returns a list of all potential matches
lookupDConAll :: DataConName -> TcMonad [(TyConName, (Telescope, CtorDef))]
lookupDConAll ctorName = do
  ctxEntries <- asks ctx
  let filterCtors ctors = [ctor | ctor@(CtorDef ctorName' _ _) <- ctors, ctorName == ctorName']
  return $ concat $
    [(typeName,) . (delta,) <$> filterCtors ctors | Data typeName delta ctors <- ctxEntries]

-- | Given the name of a data constructor and the type that it should
-- construct, find the telescopes for its parameters and arguments.
-- Throws an error if the data constructor cannot be found for that type.
lookupDCon :: DataConName -> TyConName -> TcMonad (Telescope, Telescope)
lookupDCon ctorName typeName = do
  matches <- lookupDConAll ctorName
  let (typeNames, ctors) = unzip $ map (fst &&& snd . snd) matches
  case lookup typeName matches of
    Just (typeParams, CtorDef _ ctorParams _) -> return (typeParams, ctorParams)
    Nothing -> err $
      [ DS "Cannot find data constructor", DS ctorName, DS "for type", DD typeName,
        DS "Potential matches were:" ] ++ map DD typeNames ++ map DD ctors

-- | Extend the context with a new entry
extendCtx :: Entry -> TcMonad a -> TcMonad a
extendCtx d = local (\m@Env{ctx = cs} -> m {ctx = d : cs})

-- | Extend the context with a list of bindings
extendCtxs :: [Entry] -> TcMonad a -> TcMonad a
extendCtxs ds = local (\m@Env {ctx = cs} -> m {ctx = ds ++ cs})

-- | Extend the context with a list of bindings, marking them as "global"
extendCtxsGlobal :: (MonadReader Env m) => [Entry] -> m a -> m a
extendCtxsGlobal ds = local $ \m@Env {ctx = cs} -> m { ctx = ds ++ cs, globals = length (ds ++ cs) }

-- | Extend the context with a telescope
extendCtxTele :: Telescope -> TcMonad a -> TcMonad a
extendCtxTele tele m = foldr (extendCtx . Decl) m tele



-- | Get the complete current context
getCtx :: MonadReader Env m => m [Entry]
getCtx = asks ctx

-- | Get the prefix of the context that corresponds to local variables.
getLocalCtx :: MonadReader Env m => m [Entry]
getLocalCtx = do
  g <- asks ctx
  glen <- asks globals
  return $ take (length g - glen) g

-- | Add a type hint
extendHints :: (MonadReader Env m) => TypeDecl -> m a -> m a
extendHints h = local (\m@Env {hints = hs} -> m {hints = h : hs})

-- | An error that should be reported to the user
newtype Err = Err Doc

-- | Augment the error message with addition information
extendErr :: MonadError Err m => m a -> Doc -> m a
extendErr ma msg' =
  ma `catchError` \(Err msg) ->
    throwError $ Err (msg $$ msg')

instance Semigroup Err where
  (<>) :: Err -> Err -> Err
  (Err d1) <> (Err d2) = Err (d1 `mappend` d2)

instance Monoid Err where
  mempty :: Err
  mempty = Err mempty

dispErr :: (forall a. Disp a => a -> Doc) -> Err -> Doc
dispErr disp (Err msg) = msg

instance Disp Err where
  disp :: Err -> Doc
  disp = dispErr disp
  debugDisp = dispErr debugDisp

-- | Throw an error
err :: (Disp a, MonadError Err m, MonadReader Env m) => [a] -> m b
err d = throwError $ Err (sep $ map disp d)

-- checkStage ::
--   (MonadReader Env m, MonadError Err m) =>
--   Epsilon ->
--   m ()
-- checkStage ep1 = do
--   unless (ep1 <= Rel) $ do
--     err
--       [ DS "Cannot access",
--         DD ep1,
--         DS "variables in this context"
--       ]

-- withStage :: Epsilon -> TcMonad a -> TcMonad a
-- withStage Irr = extendCtx (Demote Rel)
-- withStage _ = id
