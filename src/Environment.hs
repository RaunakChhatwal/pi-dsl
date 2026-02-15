module Environment where

import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader(local), asks, ReaderT(runReaderT))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.State.Class qualified as State
import PrettyPrint (D(..), Disp(..), Doc, ppr)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), sep)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Bifunctor (second)
import Streaming (Stream, Of)
import Streaming.Prelude qualified as S
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.State (StateT(runStateT))

-- Trace events for debugging type checking execution
data Trace = Invoc String [String] | Event String | Result String

-- Wrap a computation with trace invocation and result events
traceM :: TC m => String -> [String] -> (a -> String) -> m a -> m a
traceM funcName args toStr monad = do
  yield $ Invoc funcName args
  result <- monad
  yield $ Result $ toStr result
  return result

-- The main type checking monad with tracing, fresh names, environment, and errors
type TcMonad = Stream (Of Trace) (Unbound.FreshMT (StateT TcState (ReaderT Env (Except Err))))

-- Fresh instance for Stream to generate fresh names
instance Unbound.Fresh m => Unbound.Fresh (Stream (Of Trace) m) where
  fresh = lift . Unbound.fresh

-- Type checking monad constraint with error handling, environment, and fresh names
class (MonadError Err m, MonadReader Env m, Unbound.Fresh m) => TC m where
  yield :: Trace -> m ()

-- TC instance for the main type checking monad
instance TC TcMonad where
  yield = S.yield

-- TC instance for MaybeT transformer
instance TC m => TC (MaybeT m) where
  yield = lift . yield

-- Run the type checking monad and collect traces
traceTcMonad :: TcMonad a -> (Either String a, [Trace])
traceTcMonad stream = go stream 0 emptyTcState where
  go stream freshState tcState =
    case runExcept $
        runReaderT
          (runStateT
            (runStateT
              (Unbound.unFreshMT $ S.next stream) freshState) tcState) emptyEnv of
      Left error -> (Left $ ppr error, [])
      Right ((Left result, _), _) -> (Right result, [])
      Right ((Right (trace, restStream), newFreshState), newTcState) ->
        second (trace :) (go restStream newFreshState newTcState)
  emptyEnv = Env Map.empty Map.empty emptyLocalContext Nothing

data TcState = TcState {
  mvarCounter :: Int,
  mvarTypes :: Map Int Type,
  mvarSolutions :: Map Int Term
}

emptyTcState :: TcState
emptyTcState = TcState 0 Map.empty Map.empty

-- Type checking environment with data types, declarations, and local bindings
data LocalContext = LocalContext {
  locals :: Map TermName Type,
  reverseOrderedLocals :: [(TermName, Type)]
}

emptyLocalContext :: LocalContext
emptyLocalContext = LocalContext Map.empty []

data Env = Env {
  datatypes :: Map DataTypeName (Type, [(CtorName, Type)]),
  decls :: Map String (Type, Term),
  localCtx :: LocalContext,
  dataTypeBeingDeclared :: Maybe (DataTypeName, Type)
}

newMVar :: Type -> TcMonad Term
newMVar type' = do
  tcState <- State.get
  let id = mvarCounter tcState
  ctx <- asks (reverse . reverseOrderedLocals . localCtx)
  let mvarType = foldr (\(var, type') acc -> Pi Explicit type' (Unbound.bind var acc)) type' ctx
  State.put $ tcState {
    mvarCounter = id + 1,
    mvarTypes = Map.insert id mvarType (mvarTypes tcState)
  }
  return $ foldl App (Var (Meta id)) (map (lVar . fst) ctx)

lookUpMVarType :: Int -> TcMonad Type
lookUpMVarType id = do
  tcState <- State.get
  case Map.lookup id (mvarTypes tcState) of
    Just type' -> return type'
    Nothing -> err [DS "Meta", DD id, DS "not found"]

lookUpMVarSolution :: Int -> TcMonad (Maybe Term)
lookUpMVarSolution id = Map.lookup id . mvarSolutions <$> State.get

assignMVar :: Int -> Term -> TcMonad ()
assignMVar id term = do
  tcState <- State.get
  State.put $ tcState { mvarSolutions = Map.insert id term (mvarSolutions tcState) }

-- Look up a global declaration by name
lookUpDecl :: TC m => String -> m (Maybe (Type, Term))
lookUpDecl var = asks (Map.lookup var . decls)

-- Look up the type of a variable
lookUpType :: Var -> TcMonad Type
lookUpType (Local name) = asks (Map.lookup name . locals . localCtx) >>= \case
  Just type' -> return type'
  Nothing -> err [DS "Local variable", DD name, DS "not found"]
lookUpType (Global name) = asks (Map.lookup name . decls) >>= \case
  Just (type', _) -> return type'
  Nothing -> err [DS "Global variable", DD name, DS "not found"]
lookUpType (Meta id) = lookUpMVarType id

-- Add a local variable to the environment
addLocal :: TermName -> Type -> TcMonad a -> TcMonad a
addLocal var type' monad = do
  LocalContext map list <- asks localCtx
  newLocalContext <- case Map.lookup var map of
    Just _ -> err [DS "Attempted to add local", DD var, DS "when already exists"]
    Nothing -> return $ LocalContext (Map.insert var type' map) ((var, type') : list)
  local (\env -> env { localCtx = newLocalContext }) monad

-- Add a data type definition to the environment
addDataType :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad a -> TcMonad a
addDataType name signature ctors monad = asks (Map.lookup name . datatypes) >>= \case
  Nothing -> flip local monad $
    \env@(Env datatypes _ _ _) -> env { datatypes = Map.insert name (signature, ctors) datatypes }
  Just _ -> err [DS "Name conflict when declaring data type", DD name]

-- Add a global declaration to the environment
addDecl :: String -> Type -> Term -> TcMonad a -> TcMonad a
addDecl var type' def monad = asks (Map.lookup var . decls) >>= \case
  Nothing -> flip local monad $
    \env@(Env _ decls _ _) -> env { decls = Map.insert var (type', def) decls }
  Just _ -> err [DS "Name conflict when declaring variable", DD var]

-- Look up a data type definition by name
lookUpDataType :: TC m => DataTypeName -> m (Type, [(CtorName, Type)])
lookUpDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just dataTypeDef -> return dataTypeDef
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just _ -> err [DS "Definition of data type", DD typeName, DS "not found"]
    Nothing -> err [DS "Data type", DD typeName, DS "not found"]

-- Look up the type signature of a data type
lookUpTypeOfDataType :: DataTypeName -> TcMonad Type
lookUpTypeOfDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just (typeOfDataType, _) -> return typeOfDataType
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just (name, typeOfDataType) | name == typeName -> return typeOfDataType
    _ -> err [DS "Data type", DD typeName, DS "not found"]

-- Look up the type of a constructor
lookUpCtor :: TC m => (DataTypeName, CtorName) -> m Type
lookUpCtor (typeName, ctorName) = do
  (_, ctorDefs) <- lookUpDataType typeName
  case lookup ctorName ctorDefs of
    Nothing -> err [DS "Constructor", DD ctorName, DS "not found in data type", DD typeName]
    Just ctorType -> return ctorType

-- | An error that should be reported to the user
newtype Err = Err Doc

-- | Augment the error message with addition information
extendErr :: MonadError Err m => m a -> Doc -> m a
extendErr ma msg' =
  ma `catchError` \(Err msg) ->
    throwError $ Err (msg $$ msg')

-- Semigroup instance for combining errors
instance Semigroup Err where
  (<>) :: Err -> Err -> Err
  (Err d1) <> (Err d2) = Err (d1 `mappend` d2)

-- Monoid instance for errors with empty as identity
instance Monoid Err where
  mempty :: Err
  mempty = Err mempty

-- Helper to display errors using a given display function
dispErr :: (forall a. Disp a => a -> Doc) -> Err -> Doc
dispErr _ (Err msg) = msg

-- Disp instance for pretty printing errors
instance Disp Err where
  disp :: Err -> Doc
  disp = dispErr disp
  debugDisp = dispErr debugDisp

-- | Throw an error
err :: TC m => Disp a => [a] -> m b
err d = throwError $ Err (sep $ map disp d)
