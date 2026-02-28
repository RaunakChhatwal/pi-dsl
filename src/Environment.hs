module Environment where

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader(local), asks, ReaderT(runReaderT))
import Control.Monad.State (StateT(runStateT))
import Control.Monad.State.Class qualified as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Bifunctor (second)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Streaming (Stream, Of)
import Streaming.Prelude qualified as S
import Text.PrettyPrint.HughesPJ (render, sep)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Bind qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound
import PrettyPrint
import Syntax

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
type TcMonad = Stream (Of Trace) (Unbound.FreshMT (StateT TcState (ReaderT Env (Except String))))

-- Fresh instance for Stream to generate fresh names
instance Unbound.Fresh m => Unbound.Fresh (Stream (Of Trace) m) where
  fresh = lift . Unbound.fresh

-- Type checking monad constraint with error handling, environment, and fresh names
class (MonadError String m, MonadReader Env m, Unbound.Fresh m) => TC m where
  yield :: Trace -> m ()

-- TC instance for the main type checking monad
instance TC TcMonad where
  yield = S.yield

-- TC instance for MaybeT transformer
instance TC m => TC (MaybeT m) where
  yield = lift . yield

-- Run the type checking monad and collect traces
traceTcMonad :: Env -> TcMonad a -> (Either String a, [Trace])
traceTcMonad env stream = go stream 0 emptyTcState where
  go stream freshState tcState =
    case runExcept $
        runReaderT
          (runStateT
            (runStateT
              (Unbound.unFreshMT $ S.next stream) freshState) tcState) env of
      Left error -> (Left error, [])
      Right ((Left result, _), _) -> (Right result, [])
      Right ((Right (trace, restStream), newFreshState), newTcState) ->
        second (trace :) (go restStream newFreshState newTcState)
  emptyTcState = TcState 0 Map.empty Map.empty

data TcState = TcState {
  mvarCounter :: Int,
  mvarTypes :: Map Int Type,
  mvarSolutions :: Map Int Term
}

-- Type checking environment with data types, declarations, and local bindings
data LocalContext = LocalContext {
  locals :: Map TermName Type,
  reverseOrderedLocals :: [(TermName, Type)]
}

data Env = Env {
  datatypes :: Map DataTypeName (Type, [(CtorName, Type)]),
  decls :: Map String (Type, Term),
  localCtx :: LocalContext,
  dataTypeBeingDeclared :: Maybe (DataTypeName, Type)
}

entriesToEnv :: [Entry] -> Env
entriesToEnv entries = Env {
  datatypes = Map.fromList
    [(typeName, (signature, ctors)) | Data typeName signature ctors <- entries],
  decls = Map.fromList [(var, (type', term)) | Decl var type' term <- entries],
  localCtx = LocalContext Map.empty [],
  dataTypeBeingDeclared = Nothing
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
    Nothing -> throwError [i|Meta #{id} not found|]

lookUpMVarSolution :: Int -> TcMonad (Maybe Term)
lookUpMVarSolution id = Map.lookup id . mvarSolutions <$> State.get

instantiateMVars :: Term -> TcMonad Term
instantiateMVars term = case term of
  Var (Meta id) -> lookUpMVarSolution id >>= \case
    Just soln -> instantiateMVars soln
    Nothing -> return term
  App func arg -> App <$> instantiateMVars func <*> instantiateMVars arg
  Lam binderInfo binder -> do
    (paramName, body) <- Unbound.unbind binder
    Lam binderInfo . Unbound.bind paramName <$> instantiateMVars body
  Pi binderInfo paramType binder -> do
    (paramName, returnType) <- Unbound.unbind binder
    Pi binderInfo <$> instantiateMVars paramType <*>
      (Unbound.bind paramName <$> instantiateMVars returnType)
  Ann term type' -> Ann <$> instantiateMVars term <*> instantiateMVars type'
  _ -> return term

mvarOccursCheck :: Int -> Term -> Bool
mvarOccursCheck id term = case term of
  Var (Meta id2) -> id == id2
  App func arg -> mvarOccursCheck id func || mvarOccursCheck id arg
  Lam _ (Unbound.B _ body) -> mvarOccursCheck id body
  Pi _ paramType (Unbound.B _ returnType) ->
    mvarOccursCheck id paramType || mvarOccursCheck id returnType
  Ann term type' -> mvarOccursCheck id term || mvarOccursCheck id type'
  _ -> False

assignMVar :: Int -> Term -> TcMonad ()
assignMVar id term = do
  term <- instantiateMVars term
  if mvarOccursCheck id term
    then err [DS [i|"Occurs check failed for ?#{id} in"|], DD term]
    else unless (null $ Unbound.toListOf @Term @TermName Unbound.fv term) $
      err [DS "Meta variable solution not closed:", DD term]
  tcState <- State.get
  State.put $ tcState { mvarSolutions = Map.insert id term $ mvarSolutions tcState }

-- Look up a global declaration by name
lookUpDecl :: TC m => String -> m (Maybe (Type, Term))
lookUpDecl var = asks (Map.lookup var . decls)

-- Look up the type of a variable
lookUpType :: Var -> TcMonad Type
lookUpType (Local name) = asks (Map.lookup name . locals . localCtx) >>= \case
  Just type' -> return type'
  Nothing -> throwError [i|Local variable #{ppr name} not found|]
lookUpType (Global name) = asks (Map.lookup name . decls) >>= \case
  Just (type', _) -> return type'
  Nothing -> throwError [i|Global variable #{name} not found|]
lookUpType (Meta id) = lookUpMVarType id

-- Add a local variable to the environment
addLocal :: TermName -> Type -> TcMonad a -> TcMonad a
addLocal var type' monad = do
  LocalContext map list <- asks localCtx
  newLocalContext <- case Map.lookup var map of
    Just _ -> throwError [i|Attempted to add local #{ppr var} when already exists|]
    Nothing -> return $ LocalContext (Map.insert var type' map) ((var, type') : list)
  local (\env -> env { localCtx = newLocalContext }) monad

-- Add a data type definition to the environment
addDataType :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad a -> TcMonad a
addDataType name signature ctors monad = asks (Map.lookup name . datatypes) >>= \case
  Nothing -> flip local monad $
    \env@(Env datatypes _ _ _) -> env { datatypes = Map.insert name (signature, ctors) datatypes }
  Just _ -> throwError [i|Name conflict when declaring data type #{name}|]

-- Add a global declaration to the environment
addDecl :: String -> Type -> Term -> TcMonad a -> TcMonad a
addDecl var type' def monad = asks (Map.lookup var . decls) >>= \case
  Nothing -> flip local monad $
    \env@(Env _ decls _ _) -> env { decls = Map.insert var (type', def) decls }
  Just _ -> throwError [i|Name conflict when declaring variable #{var}|]

-- Look up a data type definition by name
lookUpDataType :: TC m => DataTypeName -> m (Type, [(CtorName, Type)])
lookUpDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just dataTypeDef -> return dataTypeDef
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just _ -> throwError [i|Definition of data type #{typeName} not found|]
    Nothing -> throwError [i|Data type #{typeName} not found|]

-- Look up the type signature of a data type
lookUpTypeOfDataType :: DataTypeName -> TcMonad Type
lookUpTypeOfDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just (typeOfDataType, _) -> return typeOfDataType
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just (name, typeOfDataType) | name == typeName -> return typeOfDataType
    _ -> throwError [i|Data type #{typeName} not found|]

-- Look up the type of a constructor
lookUpCtor :: TC m => (DataTypeName, CtorName) -> m Type
lookUpCtor (typeName, ctorName) = do
  (_, ctorDefs) <- lookUpDataType typeName
  case lookup ctorName ctorDefs of
    Nothing -> throwError [i|Constructor #{ctorName} not found in data type #{typeName}|]
    Just ctorType -> return ctorType

-- | Throw an error
err :: TC m => Disp a => [a] -> m b
err d = throwError $ render $ sep $ map disp d
