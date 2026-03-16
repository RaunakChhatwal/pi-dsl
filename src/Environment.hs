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
import Text.PrettyPrint qualified as PP
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

instance {-# OVERLAPPABLE #-} Monad m => MonadFail m where
  fail = error

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
    case runExcept $ runReaderT (runStateT (runStateT (Unbound.unFreshMT $ S.next stream) freshState) tcState) env of
      Left error -> (Left error, [])
      Right ((Left result, _), _) -> (Right result, [])
      Right ((Right (trace, restStream), newFreshState), newTcState) ->
        second (trace :) (go restStream newFreshState newTcState)
  emptyTcState = TcState 0 Map.empty Map.empty 0 Map.empty

data TcState = TcState {
  mvarCounter :: Int,
  mvarTypes :: Map Int Type,
  mvarSolutions :: Map Int Term,
  levelMVarCounter :: Int,
  levelMVarSolutions :: Map Int Level
}

-- Type checking environment with data types, declarations, and local bindings
data LocalContext = LocalContext {
  locals :: Map TermName Type,
  reverseOrderedLocals :: [(TermName, Type)]
}

data DataTypeInfo = DataTypeInfo {
  univParams :: [UnivParamName],
  signature :: Type,
  ctors :: [(CtorName, Type)],
  recursorUnivParam :: UnivParamName,
  recursorType :: Type
}

data DeclInfo = DeclInfo {
  univParams :: [UnivParamName],
  type' :: Type,
  body :: Term
}

data Env = Env {
  datatypes :: Map DataTypeName DataTypeInfo,
  decls :: Map String DeclInfo,
  localCtx :: LocalContext,
  dataTypeBeingDeclared :: Maybe (DataTypeName, ([UnivParamName], Type))
}

newLevelMVar :: TcMonad Level
newLevelMVar = do
  tcState <- State.get
  let id = tcState.levelMVarCounter
  State.put $ tcState { levelMVarCounter = id + 1 }
  return $ LMVar id

lookUpLevelMVarSolution :: Int -> TcMonad (Maybe Level)
lookUpLevelMVarSolution id = Map.lookup id . (.levelMVarSolutions) <$> State.get

instantiateLevelMVars :: Level -> TcMonad Level
instantiateLevelMVars = \case
  Succ level -> Succ <$> instantiateLevelMVars level
  Max level1 level2 -> Max <$> instantiateLevelMVars level1 <*> instantiateLevelMVars level2
  LMVar id -> lookUpLevelMVarSolution id >>= \case
    Just solvedLevel -> instantiateLevelMVars solvedLevel
    Nothing -> return $ LMVar id
  level -> return level

newMVar :: Type -> TcMonad Term
newMVar type' = do
  tcState <- State.get
  let id = tcState.mvarCounter
  ctx <- asks $ reverse . (.localCtx.reverseOrderedLocals)
  let mvarType = foldr (\(var, type') acc -> Pi Explicit type' (Unbound.bind var acc)) type' ctx
  State.put $ tcState {
    mvarCounter = id + 1,
    mvarTypes = Map.insert id mvarType tcState.mvarTypes
  }
  return $ foldl App (MVar id) $ map (LVar . fst) ctx

lookUpMVarType :: Int -> TcMonad Type
lookUpMVarType id = do
  tcState <- State.get
  case Map.lookup id tcState.mvarTypes of
    Just type' -> return type'
    Nothing -> throwError [i|Meta #{id} not found|]

lookUpMVarSolution :: Int -> TcMonad (Maybe Term)
lookUpMVarSolution id = Map.lookup id . (.mvarSolutions) <$> State.get

instantiateMVars :: Term -> TcMonad Term
instantiateMVars term = case term of
  Sort level -> Sort <$> instantiateLevelMVars level
  MVar id -> lookUpMVarSolution id >>= \case
    Just soln -> instantiateMVars soln
    Nothing -> return term
  Const constant levels -> Const constant <$> mapM instantiateLevelMVars levels
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
  MVar id2 -> id == id2
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
      err [DS [i|Meta variable solution for ?#{id} not closed:|], DD term]
  tcState <- State.get
  State.put $ tcState { mvarSolutions = Map.insert id term tcState.mvarSolutions }

levelMVarOccursCheck :: Int -> Level -> Bool
levelMVarOccursCheck id = \case
  Succ level -> levelMVarOccursCheck id level
  Max level1 level2 -> levelMVarOccursCheck id level1 || levelMVarOccursCheck id level2
  LMVar id2 -> id == id2
  _ -> False

assignLevelMVar :: Int -> Level -> TcMonad ()
assignLevelMVar id level = do
  level <- instantiateLevelMVars level
  if levelMVarOccursCheck id level
    then err [DS [i|Occurs check failed for ?u#{id} in|], DD level]
    else do
      tcState <- State.get
      State.put $ tcState { levelMVarSolutions = Map.insert id level tcState.levelMVarSolutions }

-- Look up a global declaration by name
lookUpDecl :: TC m => String -> m (Maybe DeclInfo)
lookUpDecl var = asks $ Map.lookup var . (.decls)

lookUpLVarType :: TermName -> TcMonad Type
lookUpLVarType name = asks (Map.lookup name . (.localCtx.locals)) >>= \case
  Just type' -> return type'
  Nothing -> throwError [i|Local variable #{ppr name} not found|]

-- Look up a data type definition by name
lookUpDataTypeInfo :: TC m => DataTypeName -> m DataTypeInfo
lookUpDataTypeInfo typeName = asks (Map.lookup typeName . (.datatypes)) >>= \case
  Just dataTypeInfo -> return dataTypeInfo
  Nothing -> asks (.dataTypeBeingDeclared) >>= \case
    Just _ -> throwError [i|Definition of data type #{typeName} not found|]
    Nothing -> throwError [i|Data type #{typeName} not found|]

-- Look up the type signature of a data type
lookUpDataType :: DataTypeName -> TcMonad ([UnivParamName], Type)
lookUpDataType typeName = asks (Map.lookup typeName . (.datatypes)) >>= \case
  Just dataTypeInfo -> return (dataTypeInfo.univParams, dataTypeInfo.signature)
  Nothing -> asks (.dataTypeBeingDeclared) >>= \case
    Just (name, info) | name == typeName -> return info
    _ -> throwError [i|Data type #{typeName} not found|]

-- Look up the type of a constructor
lookUpCtor :: TC m => DataTypeName -> CtorName -> m ([UnivParamName], Type)
lookUpCtor typeName ctorName = do
  DataTypeInfo univParams _ ctors _ _ <- lookUpDataTypeInfo typeName
  case lookup ctorName ctors of
    Nothing -> throwError [i|Constructor #{ctorName} not found in data type #{typeName}|]
    Just ctorType -> return (univParams, ctorType)

lookUpConst :: Const -> TcMonad ([UnivParamName], Type)
lookUpConst = \case
  GVar name -> lookUpDecl name >>= \case
    Just (DeclInfo univParams type' _) -> return (univParams, type')
    Nothing -> throwError [i|Global variable #{name} not found|]
  DataType typeName -> lookUpDataType typeName
  Ctor typeName ctorName -> lookUpCtor typeName ctorName
  Rec typeName -> do
    DataTypeInfo univParams _ _ recursorUnivParam recursorType <- lookUpDataTypeInfo typeName
    return (recursorUnivParam : univParams, recursorType)

-- Add a local variable to the environment
addLocal :: TermName -> Type -> TcMonad a -> TcMonad a
addLocal var type' monad = do
  LocalContext map list <- asks (.localCtx)
  newLocalContext <- case Map.lookup var map of
    Just _ -> throwError [i|Attempted to add local #{ppr var} when already exists|]
    Nothing -> return $ LocalContext (Map.insert var type' map) ((var, type') : list)
  local (\env -> env { localCtx = newLocalContext }) monad

-- Add a data type definition to the environment
addDataType :: DataTypeName -> DataTypeInfo -> TcMonad a -> TcMonad a
addDataType name dataTypeInfo monad = asks (Map.lookup name . (.datatypes)) >>= \case
  Nothing -> flip local monad $
    \env -> env { datatypes = Map.insert name dataTypeInfo env.datatypes }
  Just _ -> throwError [i|Name conflict when declaring data type #{name}|]

-- Add a global declaration to the environment
addDecl :: String -> DeclInfo -> TcMonad a -> TcMonad a
addDecl var declInfo monad = asks (Map.lookup var . (.decls)) >>= \case
  Nothing -> flip local monad $ \env -> env { decls = Map.insert var declInfo env.decls }
  Just _ -> throwError [i|Name conflict when declaring variable #{var}|]

lookUpRecursorType :: DataTypeName -> TcMonad Type
lookUpRecursorType typeName = asks (Map.lookup typeName . (.datatypes)) >>= \case
  Just dataTypeInfo -> return dataTypeInfo.recursorType
  Nothing -> throwError [i|Data type #{typeName} not found|]

-- | Throw an error
err :: TC m => [D] -> m b
err messages = throwError $ PP.render $ PP.sep $ flip map messages $ \case
  DS s -> PP.text s
  DD d -> PP.nest 2 $ disp d
