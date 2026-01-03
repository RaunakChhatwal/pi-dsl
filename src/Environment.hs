module Environment where

import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader(local), asks, ReaderT(runReaderT))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import PrettyPrint (D(..), Disp(..), Doc, ppr)
import Syntax (CtorName, DataTypeName, Term(..), TermName, Type, Var(Global, Local))
import Text.PrettyPrint.HughesPJ (($$), sep)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Data.Bifunctor (first, second)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (find)
import Data.String.Interpolate (i)
import Control.Monad.State (StateT(runStateT))

data Trace = Invoc String [String] | Event String | Result String

traceM :: TC m => String -> [String] -> (a -> String) -> m a -> m a
traceM funcName args toStr monad = do
  yield $ Invoc funcName args
  result <- monad
  yield $ Result $ toStr result
  return result

type TcMonad = Stream (Of Trace) (Unbound.FreshMT (ReaderT Env (Except Err)))

instance Unbound.Fresh m => Unbound.Fresh (Stream (Of Trace) m) where
  fresh = lift . Unbound.fresh

class (MonadError Err m, MonadReader Env m, Unbound.Fresh m) => TC m where
  yield :: Trace -> m ()

instance TC TcMonad where
  yield = S.yield

instance TC m => TC (MaybeT m) where
  yield = lift . yield

traceTcMonad :: TcMonad a -> (Either String a, [Trace])
traceTcMonad stream = go stream 0 where
  go stream depth =
    case runExcept $ runReaderT (runStateT (Unbound.unFreshMT $ S.next stream) depth) emptyEnv of
      Left error -> (Left $ ppr error, [])
      Right (Left result, _) -> (Right result, [])
      Right (Right (trace, restStream), newDepth) -> second (trace :) (go restStream newDepth)
  emptyEnv = Env Map.empty Map.empty Map.empty Nothing

data Env = Env {
  datatypes :: Map DataTypeName (Type, [(CtorName, Type)]),
  decls :: Map String (Type, Term),
  locals :: Map TermName Type,
  dataTypeBeingDeclared :: Maybe (DataTypeName, Type)
}

lookUpDecl :: TC m => String -> m (Maybe (Type, Term))
lookUpDecl var = asks (Map.lookup var . decls)

lookUpType :: Var -> TcMonad Type
lookUpType (Local name) = asks (Map.lookup name . locals) >>= \case
  Just type' -> return type'
  Nothing -> err [DS"Local variable", DD name, DS "not found"]
lookUpType (Global name) = asks (Map.lookup name . decls) >>= \case
  Just (type', _) -> return type'
  Nothing -> err [DS"Global variable", DD name, DS "not found"]

addLocal :: TermName -> Type -> TcMonad a -> TcMonad a
addLocal var type' = local $ \env@(Env _ _ locals _) -> env { locals = Map.insert var type' locals }

addDataType :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad a -> TcMonad a
addDataType name signature ctors monad = asks (Map.lookup name . datatypes) >>= \case
  Nothing -> flip local monad $
    \env@(Env datatypes _ _ _) -> env { datatypes = Map.insert name (signature, ctors) datatypes }
  Just _ -> err [DS "Name conflict when declaring data type", DD name]

addDecl :: String -> Type -> Term -> TcMonad a -> TcMonad a
addDecl var type' def monad = asks (Map.lookup var . decls) >>= \case
  Nothing -> flip local monad $
    \env@(Env _ decls _ _) -> env { decls = Map.insert var (type', def) decls }
  Just _ -> err [DS "Name conflict when declaring variable", DD var]

lookUpDataType :: TC m => DataTypeName -> m (Type, [(CtorName, Type)])
lookUpDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just dataTypeDef -> return dataTypeDef
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just _ -> err [DS "Definition of data type", DD typeName, DS "not found"]
    Nothing -> err [DS "Data type", DD typeName, DS "not found"]

lookUpTypeOfDataType :: DataTypeName -> TcMonad Type
lookUpTypeOfDataType typeName = asks (Map.lookup typeName . datatypes) >>= \case
  Just (typeOfDataType, _) -> return typeOfDataType
  Nothing -> asks dataTypeBeingDeclared >>= \case
    Just (name, typeOfDataType) | name == typeName -> return typeOfDataType
    _ -> err [DS "Data type", DD typeName, DS "not found"]

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
err :: TC m => Disp a => [a] -> m b
err d = throwError $ Err (sep $ map disp d)
