module Environment where

import Control.Monad.Except (MonadError(..), Except, runExcept)
import Control.Monad.Reader (MonadReader(local), asks, ReaderT(runReaderT))
import Control.Monad.Trans (lift)
import PrettyPrint (D(..), Disp(..), Doc, ppr)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), sep)
import qualified Unbound.Generics.LocallyNameless as Unbound
import Data.Bifunctor (first)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import Control.Arrow ((&&&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (find)
import Data.String.Interpolate (i)

data Trace = Invoc String [String] | Event String | Result String

traceM :: String -> [String] -> (a -> String) -> TcMonad a -> TcMonad a
traceM funcName args toStr monad = do
  S.yield $ Invoc funcName args
  result <- monad
  S.yield $ Result $ toStr result
  return result

type TcMonad = Stream (Of Trace) (Unbound.FreshMT (ReaderT Env (Except Err)))

instance Unbound.Fresh TcMonad where
  fresh = lift . Unbound.fresh

runTcMonad :: TcMonad a -> Either a String
runTcMonad stream = fst $ traceTcMonad stream

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty

traceTcMonad :: TcMonad a -> (Either a String, [Trace])
traceTcMonad = go [] where
  go traces stream = case runExcept $ runReaderT (Unbound.runFreshMT $ S.next stream) emptyEnv of
    Left error -> (Right $ ppr error, reverse traces)
    Right (Left result) -> (Left result, reverse traces)
    Right (Right (trace, rest)) -> go (trace : traces) rest

data Env = Env {
  datatypes :: Map DataTypeName (Type, [(CtorName, Type)]),
  decls :: Map String (Type, Term),
  locals :: Map TermName Type
}

justOr :: Disp e => [e] -> TcMonad (Maybe a) -> TcMonad a
justOr error m = m >>= \case
  Just a -> return a
  Nothing -> err error

lookupDecl :: String -> TcMonad (Maybe (Type, Term))
lookupDecl var = asks (Map.lookup var . decls)

lookupType :: Var -> TcMonad Type
lookupType (Local name) = asks (Map.lookup name . locals) >>= \case
  Just type' -> return type'
  Nothing -> err [DS"Local variable", DD name, DS "not found"]
lookupType (Global name) = asks (Map.lookup name . decls) >>= \case
  Just (type', _) -> return type'
  Nothing -> err [DS"Global variable", DD name, DS "not found"]

addLocal :: TermName -> Type -> TcMonad a -> TcMonad a
addLocal var type' = local $ \env@(Env _ _ locals) -> env { locals = Map.insert var type' locals }

addDataType :: DataTypeName -> Type -> [(CtorName, Type)] -> TcMonad a -> TcMonad a
addDataType name signature ctors monad = asks (Map.lookup name . datatypes) >>= \case
  Nothing -> flip local monad $
    \env@(Env datatypes _ _) -> env { datatypes = Map.insert name (signature, ctors) datatypes }
  Just _ -> err [DS "Name conflict when declaring data type", DD name]

addDecl :: String -> Type -> Term -> TcMonad a -> TcMonad a
addDecl var type' def monad = asks (Map.lookup var . decls) >>= \case
  Nothing -> flip local monad $
    \env@(Env _ decls _) -> env { decls = Map.insert var (type', def) decls }
  Just _ -> err [DS "Name conflict when declaring variable", DD var]

lookupDataType :: DataTypeName -> TcMonad (Type, [(CtorName, Type)])
lookupDataType typeName =
  justOr [DS "Data type", DD typeName, DS "not found"] $ asks (Map.lookup typeName . datatypes)

lookupCtor :: (DataTypeName, CtorName) -> TcMonad Type
lookupCtor (typeName, ctorName) = do
  (_, ctorDefs) <- lookupDataType typeName
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
err :: Disp a => [a] -> TcMonad b
err d = throwError $ Err (sep $ map disp d)
