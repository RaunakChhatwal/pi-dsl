module TypeCheck where

import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Data.List (intercalate)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal (equate, whnf)
import PrettyPrint (D(DS, DD), ppr)
import Syntax (Entry(Data, Decl), Level(Succ), Term(..), TermName, Type, maxLevel)
import Inductive (checkDataTypeDecl, synthesizeRecursorType)

-- Check that a term is a type and return its universe level
ensureType :: Term -> TcMonad Level
ensureType term = inferType term >>= whnf >>= \case
  Sort u -> return u
  type' -> Env.err [DS "Expected type but got", DD term, DS "with type", DD type']

-- Infer the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> Env.lookUpType var

  Sort level -> return $ Sort $ Succ level

  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    paramSortLevel <- ensureType paramType
    returnSortLevel <- Env.addLocal paramName paramType $ ensureType returnType
    return $ Sort $ maxLevel paramSortLevel returnSortLevel

  App func arg -> do
    funcType <- inferType func
    whnf funcType >>= \case
      Pi paramType bind -> do
        checkType arg paramType
        return $ Unbound.instantiate bind [arg]
      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann term type' -> do
    _ <- ensureType type'
    checkType term type'
    return type'

  DataType typeName -> Env.lookUpTypeOfDataType typeName

  Ctor typeName ctorName -> Env.lookUpCtor (typeName, ctorName)

  Rec typeName -> synthesizeRecursorType typeName

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that a term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType term type' = traceM "checkType" [ppr term, ppr type'] (const "") $
  case term of
    Lam bodyBind -> whnf type' >>= \case
      Pi paramType typeBind -> do
        -- unbind the variables in the lambda expression and pi type
        (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBind typeBind
        -- check the type of the body of the lambda expression
        Env.addLocal var paramType $ checkType body returnType
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD type']

    _ -> flip equate type' =<< inferType term

-- Verify that a term has no free variables
checkClosed :: Term -> TcMonad ()
checkClosed term = unless (null freeVars) $
  Env.err [DS "Expected closed expressions as input but found free variables:",
           DS $ intercalate ", " $ map Unbound.name2String freeVars]
  where freeVars :: [TermName] = Unbound.toListOf Unbound.fv term

-- Type-check a single top-level entry
tcEntry :: Entry -> TcMonad ()
tcEntry entry = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl _ type' term -> do
    checkClosed type'
    _ <- ensureType type'
    checkClosed term
    checkType term type'
  Data typeName typeSignature ctors -> do
    checkClosed typeSignature
    mapM_ (checkClosed . snd) ctors
    checkDataTypeDecl typeName typeSignature ctors

-- Run a computation with entries added to the environment
withEntries :: [Entry] -> TcMonad a -> TcMonad a
withEntries [] monad = monad
withEntries (entry : rest) monad = tcEntry entry >> case entry of
  Decl var type' def -> Env.addDecl var type' def (withEntries rest monad)
  Data name params ctors -> Env.addDataType name params ctors (withEntries rest monad)

-- Type-check a list of top-level entries
tcEntries :: [Entry] -> TcMonad ()
tcEntries = flip withEntries $ return ()
