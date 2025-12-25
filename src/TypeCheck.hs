module TypeCheck where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (forM_, when, (<=<), (>=>), forM)
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal qualified
import PrettyPrint (D(DS, DD), Disp (disp), ppr)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), render)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.String.Interpolate (i)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))

hole :: TermName
hole = Unbound.string2Name "_"

addParam :: (TermName, Type) -> Type -> Type
addParam (paramName, paramType) = Pi paramType . Unbound.bind paramName

motiveFromTypeSignature :: DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromTypeSignature typeName paramNames (Pi paramType bind) = do
  (paramName, returnType) <- Unbound.unbind bind
  addParam (paramName, paramType) <$>
    motiveFromTypeSignature typeName (paramName : paramNames) returnType
motiveFromTypeSignature typeName paramNames _ = do
  let fullyApplied = foldl App (DataType typeName) $ map Var (reverse paramNames)
  return $ addParam (hole, fullyApplied) TyType

motive :: TermName
motive = Unbound.string2Name "motive"

motiveFromCtorParam ::
  DataTypeName -> [TermName] -> [Term] -> TermName -> Type -> TcMonad (Maybe Type)
motiveFromCtorParam self paramNames args name = Equal.whnf >=> \case
  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    fmap (addParam (paramName, paramType)) <$>
      motiveFromCtorParam self (paramName : paramNames) [] name returnType
  App f arg -> motiveFromCtorParam self paramNames (arg : args) name f
  DataType typeName | typeName == self ->
    return $ Just $ App motiveApplied paramApplied where
    motiveApplied = foldl App (Var motive) args
    paramApplied = foldl App (Var name) $ map Var (reverse paramNames)
  _ -> return Nothing

motiveFromCtorReturnType :: Term -> [Term] -> Type -> TcMonad Type
motiveFromCtorReturnType ctorApplied args = Equal.whnf >=> \case
  App f arg -> motiveFromCtorReturnType ctorApplied (arg : args) f
  _ -> return $ App (foldl App (Var motive) args) ctorApplied

recursorCaseFromCtor :: CtorName -> DataTypeName -> Term -> Type -> TcMonad Type
recursorCaseFromCtor ctorName typeName ctorApplied = Equal.whnf >=> \case
  (Pi paramType bind) -> do
    (paramName, returnType) <- Unbound.unbind bind
    rest <- recursorCaseFromCtor ctorName typeName (App ctorApplied $ Var paramName) returnType
    motiveFromCtorParam typeName [] [] paramName paramType <&> \case
      Nothing -> addParam (paramName, paramType) rest
      Just motive -> addParam (paramName, paramType) $ addParam (hole, motive) rest
  returnType -> motiveFromCtorReturnType ctorApplied [] returnType

motiveFromSelf :: DataTypeName -> [TermName] -> Type -> TcMonad Type
motiveFromSelf typeName paramNames (Pi paramType bind) = do -- TODO: whnf?
  (paramName, returnType) <- Unbound.unbind bind
  addParam (paramName, paramType) <$>
    motiveFromSelf typeName (paramName : paramNames) returnType
motiveFromSelf typeName paramNames _ = do
  let self = Unbound.string2Name "self"
  let selfApplied = foldl App (DataType typeName) $ map Var $ reverse paramNames
  let motiveApplied = App (foldl App (Var motive) $ map Var $ reverse paramNames) (Var self)
  return $ addParam (self, selfApplied) motiveApplied

-- Infer/synthesize the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var var -> Env.lookupType var

  TyType -> return TyType

  Pi paramType bind -> do
    (paramName, returnType) <- Unbound.unbind bind
    checkType paramType TyType
    Env.addLocal paramName paramType $ checkType returnType TyType
    return TyType

  App func arg -> do
    funcType <- inferType func
    Equal.whnf funcType >>= \case
      Pi paramType bind -> do
        checkType arg paramType
        return $ Unbound.instantiate bind [arg]
      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann term type' -> do
    checkType type' TyType
    checkType term type'
    return type'

  DataType typeName -> do
    typeSignature <- fst <$> Env.lookupDataType typeName
    checkType typeSignature TyType
    return typeSignature

  Ctor typeName ctorName -> do
    ctorType <- Env.lookupCtor (typeName, ctorName)
    checkType ctorType TyType
    return ctorType

  Rec typeName -> do
    (signature, ctorDefs) <- Env.lookupDataType typeName
    motiveType <- motiveFromTypeSignature typeName [] signature
    cases <- forM ctorDefs $ \(ctorName, ctorType) ->
      recursorCaseFromCtor ctorName typeName (Ctor typeName ctorName) ctorType
    addParam (motive, motiveType) <$>
      (foldr (addParam . (hole,)) <$> motiveFromSelf typeName [] signature <*> pure cases)

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that the given term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType term type' = traceM "checkType" [ppr term, ppr type'] (const "") $
  case term of
    Lam bodyBind -> Equal.whnf type' >>= \case
      Pi paramType typeBind -> do
        -- unbind the variables in the lambda expression and pi type
        (var, body, _, returnType) <- lift $ Unbound.unbind2Plus bodyBind typeBind
        -- check the type of the body of the lambda expression
        Env.addLocal var paramType $ checkType body returnType
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD type']

    TrustMe -> return ()

    _ -> flip Equal.equate type' =<< inferType term

checkCtorReturnsSelf :: CtorName -> DataTypeName -> Type -> TcMonad ()
checkCtorReturnsSelf _ self (DataType typeName) | typeName == self = return ()
checkCtorReturnsSelf ctorName self (App typeCtor _) = checkCtorReturnsSelf ctorName self typeCtor
checkCtorReturnsSelf ctorName self _ =
  Env.err [DS "Constructor", DD ctorName, DS "must return", DD self]

throwIfFound :: DataTypeName -> Type -> TcMonad ()
throwIfFound typeName type' = Equal.whnf type' >>= \case
  TyType -> return ()
  Var _ -> return ()
  Lam bind -> do
    (_, body) <- Unbound.unbind bind
    throwIfFound typeName body
  App a b -> mapM_ (throwIfFound typeName) [a, b]
  Pi paramType bind -> do
    throwIfFound typeName paramType
    (_, returnType) <- Unbound.unbind bind
    throwIfFound typeName returnType
  Ann term _ -> throwIfFound typeName term
  TrustMe -> return ()
  DataType name -> when (name == typeName) $
    Env.err [DS "Invalid occurence of data type", DD typeName, DS "being declared"]
  Ctor _ _ -> return ()
  Rec _ -> return ()

checkStrictPositivity :: DataTypeName -> Type -> TcMonad ()
checkStrictPositivity self paramType = Equal.whnf paramType >>= \case
  App a b -> do
    throwIfFound self b
    checkStrictPositivity self a
  Pi paramType bind -> do
    throwIfFound self paramType
    (_, returnType) <- Unbound.unbind bind
    checkStrictPositivity self returnType
  _ -> return ()

-- | Check each sort of declaration in a module
tcEntry :: Entry -> TcMonad ()
tcEntry entry = traceM "tcEntry" [ppr entry] (const "") $ case entry of
  Decl var hint term -> Env.lookupDecl var >>= \case
    Just _ -> Env.err [DD var, DS "already defined"]
    Nothing -> checkType term hint `catchError` handler where
      handler (Env.Err msg) = throwError $ Env.Err (msg $$ msg')
      msg' = disp [DS "When checking the term", DD term, DS "against the type", DD hint]
  dataDecl@(Data typeName typeParams ctors) -> do
    checkType typeParams TyType
    forM_ ctors $ \(ctorName, ctorType) -> do
      Env.addDataType typeName typeParams [] $ checkType ctorType TyType
      (paramTypes, returnType) <- Equal.unfoldPi ctorType
      checkCtorReturnsSelf ctorName typeName =<< Equal.whnf returnType
      mapM (checkStrictPositivity typeName) paramTypes

withEntries :: [Entry] -> TcMonad a -> TcMonad a
withEntries [] monad = monad
withEntries (entry : rest) monad = tcEntry entry >> case entry of
  Decl var type' def -> Env.addDecl var type' def (withEntries rest monad)
  Data name params ctors -> Env.addDataType name params ctors (withEntries rest monad)

tcEntries :: [Entry] -> TcMonad ()
tcEntries = flip withEntries $ return ()