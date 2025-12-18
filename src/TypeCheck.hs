module TypeCheck where

import Control.Monad.Except
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Environment (TcMonad, traceM)
import Environment qualified as Env
import Equal qualified
import PrettyPrint (D(DS, DD), Disp (disp), debug, ppr, debugDisp, teleToPi)
import Syntax
import Text.PrettyPrint.HughesPJ (($$), render)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.String.Interpolate (i)

betaReduceTyPi :: (Type, Unbound.Bind TName Type) -> Term -> TcMonad Type
betaReduceTyPi (paramType, returnType) arg = do
  checkType arg paramType
  return $ Unbound.instantiate returnType [arg]

-- Infer/synthesize the type of a term
inferType :: Term -> TcMonad Type
inferType term = traceM "inferType" [ppr term] ppr $ case term of
  Var x -> declType <$> Env.lookupTy x

  TyType -> return TyType

  TyPi tyA bnd -> do
    (x, tyB) <- Unbound.unbind bnd
    checkType tyA TyType
    Env.extendCtx (Decl (TypeDecl x tyA)) (checkType tyB TyType)
    return TyType

  App func arg -> do
    funcType <- inferType func
    Equal.whnf funcType >>= \case
      TyPi paramType returnType -> betaReduceTyPi (paramType, returnType) arg

      typeCtor@(TyCon _) -> inferType typeCtor >>= \case
        TyType -> undefined -- TODO: throw error message
        TyPi paramType returnType -> betaReduceTyPi (paramType, returnType) arg
        _ -> undefined

      _ -> Env.err [DS "Expected function but found ", DD func, DS "of type", DD funcType]

  Ann a tyA -> do
    checkType tyA TyType
    checkType a tyA
    return tyA

  -- Type constructor application
  TyCon typeName -> do
    (typeParams, _) <- Env.lookupTCon typeName
    return $ teleToPi typeParams TyType

  -- Data constructor application
  -- we don't know the expected type
  -- so see if there is only one datacon of that name that takes no parameters
  DataCon ctorName -> Env.lookupDConAll ctorName >>= \case
    [(_, (_, CtorDef _ ctorParams returnType))] -> return $ teleToPi ctorParams returnType
    _ -> Env.err [DS "Ambiguous data constructor", DS ctorName]

  _ -> Env.err [DS "Need a type annotation for", DD term]

-- Check that the given term has the expected type
checkType :: Term -> Type -> TcMonad ()
checkType tm ty = traceM "checkType" [ppr tm, ppr ty] (const "") $ do
  ty' <- Equal.whnf ty 
  case tm of 
    -- c-lam: check the type of a function
    Lam bnd -> case ty' of
      TyPi tyA bnd2 -> do
        -- unbind the variables in the lambda expression and pi type
        (x, body, tyB) <- unbind2 bnd bnd2
        -- check the type of the body of the lambda expression
        Env.extendCtx (Decl (TypeDecl x tyA)) (checkType body tyB)
      _ -> Env.err [DS "Lambda expression should have a function type, not", DD ty']

    TrustMe -> return ()

    _ -> do
      tyA <- inferType tm
      Equal.equate tyA ty'

-- | The Env-delta returned when type-checking a top-level Entry.
data HintOrCtx
  = AddHint TypeDecl
  | AddCtx [Entry]

instance Disp HintOrCtx where
  disp (AddHint typeDecl) = disp @String [i|AddHint:\n#{ppr typeDecl}|]
  disp (AddCtx decls) = disp @String [i|AddCtx:\n#{intercalate "\n" $ map ppr decls}|]
  debugDisp = disp

-- | Check each sort of declaration in a module
tcEntry :: Entry -> TcMonad HintOrCtx
tcEntry entry = traceM "tcEntry" [ppr entry] ppr $ case entry of
  Def n term -> do
    oldDef <- Env.lookupDef n
    maybe tc die oldDef
    where
      tc = do
        lkup <- Env.lookupHint n
        case lkup of
          Nothing -> do
            ty <- inferType term
            return $ AddCtx [Decl (TypeDecl n ty), Def n term]
          Just decl -> let
            handler (Env.Err msg) = throwError $ Env.Err (msg $$ msg')
            msg' = disp [ DS "When checking the term", DD term, DS "against the type", DD decl ]
            in do
              -- Env.extendCtx (Decl decl) $ checkType term (declType decl) `catchError` handler
              checkType term (declType decl) `catchError` handler
              return $ AddCtx [Decl decl, Def n term]
      die term' = Env.err
        [DS "Multiple definitions of", DD n, DS "Previous definition was", DD term']
  Decl decl -> do
    duplicateTypeBindingCheck decl
    checkType (declType decl) TyType
    return $ AddHint decl
  dataDecl@(Data typeName typeParams ctors) -> do
    checkType (teleToPi typeParams TyType) TyType
    -- TODO: check strict positivity, returnType is self
    forM_ ctors $ \(CtorDef name params returnType) ->
      Env.extendCtx (Data typeName typeParams []) $ checkType (teleToPi params returnType) TyType
    -- TODO: assert unique ctor name
    return $ AddCtx [dataDecl]


-- | Make sure that we don't have the same name twice in the
-- environment. (We don't rename top-level module definitions.)
duplicateTypeBindingCheck :: TypeDecl -> TcMonad ()
duplicateTypeBindingCheck decl = do
  -- Look for existing type bindings ...
  let n = declName decl
  l <- Env.lookupTyMaybe n
  l' <- Env.lookupHint n
  -- ... we don't care which, if either are Just.
  case catMaybes [l, l'] of
    [] -> return ()
    -- We already have a type in the environment so fail.
    decl' : _ -> Env.err [DS "Duplicate type declaration", DD decl, DS "Previous was", DD decl']


-- | Given a particular data constructor name and a list of data
-- constructor definitions, pull the definition out of the list and
-- return it paired with the remainder of the list.
removeDCon :: DataConName -> [CtorDef] -> TcMonad (CtorDef, [CtorDef])
removeDCon dc (cd@(CtorDef dc' _ returnType) : rest)
  | dc == dc' =
    return (cd, rest)
removeDCon dc (cd1 : rest) = do
  (cd2, rr) <- removeDCon dc rest
  return (cd2, cd1 : rr)
removeDCon dc [] = Env.err [DS $ "Internal error: Can't find " ++ show dc]
