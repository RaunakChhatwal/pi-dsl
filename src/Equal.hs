module Equal (isDefEq, whnf, unify) where

import Control.Applicative (empty)
import Control.Monad (unless)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Extra (andM, unlessM)
import Control.Monad.State.Class qualified as State
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Data.Bifunctor (second)
import Data.Foldable (foldrM)
import Data.List (nub)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound
import Environment
import Inductive
import PrettyPrint
import Syntax
import {-# SOURCE #-} TypeCheck

flattenAndNormalizeMaxArgs :: Level -> [Level]
flattenAndNormalizeMaxArgs level = go level False where
  go (Max level1 level2) alreadyNormalized = go level2 alreadyNormalized ++ go level1 alreadyNormalized
  go level False = go (normalizeLevel level) True
  go level True = [level]

mergeMaxArgs :: [Level] -> [(Level, Int)]
mergeMaxArgs levels = Map.toAscList $ foldl mergeMaxArg Map.empty $ map unfoldSucc levels where
  mergeMaxArg maxArgs (inner, offset) = case Map.lookup inner maxArgs of
    Just offset2 | offset < offset2 -> maxArgs
    _ -> Map.insert inner offset maxArgs

normalizeLevel :: Level -> Level
normalizeLevel level = case unfoldSucc level of
  (Max level1 level2, offset) -> foldl1 Max $ map foldSucc subsumedMaxArgs where
    subsumedMaxArgs = case uniqueMaxArgs of
      (Zero, explicitOffset) : rest | any ((explicitOffset <=) . snd) rest -> rest
      _ -> uniqueMaxArgs
    uniqueMaxArgs = map (second (offset +)) $ mergeMaxArgs maxArgs
    maxArgs = flattenAndNormalizeMaxArgs level1 ++ flattenAndNormalizeMaxArgs level2

  _ -> level

decrementLevel :: Level -> MaybeT TcMonad Level
decrementLevel = go True where
  go canAssignMVars = \case
    LMVar id | canAssignMVars -> do
      freshLevel <- lift newLevelMVar
      lift $ assignLevelMVar id $ Succ freshLevel
      return freshLevel
    Succ level -> return level
    Max level1 level2 -> fmap normalizeLevel $ Max <$> go False level1 <*> go False level2
    _ -> empty

isLevelDefEqOneWayHelper :: Level -> Level -> TcMonad Bool
isLevelDefEqOneWayHelper level1 level2 | level1 == level2 = return True
isLevelDefEqOneWayHelper level1 level2 = case (level1, level2) of
  (LMVar id, _) | not $ levelMVarOccursCheck id level2 -> assignLevelMVar id level2 >> return True
  (_, LMVar _) -> return False

  (Zero, Max level21 level22) ->
    (&&) <$> isLevelDefEq Zero level21 <*> isLevelDefEq Zero level22

  (Succ pred1, _)
    | LMVar id <- pred1, levelMVarOccursCheck id level2 -> return False
    | otherwise -> runMaybeT (decrementLevel level2) >>= \case
      Just pred2 -> isLevelDefEq pred1 pred2
      Nothing -> return False

  _ -> return False

isLevelDefEqOneWay :: Level -> Level -> TcMonad Bool
isLevelDefEqOneWay level1 level2 = do
  tcState <- State.get
  isLevelDefEqOneWayHelper level1 level2 >>= \case
    True -> return True
    False -> State.put tcState >> return False

isLevelDefEq :: Level -> Level -> TcMonad Bool
isLevelDefEq (Succ level1) (Succ level2) = isLevelDefEq level1 level2
isLevelDefEq level1 level2 = do
  level1 <- normalizeLevel <$> instantiateLevelMVars level1
  level2 <- normalizeLevel <$> instantiateLevelMVars level2
  isLevelDefEqOneWay level1 level2 >>= \case
    True -> return True
    False -> isLevelDefEqOneWay level2 level1

-- unifyLevel :: Level -> Level -> TcMonad ()
-- unifyLevel level1 level2 = unlessM (isLevelDefEq level1 level2) $
--   err [DS "Expected universe level", DD level2, DS "but found", DD level1]

millerPatternCheck :: Term -> TcMonad (Maybe (Int, [TermName]))
millerPatternCheck term = case unfoldApps term of
  (MVar id, args) -> return $ do
    let prependIfLocal (LVar name) rest = return $ name : rest
        prependIfLocal _ _ = Nothing
    localArgs <- foldrM prependIfLocal [] args
    unless (length localArgs == length (nub localArgs)) Nothing
    return (id, localArgs)
  _ -> return Nothing

rollbackUnlessTrue :: TcMonad Bool -> TcMonad Bool
rollbackUnlessTrue monad = do
  tcState <- State.get
  solved <- catchError monad $ const $ return False
  unless solved $ State.put tcState
  return solved

-- Solve a metavariable application ?m x1 ... xn
solveMVar :: Int -> Term -> [TermName] -> TcMonad Bool
solveMVar id term args = rollbackUnlessTrue $ do
  let freeVars = Unbound.toListOf @Term @TermName Unbound.fv term
  solved <- rollbackUnlessTrue $ if any (`notElem` args) freeVars
    then return False
    else do
      let abstract arg body = Lam Explicit $ Unbound.bind arg body
      let solution = foldr abstract term args
      checkType solution =<< lookUpMVarType id
      assignMVar id solution
      return True

  if solved
    then return True
    else case (term, reverse args) of
      (App func arg, lastArg : initArgsRev) ->
        isDefEq (LVar lastArg) arg >>= \case
          True -> solveMVar id func (reverse initArgsRev)
          False -> return False
      _ -> return False

isDefEq :: Term -> Term -> TcMonad Bool
isDefEq term1 term2 = traceM "isDefEq" [ppr term1, ppr term2] (const "") $ rollbackUnlessTrue $ do
  term1 <- whnf term1
  term2 <- whnf term2
  (,) <$> millerPatternCheck term1 <*> millerPatternCheck term2 >>= \case
    (Just (id1, args1), Just (id2, args2)) | id1 == id2 && length args1 == length args2 ->
      andM $ zipWith isDefEq (map LVar args1) (map LVar args2)
    (Just (id1, args1), Just (id2, args2)) -> do
      solveMVar id1 term2 args1 >>= \case
        True -> return True
        False -> solveMVar id2 term1 args2
    (Just (id, args), _) -> solveMVar id term2 args
    (_, Just (id, args)) -> solveMVar id term1 args

    _ -> case (term1, term2) of
      (Sort level1, Sort level2) -> isLevelDefEq level1 level2

      (Const const1 levels1, Const const2 levels2) | Unbound.aeq const1 const2 ->
        andM $ zipWith isLevelDefEq levels1 levels2

      (Lam binderInfo1 binder1, Lam binderInfo2 binder2) | binderInfo1 == binderInfo2 -> do
        (_, body1, _, body2) <- lift $ Unbound.unbind2Plus binder1 binder2
        isDefEq body1 body2

      (App func1 arg1, App func2 arg2) -> (&&) <$> isDefEq func1 func2 <*> isDefEq arg1 arg2

      (Pi binderInfo1 paramType1 binder1, Pi binderInfo2 paramType2 binder2) | binderInfo1 == binderInfo2 -> do
        (_, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus binder1 binder2
        (&&) <$> isDefEq paramType1 paramType2 <*> isDefEq returnType1 returnType2

      _ | Unbound.aeq term1 term2 -> return True

      _ -> return False

unify :: Term -> Term -> TcMonad ()
unify term1 term2 = unlessM (isDefEq term1 term2) $ err [DS "Expected", DD term2, DS "but found", DD term1]

-- Convert a term to its weak-head normal form, only accepts well typed terms
whnf :: Term -> TcMonad Term
whnf term = traceM "whnf" [ppr term] ppr $ case term of
  Const (GVar var) levels -> lookUpDecl var >>= \case
    Just (DeclInfo univParams _ body) -> substLevels (zip univParams levels) <$> whnf body
    Nothing -> throwError [i|Global variable #{var} not found|]

  MVar id -> lookUpMVarSolution id >>= \case
    Just soln -> whnf soln
    Nothing -> return term

  term@(App _ _) -> do
    let (func, args) = unfoldApps term
    funcNF <- whnf func
    case funcNF of
      Lam _ binder -> whnf $ foldl App (Unbound.instantiate binder [arg]) rest where (arg : rest) = args
      Const (Rec typeName) _ -> runMaybeT (reduceRecursor typeName funcNF args) >>= \case
        Nothing -> return $ foldl App funcNF args
        Just reduced -> whnf reduced
      _ | Unbound.aeq func funcNF -> return term
      _ -> whnf $ foldl App funcNF args

  Ann term _ -> whnf term

  Sort level -> return $ Sort $ normalizeLevel level

  term -> return term
