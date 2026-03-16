module Equal (whnf, unify) where

import Control.Applicative (empty)
import Control.Monad (zipWithM_, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Data.Bifunctor (second)
import Data.Foldable (foldrM)
import Data.List (nub)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Environment
import Inductive
import PrettyPrint
import Syntax
import {-# SOURCE #-} TypeCheck

millerPatternCheck :: Term -> TcMonad (Maybe (Int, [TermName]))
millerPatternCheck term = case unfoldApps term of
  (MVar id, args) -> return $ do
    let prependIfLocal (LVar name) rest = return $ name : rest
        prependIfLocal _ _ = Nothing
    localArgs <- foldrM prependIfLocal [] args
    unless (length localArgs == length (nub localArgs)) Nothing
    return (id, localArgs)
  _ -> return Nothing

-- Solve a metavariable application ?m x1 ... xn
solveMVar :: Int -> Term -> [TermName] -> TcMonad ()
solveMVar id term args = lookUpMVarSolution id >>= \case
  Just soln -> unify (foldl App soln $ map LVar args) term
  Nothing -> do
    let abstract arg body = Lam Explicit $ Unbound.bind arg body
    let solution = foldr abstract term args
    checkType solution =<< lookUpMVarType id
    assignMVar id solution

flattenAndNormalizeMaxArgs :: Level -> [Level]
flattenAndNormalizeMaxArgs level = go level False where
  go (Max level1 level2) alreadyNormalized =
    go level2 alreadyNormalized ++ go level1 alreadyNormalized
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

solveLevel :: Level -> Level -> TcMonad Bool
solveLevel level1 level2 | level1 == level2 = return True
solveLevel level1 level2 = case (level1, level2) of
  (LMVar id, _) | not $ levelMVarOccursCheck id level2 -> assignLevelMVar id level2 >> return True
  (_, LMVar _) -> return False

  (Zero, Max level21 level22) ->
    (&&) <$> tryUnifyLevel Zero level21 <*> tryUnifyLevel Zero level22

  (Succ pred1, _)
    | LMVar id <- pred1, levelMVarOccursCheck id level2 -> return False
    | otherwise -> runMaybeT (decrementLevel level2) >>= \case
      Just pred2 -> tryUnifyLevel pred1 pred2
      Nothing -> return False

  _ -> return False

tryUnifyLevel :: Level -> Level -> TcMonad Bool
tryUnifyLevel (Succ level1) (Succ level2) = tryUnifyLevel level1 level2
tryUnifyLevel level1 level2 = do
  level1 <- normalizeLevel <$> instantiateLevelMVars level1
  level2 <- normalizeLevel <$> instantiateLevelMVars level2
  solveLevel level1 level2 >>= \case
    True -> return True
    False -> solveLevel level2 level1

unifyLevel :: Level -> Level -> TcMonad ()
unifyLevel level1 level2 = unlessM (tryUnifyLevel level1 level2) $
  err [DS "Expected universe level", DD level2, DS "but found", DD level1]

unify :: Term -> Term -> TcMonad ()
unify term1 term2 = traceM "unify" [ppr term1, ppr term2] (const "") $ do
  term1 <- whnf term1
  term2 <- whnf term2
  (,) <$> millerPatternCheck term1 <*> millerPatternCheck term2 >>= \case
    (Just (id1, args1), Just (id2, args2)) | id1 == id2 && length args1 == length args2 ->
      zipWithM_ unify (map LVar args1) (map LVar args2)
    (Just (id, args), _) -> solveMVar id term2 args
    (_, Just (id, args)) -> solveMVar id term1 args

    _ -> case (term1, term2) of
      (Sort level1, Sort level2) -> unifyLevel level1 level2

      (Const const1 levels1, Const const2 levels2) | Unbound.aeq const1 const2 ->
        zipWithM_ unifyLevel levels1 levels2

      (Lam binderInfo1 binder1, Lam binderInfo2 binder2) | binderInfo1 == binderInfo2 -> do
        (_, body1, _, body2) <- lift $ Unbound.unbind2Plus binder1 binder2
        unify body1 body2

      (App func1 arg1, App func2 arg2) ->
        unify func1 func2 >> unify arg1 arg2

      (Pi binderInfo1 paramType1 binder1, Pi binderInfo2 paramType2 binder2)
        | binderInfo1 == binderInfo2 -> do
          unify paramType1 paramType2
          (_, returnType1, _, returnType2) <- lift $ Unbound.unbind2Plus binder1 binder2
          unify returnType1 returnType2

      _ | Unbound.aeq term1 term2 -> return ()

      _ -> err [DS "Expected", DD term2, DS "but found", DD term1]

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
      Lam _ binder ->
        whnf $ foldl App (Unbound.instantiate binder [arg]) rest where (arg : rest) = args
      Const (Rec typeName) _ -> runMaybeT (reduceRecursor typeName funcNF args) >>= \case
        Nothing -> return $ foldl App funcNF args
        Just reduced -> whnf reduced
      _ | Unbound.aeq func funcNF -> return term
      _ -> whnf $ foldl App funcNF args

  Ann term _ -> whnf term

  term -> return term
