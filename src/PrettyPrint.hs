module PrettyPrint where

import Prelude hiding ((<>))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader (ask, local), asks)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Text.PrettyPrint (Doc, (<>), (<+>))
import Text.PrettyPrint qualified as PP
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Name qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)
import Syntax

displayUnivParams :: [UnivParamName] -> Doc
displayUnivParams [] = PP.empty
displayUnivParams univParamNames =
  PP.text ".{" <> PP.hsep (PP.punctuate PP.comma (map PP.text univParamNames)) <> PP.text "}"

disp :: Display a => a -> Doc
disp = flip display DispInfo { taken = Set.empty, prec = 0 }

-- | Convenience entry point for the pretty printer
ppr :: Display a => a -> String
ppr = PP.render . disp

class Unbound.Alpha a => Display a where
  -- | Convert a value to a 'Doc'.
  display :: a -> DispInfo -> Doc

instance {-# OVERLAPPABLE #-} (Show a, Unbound.Alpha a) => Display a where
  display = return . PP.text . show

-- | The data structure for information about the display
data DispInfo = DispInfo {
  taken :: Set Unbound.AnyName, -- names that have been used
  prec :: Int -- current precedence level
}

-- | Error message quoting
data D = DS String | forall a. Display a => DD a

instance Display [Entry] where
  display entries = PP.vcat <$> mapM display entries

instance Display Entry where
  display (Decl var univParamNames hint def) = PP.hang <$> decl <*> pure 2 <*> display def where
    decl = foldl1 (<+>) <$> sequence
      [pure $ PP.text var <> displayUnivParams univParamNames, pure $ PP.text ":", display hint, pure $ PP.text "="]

  display (Data name univParamNames params ctors) = do
    name <- display name
    params <- display params
    ctors <- forM ctors $ \(ctorName, ctorType) -> do
      ctorName <- display ctorName
      ctorType <- display ctorType
      return $ ctorName <+> PP.text ":" <+> ctorType
    return $ PP.hang (PP.text "data" <+> (name <> displayUnivParams univParamNames) <+> PP.text ":" <+> params <+> PP.text "where") 2 (PP.vcat ctors)

instance Display Const where
  display (GVar name) = display name
  display (DataType typeName) = display typeName
  display (Ctor typeName ctorName) = display @String [i|#{typeName}.#{ctorName}|]
  display (Rec typeName) = display @String [i|#{typeName}.rec|]

instance Display Level where
  display level
    | Just n <- levelToInt level = return $ PP.int n
    | otherwise = do
        baseDoc <- case baseLevel of
          Max level1 level2 -> do
            level1 <- display level1
            level2 <- display level2
            return $ PP.text "max" <> PP.parens (level1 <> PP.comma <+> level2)
          Param univParamName -> return $ PP.text univParamName
          LMVar id -> return $ PP.text $ "?u#" ++ show id
          Zero -> return $ PP.int 0
          Succ _ -> error "unreachable normalized level"
        return $ case offset of
          0 -> baseDoc
          _ | baseLevel == Zero -> PP.int offset
          _ -> parenthesizeIfNeeded baseLevel baseDoc <> PP.text "+" <> PP.int offset
    where
      (baseLevel, offset) = unfoldSucc level

      parenthesizeIfNeeded baseLevel = case baseLevel of
        Max _ _ -> PP.parens
        _ -> id

instance Display String where
  display = return . PP.text

levelApp :: Int
levelApp     = 10
levelLam :: Int
levelLam     = 0
levelArrow :: Int
levelArrow   = 5

withPrec :: MonadReader DispInfo m => Int -> m a -> m a
withPrec prec = local (\dispInfo -> dispInfo { prec = prec })

parensIf :: Bool -> Doc -> Doc
parensIf b = if b then PP.parens else id

showNameExact :: Unbound.Name a -> String
showNameExact (Unbound.Fn name id) = [i|#{name}.#{id}|]
showNameExact (Unbound.Bn debruijn id) = [i|#{debruijn}.#{id}|]

instance Display (Unbound.Name Term) where
  display = display . showNameExact

piDocs :: Type -> DispInfo -> [Doc]
piDocs (Pi binderInfo paramType bind) = Unbound.lunbind bind $ \(paramName, returnType) -> do
  paramDoc <- if paramName `elem` toListOf Unbound.fv returnType
    then fmap PP.parens $ (<+>) . (<+> PP.colon) <$> display paramName <*> display paramType
    else withPrec (levelArrow + 1) (display paramType)
  paramDoc <- return $ case binderInfo of
    Implicit -> PP.braces paramDoc
    Explicit -> paramDoc
  (paramDoc :) <$> piDocs returnType
piDocs returnType = return <$> display returnType

instance Display Term where
  display (Sort Zero) = return $ PP.text "Set"

  display (Sort level) = (PP.text "Sort" <+>) <$> display level

  display (LVar name) = display name

  display (MVar i) = display $ "?" ++ show i

  display (Const constant levels) = (<>) <$> display constant <*> displayLevels levels where
    displayLevels [] = const PP.empty
    displayLevels levels = do
      dlevels <- mapM display levels
      return $ PP.text ".{" <> PP.hsep (PP.punctuate PP.comma dlevels) <> PP.text "}"

  display a@(Lam _ _) = do
    n <- asks (.prec)
    (binds, body) <- withPrec levelLam $ gatherBinders a
    return $ parensIf (levelLam < n) $
      PP.hang (PP.text "\\" <> PP.hsep binds <> PP.text ".") 2 body where
      gatherBinders (Lam _ b) =
        Unbound.lunbind b $ \(name, body) -> do
          name <- display name
          (rest, body') <- gatherBinders body
          return (name : rest, body')
      gatherBinders body = ([],) <$> display body

  display app@(App _ _) = do
    prec <- asks (.prec)
    let (func, args) = unfoldApps app
    func <- withPrec levelApp $ display func
    args <- mapM (withPrec (levelApp + 1) . display) args
    return $ parensIf (levelApp < prec) $ PP.hang func 2 $ PP.sep args

  display piType@(Pi {}) = do
    precision <- asks (.prec)
    let arrow = mappend PP.space $ PP.text "->"
    parensIf (levelArrow < precision) . PP.sep . PP.punctuate arrow <$> piDocs piType

  display (Ann term type') = do
    term <- withPrec 0 $ display term
    type' <- withPrec 0 $ display type'
    return $ PP.parens $ term <+> PP.text ":" <+> type'

-- LFresh instance for DisplayInfo reader monad
instance Unbound.LFresh ((->) DispInfo) where
  lfresh name = do
    DispInfo dispAvoid _ <- ask
    let baseName = Unbound.name2String name
    let freshNames = map (Unbound.makeName baseName) [0 ..]
    return $ fromJust $ find ((`Set.notMember` dispAvoid) . Unbound.AnyName) freshNames
  getAvoids = asks (.taken)
  avoid names = local $
    \dispInfo -> dispInfo { taken = Set.fromList names `Set.union` dispInfo.taken }
