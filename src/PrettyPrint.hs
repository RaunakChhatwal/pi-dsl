-- | A Pretty Printer.
module PrettyPrint (Disp (..), D (..), PP.Doc, ppr, debug) where

import Control.Monad.Reader (MonadReader (ask, local), asks)
import Data.Set qualified as S

import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP
import Unbound.Generics.LocallyNameless qualified as Unbound
import qualified Unbound.Generics.LocallyNameless.Name as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import Syntax
import Data.String.Interpolate (i)
import Data.Foldable (find)
import Data.Maybe (fromJust)

-------------------------------------------------------------------------

-- * Classes and Types for Pretty Printing

-------------------------------------------------------------------------

-- | The 'Disp' class governs all types which can be turned into 'Doc's
-- The `disp` function is the main entry point for the pretty printer
class Disp d where
  disp :: d -> Doc
  debugDisp :: d -> Doc

  default disp :: (Display d) => d -> Doc
  disp d = display d initDI

  default debugDisp :: (Display d) => d -> Doc
  debugDisp d = display d initDI{showLongNames = True, showAnnots=True}

-- | Convenience entry point for the pretty printer
ppr :: Disp d => d -> String
ppr p = PP.render (disp p)

debug :: Disp d => d -> String
debug p = PP.render (debugDisp p)

-- | The 'Display' class is like the 'Disp' class. It qualifies
--   types that can be turned into 'Doc'.  The difference is that
--   this class uses the 'DispInfo' parameter and the Unbound library
--   to generate fresh names during printing.
class (Unbound.Alpha t) => Display t where
  -- | Convert a value to a 'Doc'.
  display :: t -> DispInfo -> Doc

-- | The data structure for information about the display
data DispInfo = DI
  { -- | should we show type annotations?
    showAnnots :: Bool,
    -- | names that have been used
    dispAvoid :: S.Set Unbound.AnyName,
    -- | current precedence level
    prec :: Int,
    -- | should we print internally-generated names, or user-friendly versions
    showLongNames :: Bool
  }

-- | Error message quoting
data D
  = -- | String literal
    DS String
  | -- | Displayable value
    forall a. Disp a => DD a

initDI :: DispInfo
initDI = DI {showAnnots = True,
                          dispAvoid = S.empty,
                          prec = 0,
                          showLongNames = True
                          }


-------------------------------------------------------------------------

-- * Disp Instances for quoting, errors, source positions, names

-------------------------------------------------------------------------

instance Disp D where
  disp (DS s) = PP.text s
  disp (DD d) = PP.nest 2 $ disp d

  debugDisp d@(DS _) = disp d
  debugDisp (DD d) = PP.nest 2 $ debugDisp d

instance Disp [D] where
  disp dl = PP.sep $ map disp dl
  debugDisp dl = PP.sep $ map disp dl

instance Disp (Unbound.Name Term) where
  disp = PP.text . Unbound.name2String
  debugDisp = PP.text . show

-------------------------------------------------------------------------

-- * Disp Instances for Term syntax (defaults to Display, see below)

-------------------------------------------------------------------------

instance Disp Term

instance Disp Entry

instance Disp [Entry]


instance Display [Entry] where
  display ds = do
    dd <- mapM display ds
    pure $ PP.vcat dd


instance Display Entry where
  display (Decl var hint def) = PP.hang <$> decl <*> pure 2 <*> display def where
    decl = foldl1 (<+>) <$> sequence
      [display var, pure $ PP.text ":", display hint, pure $ PP.text "="]
  display (Data n params constructors) = do
    dn <- display n
    dp <- display params
    dc <- mapM (\(ctorName, ctorType) -> do
      dcn <- display ctorName
      dct <- display ctorType
      pure $ dcn <+> PP.text ":" <+> dct) constructors
    pure $ PP.hang (PP.text "data" <+> dn <+> PP.text ":" <+> dp <+> PP.text "where") 2 (PP.vcat dc)


instance Display Var where
  display (Local name) = display name
  display (Global name) = display name
  display (Meta i) = display @String ("?" ++ show i)

-------------------------------------------------------------------------

-- * Disp Instances for Prelude types

-------------------------------------------------------------------------

instance Disp String where
  disp = PP.text
  debugDisp = disp

instance Disp Int where
  disp = PP.text . show
  debugDisp = disp

instance Disp Integer where
  disp = PP.text . show
  debugDisp = disp

instance Disp Double where
  disp = PP.text . show
  debugDisp = disp

instance Disp Float where
  disp = PP.text . show
  debugDisp = disp

instance Disp Char where
  disp = PP.text . show
  debugDisp = disp

instance Disp Bool where
  disp = PP.text . show
  debugDisp = disp

dispMaybe :: (t -> Doc) -> Maybe t -> Doc
dispMaybe disp m = case m of
  (Just a) -> PP.text "Just" <+> disp a
  Nothing -> PP.text "Nothing"

instance Disp a => Disp (Maybe a) where
  disp = dispMaybe disp
  debugDisp = dispMaybe debugDisp


dispEither :: (Disp a, Disp b) => (forall a. Disp a => a -> Doc) -> Either a b -> Doc
dispEither disp e = case e of
     (Left a) -> PP.text "Left" <+> disp a
     (Right a) -> PP.text "Right" <+> disp a

instance (Disp a, Disp b) => Disp (Either a b) where
  disp = dispEither disp
  debugDisp = dispEither debugDisp


-------------------------------------------------------------------------

-- * Display instances for Prelude types used in AST

-------------------------------------------------------------------------

instance Display String where
  display = return . PP.text

instance Display Int where
  display = return . PP.text . show

instance Display Integer where
  display = return . PP.text . show

instance Display Double where
  display = return . PP.text . show

instance Display Float where
  display = return . PP.text . show

instance Display Char where
  display = return . PP.text . show

instance Display Bool where
  display = return . PP.text . show

-------------------------------------------------------------------------

-- * Display instances for Terms

-------------------------------------------------------------------------


levelApp :: Int
levelApp     = 10
levelLam :: Int
levelLam     = 0
levelArrow :: Int
levelArrow   = 5

withPrec :: MonadReader DispInfo m => Int -> m a -> m a
withPrec p = local (\d -> d { prec = p })

parens :: Bool -> Doc -> Doc
parens b = if b then PP.parens else id

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
  let paramDoc' = case binderInfo of
        Implicit -> PP.braces paramDoc
        Explicit -> paramDoc
  (paramDoc' :) <$> piDocs returnType
piDocs returnType = pure <$> display returnType

instance Display Term where
  display (Sort Zero) = return $ PP.text "Set"
  display (Sort level) = return $ PP.text ("Sort" ++ show level)
  display (Var var) = display var
  display a@(Lam _ _) = do
    n <- ask prec
    (binds, body) <- withPrec levelLam $ gatherBinders a
    return $ parens (levelLam < n) $ PP.hang (PP.text "\\" PP.<> PP.hsep binds PP.<> PP.text ".") 2 body
  display app@(App _ _) = do
    n <- ask prec
    let (func, args) = unfoldApps app
    df <- withPrec levelApp (display func)
    dargs <- mapM (withPrec (levelApp+1) . display) args
    return $ parens (levelApp < n) $ PP.hang df 2 (PP.sep dargs)
  display piType@(Pi {}) = do
    precision <- ask prec
    let arrow = PP.space <> PP.text "->"
    parens (levelArrow < precision) . PP.sep . PP.punctuate arrow <$> piDocs piType
  display (Ann a b) = do
    sa <- ask showAnnots
    if sa then do
      da <- withPrec 0 (display a)
      db <- withPrec 0 (display b)
      return $ PP.parens (da <+> PP.text ":" <+> db)
      else display a
  display (DataType typeName) = display typeName
  display (Ctor typeName ctorName) = display @String [i|#{typeName}.#{ctorName}|]
  display (Rec typeName) = display @String [i|#{typeName}.rec|]

gatherBinders :: Term -> DispInfo -> ([Doc], Doc)
gatherBinders (Lam _ b) =
  Unbound.lunbind b $ \(n, body) -> do
    dn <- display n
    (rest, body') <- gatherBinders body
    return (dn : rest, body')
gatherBinders body = do
  db <- display body
  return ([], db)


-- LFresh instance for DisplayInfo reader monad
instance Unbound.LFresh ((->) DispInfo) where
  lfresh nm = do
    let s = Unbound.name2String nm
    di <- ask
    return $ fromJust $
      find (\x -> Unbound.AnyName x `S.notMember` dispAvoid di) (map (Unbound.makeName s) [0 ..])
  getAvoids = asks dispAvoid
  avoid names = local upd where upd di = di { dispAvoid = S.fromList names `S.union` dispAvoid di }
