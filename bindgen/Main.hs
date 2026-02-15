module Main where

import Language.Haskell.TH qualified as TH
import Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings)
import Environment (Env, Trace)
import Syntax (Entry, Term, Type, TermName)
import qualified Unbound.Generics.LocallyNameless as Unbound

-- Entry point: prints generated Python bindings to stdout
main :: IO ()
main = putStrLn $(do
  -- Build topologically sorted list of types reachable from Env
  declOrder <- buildDeclOrder ''Env
  -- Generate bindings for all types in dependency order
  bindings <- mapM bindingFromName $ [''Either, ''Trace] ++ declOrder ++ [''Entry]
  -- FFI wrapper for creating bound terms
  bind <- functionBinding "bind" ["var", "body"]
    <$> sequence [[t|TermName|], [t|Term|]] <*> [t|Unbound.Bind TermName Term|]
  -- FFI wrapper for pretty-printing terms
  pprTerm <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  -- FFI wrapper for type checking a list of entries
  typeCheck <- functionBinding "type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| (Maybe String, [Trace]) |]
  -- FFI wrapper for inferring the type of a term
  inferType <- functionBinding "infer_type" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| (Either String Type, [Trace]) |]
  -- FFI wrapper for checking a term against an expected type
  checkType <- functionBinding "check_type" ["entries", "term", "type"]
    <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]] <*> [t| (Maybe String, [Trace]) |]
  -- FFI wrapper for elaborating a term
  elaborate <- functionBinding "elaborate" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for removing implicit applications
  delaborate <- functionBinding "delaborate" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for delaborating against an expected type
  delaborateAgainst <- functionBinding "delaborate_against" ["entries", "term", "type"]
    <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for elaborating a term against an expected type
  elaborateAgainst <- functionBinding "elaborate_against" ["entries", "term", "type"]
    <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for unifying two terms
  unify <- functionBinding "unify" ["entries", "term1", "term2"]
    <$> sequence [[t| [Entry] |], [t|Term|], [t|Term|]] <*> [t| (Maybe String, [Trace]) |]
  -- FFI wrapper for computing weak head normal form
  whnf <- functionBinding "whnf" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for instantiating metavariables
  instantiateMVars <- functionBinding "instantiate_mvars" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for opening a binder
  unbind <- functionBinding "unbind" ["binding"]
    <$> sequence [[t| Unbound.Bind TermName Term |]] <*> [t| (TermName, Term) |]
  -- Helper to convert string to TH literal
  let stringify = TH.LitE . TH.StringL
  -- Collect all function bindings for export
  let functionBindings = [bind, unbind, pprTerm, typeCheck, inferType, checkType, elaborate, delaborate, delaborateAgainst, elaborateAgainst, unify, whnf, instantiateMVars]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
