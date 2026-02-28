module Main where

import Language.Haskell.TH qualified as TH
import Unbound.Generics.LocallyNameless qualified as Unbound
import Bindings
import Environment
import Syntax

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
  -- FFI wrapper for converting entries to Env without checking
  entriesToEnv <- functionBinding "entries_to_env" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| Env |]
  -- FFI wrapper for checking one entry against an existing environment
  checkEntry <- functionBinding "check_entry" ["env", "entry"]
    <$> sequence [[t| Env |], [t| Entry |]] <*> [t| (Either String Entry, [Trace]) |]
  -- FFI wrapper for inferring the type of a term
  inferType <- functionBinding "infer_type" ["env", "term"]
    <$> sequence [[t| Env |], [t|Term|]] <*> [t| (Either String Type, [Trace]) |]
  -- FFI wrapper for checking a term against an expected type
  checkType <- functionBinding "check_type" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t|Term|], [t|Type|]] <*> [t| (Maybe String, [Trace]) |]
  -- FFI wrapper for elaborating a term
  elaborate <- functionBinding "elaborate" ["env", "term"]
    <$> sequence [[t| Env |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for removing implicit applications
  delaborate <- functionBinding "delaborate" ["env", "term"]
    <$> sequence [[t| Env |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for delaborating against an expected type
  delaborateAgainst <- functionBinding "delaborate_against" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t|Term|], [t|Type|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for elaborating a term against an expected type
  elaborateAgainst <- functionBinding "elaborate_against" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t|Term|], [t|Type|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for unifying two terms
  unify <- functionBinding "unify" ["env", "term1", "term2"]
    <$> sequence [[t| Env |], [t|Term|], [t|Term|]] <*> [t| (Maybe String, [Trace]) |]
  -- FFI wrapper for computing weak head normal form
  whnf <- functionBinding "whnf" ["env", "term"]
    <$> sequence [[t| Env |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for instantiating metavariables
  instantiateMVars <- functionBinding "instantiate_mvars" ["env", "term"]
    <$> sequence [[t| Env |], [t|Term|]] <*> [t| (Either String Term, [Trace]) |]
  -- FFI wrapper for opening a binder
  unbind <- functionBinding "unbind" ["binding"]
    <$> sequence [[t| Unbound.Bind TermName Term |]] <*> [t| (TermName, Term) |]
  -- Helper to convert string to TH literal
  let stringify = TH.LitE . TH.StringL
  -- Collect all function bindings for export
  let functionBindings = [bind, unbind, pprTerm, entriesToEnv, checkEntry, inferType, checkType, elaborate, delaborate, delaborateAgainst, elaborateAgainst, unify, whnf, instantiateMVars]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
