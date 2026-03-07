module Main where

import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (lift)
import Language.Haskell.TH qualified as TH
import Unbound.Generics.LocallyNameless qualified as Unbound
import Bindings
import Environment
import Syntax

-- Entry point: prints generated Python bindings to stdout
main :: IO ()
main = putStrLn $(State.evalStateT (do
  let addBinding monad = do
        binding <- lift monad
        State.modify (binding:)

  declOrder <- lift $ buildDeclOrder ''Env

  mapM_ (addBinding . bindingFromName) $ [''Either, ''Trace] ++ declOrder ++ [''Entry]

  addBinding $ functionBinding "bind" ["var", "body"]
    <$> sequence [[t| TermName |], [t| Term |]] <*> [t| Unbound.Bind TermName Term |]

  addBinding $ functionBinding "unbind" ["binding"]
    <$> sequence [[t| Unbound.Bind TermName Term |]] <*> [t| (TermName, Term) |]

  addBinding $ functionBinding "ppr_term" ["term"]
    <$> sequence [[t| Term |]] <*> [t| String |]

  addBinding $ functionBinding "add_entry" ["env", "entry"]
    <$> sequence [[t| Env |], [t| Entry |]] <*> [t| (Either String Env, [Trace]) |]

  addBinding $ functionBinding "infer_type" ["env", "term"]
    <$> sequence [[t| Env |], [t| Term |]] <*> [t| (Either String Type, [Trace]) |]

  addBinding $ functionBinding "check_type" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t| Term |], [t| Type |]] <*> [t| (Maybe String, [Trace]) |]

  addBinding $ functionBinding "elaborate" ["env", "term"]
    <$> sequence [[t| Env |], [t| Term |]] <*> [t| (Either String Term, [Trace]) |]

  addBinding $ functionBinding "delaborate" ["env", "term"]
    <$> sequence [[t| Env |], [t| Term |]] <*> [t| (Either String Term, [Trace]) |]

  addBinding $ functionBinding "delaborate_against" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t| Term |], [t| Type |]] <*> [t| (Either String Term, [Trace]) |]

  addBinding $ functionBinding "elaborate_against" ["env", "term", "type"]
    <$> sequence [[t| Env |], [t| Term |], [t| Type |]] <*> [t| (Either String Term, [Trace]) |]

  addBinding $ functionBinding "unify" ["env", "term1", "term2"]
    <$> sequence [[t| Env |], [t| Term |], [t| Term |]] <*> [t| (Maybe String, [Trace]) |]

  addBinding $ functionBinding "whnf" ["env", "term"]
    <$> sequence [[t| Env |], [t| Term |]] <*> [t| (Either String Term, [Trace]) |]

  addBinding $ functionBinding "instantiate_mvars" ["env", "term"]
    <$> sequence [[t| Env |], [t| Term |]] <*> [t| (Either String Term, [Trace]) |]

  -- Helper to convert string to TH literal
  let stringify = TH.LitE . TH.StringL
  stringify . generateBindings . reverse <$> State.get) [])
