module Main where

import Language.Haskell.TH qualified as TH
import Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings)
import Environment (Env, Trace)
import Syntax (Entry, Term, Type, TermName)
import qualified Unbound.Generics.LocallyNameless as Unbound

main :: IO ()
main = putStrLn $(do
  declOrder <- buildDeclOrder ''Env
  bindings <- mapM bindingFromName $ [''Either, ''Trace] ++ declOrder ++ [''Entry]
  bind <- functionBinding "bind" ["var", "body"]
    <$> sequence [[t|TermName|], [t|Term|]] <*> [t|Unbound.Bind TermName Term|]
  pprTerm <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  typeCheck <- functionBinding "type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| Maybe String |]
  traceTypeCheck <- functionBinding "trace_type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| (Maybe String, [Trace]) |]
  inferType <- functionBinding "infer_type" ["entries", "term"]
    <$> sequence [[t| [Entry] |], [t|Term|]] <*> [t| Either Type String |]
  checkType <- functionBinding "check_type" ["entries", "term", "type"]
    <$> sequence [[t| [Entry] |], [t|Term|], [t|Type|]] <*> [t| Maybe String |]
  unbind <- functionBinding "unbind" ["binding"]
    <$> sequence [[t|Unbound.Bind TermName Term|]] <*> [t| (TermName, Term) |]
  let stringify = TH.LitE . TH.StringL
  let functionBindings = [bind, unbind, pprTerm, typeCheck, traceTypeCheck, inferType, checkType]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
