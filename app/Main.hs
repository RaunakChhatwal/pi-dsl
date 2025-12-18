module Main where

import Language.Haskell.TH qualified as TH
import Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings)
import Environment (Env, Trace)
import Syntax (Entry, Term, Type, TName)
import qualified Unbound.Generics.LocallyNameless as Unbound

main :: IO ()
main = putStrLn $(do
  declOrder <- buildDeclOrder ''Env
  bindings <- mapM bindingFromName $ [''Maybe, ''Either, ''Trace] ++ declOrder
  bind <- functionBinding "bind" ["var", "body"]
    <$> sequence [[t|TName|], [t|Term|]] <*> [t|Unbound.Bind TName Term|]
  pprTerm <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  typeCheck <- functionBinding "type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| Maybe String |]
  traceTypeCheck <- functionBinding "trace_type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| (Maybe String, [Trace]) |]
  let stringify = TH.LitE . TH.StringL
  let functionBindings = [bind, pprTerm, typeCheck, traceTypeCheck]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
