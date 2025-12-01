module Main where

import Language.Haskell.TH qualified as TH
import Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings)
import Environment (Env)
import Syntax (Entry, Term, Type)

main :: IO ()
main = putStrLn $(do
  let stringify = TH.LitE . TH.StringL
  declOrder <- buildDeclOrder ''Env
  bindings <- (++) <$> mapM bindingFromName [''Maybe, ''Either] <*> mapM bindingFromName declOrder
  pprTermBinding <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  typeCheckBinding <- functionBinding "type_check" ["entries"]
    <$> sequence [[t| [Entry] |]] <*> [t| Maybe String |]
  let functionBindings = [pprTermBinding, typeCheckBinding]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
