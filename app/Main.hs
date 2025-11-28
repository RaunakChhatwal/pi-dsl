module Main where

import Language.Haskell.TH qualified as TH
import Bindings (bindingFromName, buildDeclOrder, functionBinding, generateBindings)
import Environment (Env)
import Syntax (Term, Type)

main :: IO ()
main = putStrLn $(do
  let stringify = TH.LitE . TH.StringL
  declOrder <- buildDeclOrder ''Env
  bindings <- (++) <$> mapM bindingFromName [''Maybe, ''Either] <*> mapM bindingFromName declOrder
  pprTermBinding <- functionBinding "ppr_term" ["term"]
    <$> sequence [[t|Term|]] <*> [t|String|]
  inferTypeBinding <- functionBinding "infer_type" ["env", "term"]
    <$> sequence [[t|Env|], [t|Term|]] <*> [t| Either String Type |]
  checkTypeBinding <- functionBinding "check_type" ["env", "term", "ty"]
    <$> sequence [[t|Env|], [t|Term|], [t|Type|]] <*> [t| Maybe String |]
  let functionBindings = [pprTermBinding, inferTypeBinding, checkTypeBinding]
  return $ stringify $ generateBindings (bindings ++ functionBindings))
