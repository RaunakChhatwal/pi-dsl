module Main where

import Bindings (dbg)

main :: IO ()
main = do
  putStrLn $(dbg)
