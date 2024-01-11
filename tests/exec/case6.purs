module Main where

import Prelude
import Effect
import Effect.Console


f :: String -> String
f "aa" = "bb"
f s = s

main :: Effect Unit
main = do log (f "aa")
          log (f "cc")

