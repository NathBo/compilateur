module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do log (if 1 < 2 then "a" else "b")
          log (if 3 > 4 then "b" else "a")
          log (if 3 >= 4 then "b" else "a")
          log (if 1 <= 2 then "a" else "b")
          log (if 1 <= 1 then "a" else "b")
          log (if 1 >= 1 then "a" else "b")
