module Main where

import Prelude
import Effect
import Effect.Console


main :: Effect Unit
main = do log (show 2)
          log "abc"
          (let x = 3
               y = 4 in log (show (y-x)))

