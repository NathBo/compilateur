module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = log (show (let x = "a" in (x+2)))
