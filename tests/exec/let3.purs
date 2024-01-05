module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = log (let x = "a" in x)
