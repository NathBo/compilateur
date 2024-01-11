module Main where

import Prelude
import Effect
import Effect.Console

data Q = A | B
data T = U Q | V

f :: T -> String
f V = ">>>V<<<<"
f (U A) = ">>>>U A<<<<"
f (U B) = ">>>>U B<<<<"

main :: Effect Unit
main = do log (f (U B))
          log (f (U A))
          log (f (V))

