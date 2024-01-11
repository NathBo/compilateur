module Main where

import Prelude
import Effect
import Effect.Console

data Q = A Int | B
data T = U Q | N | V Q Q Int

f :: T -> String
f (U (A 3)) = "cas 1"
f (U (A _)) = "cas 2"
f (U B) = "cas 3"
f N = "cas 4"
f (V (A 2) B 3) = "cas 5"
f (V (A _) B 3) = "cas 6"
f (V B B 3) = "cas 7"
f (V (A 4) B _) = "cas 8"
f (V _ _ _) = "cas 9"

main :: Effect Unit
main = do log (f (U (A 3)))
          log (f (U (A 2)))
          log (f (U B))
          log (f N)
          log (f (V (A 2) B 3))
          log (f (V (A 3) B 3))
          log (f (V B B 3))
          log (f (V (A 4) B 2))
          log (f (V B (A 3) 1))

