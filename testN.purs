module Main where
import Prelude
import Effect
import Effect.Console




data T = C Int Int
f:: T -> T -> Int
f (C x y) (C y z) = x
main :: Effect Unit
main = log ""
