module Main where

import Prelude
import Effect
import Effect.Console

f :: Int -> Int -> Int
f x y = x + 2*y

main :: Effect Unit
main = log (show (let g = f 3 in g 4))
