module Main where

import Prelude
import Effect
import Effect.Console


data List = Nil | Cons Int List


get:: List -> Int -> Int
get Nil        m = 42
get (Cons x l) n = n



main :: Effect Unit
main = log (show (get (Cons 0 Nil) 3))

