module Main where
import Prelude
import Effect
import Effect.Console



data T = A Int | B Int

main::T -> Int
main x = case x of A x -> x
                   B  y -> z



