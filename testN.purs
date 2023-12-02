module Main where
import Prelude
import Effect
import Effect.Console


data T = A Int Int | B

f:: T -> Int
f x = case x of
	A 0 _ -> 1
	A _ _ -> 0
	B -> 2
  
main::Effect Unit
main = log ""
