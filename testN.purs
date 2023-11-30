module Main where
import Prelude
import Effect
import Effect.Console


f:: forall a b. a -> b -> b
f x y = x
