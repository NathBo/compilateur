module Main where
import Prelude
import Effect
import Effect.Console


class A where
  foo:: Int -> String ? forall a b
class B where
  bar:: Int -> String
instance A => B where
  bar n = foo n
main :: Effect Unit
main = log (bar 42)
