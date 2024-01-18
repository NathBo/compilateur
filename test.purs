module Main where

import Prelude
import Effect
import Effect.Console

class C where
  foo:: Int -> String
  bar:: Boolean -> String

instance C where
  foo _ = "a"
  bar _ = "b"

main :: Effect Unit
main = do log (foo 1)
          log (bar true)


