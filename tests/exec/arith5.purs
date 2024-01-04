module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (show (mod (-20) ( 3)))
  log (show (mod ( 20) (-3)))
  log (show (mod (-20) (-3)))
  log (show (mod ( 20) ( 3)))
  log (show (mod (-21) ( 3)))
  log (show (mod ( 21) (-3)))
  log (show (mod (-21) (-3)))
  log (show (mod ( 21) ( 3)))
