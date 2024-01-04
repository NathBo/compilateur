module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log (show ((-20) / ( 3)))
  log (show (( 20) / (-3)))
  log (show ((-20) / (-3)))
  log (show (( 20) / ( 3)))
  log (show ((-21) / ( 3)))
  log (show (( 21) / (-3)))
  log (show ((-21) / (-3)))
  log (show (( 21) / ( 3)))
