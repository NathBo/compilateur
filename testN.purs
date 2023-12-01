module Main where
import Prelude
import Effect
import Effect.Console



data T = A | B

main::T -> Effect Unit
main x = case x of A -> log "yes"
                   B -> log "no"



