module Main where
import Prelude
import Effect
import Effect.Console


class C a b where
	f::String
instance C a b where
	f = "a"
instance C Int String where
	f = "b"

main :: Effect Unit
main = log "ok"
