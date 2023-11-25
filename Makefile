all: purescript_main.exe
	dune exec ./purescript_main.exe test.purs

tests: purescript_main.exe
	./run-tests.sh -all ./purescript_main.exe

miniturtle.exe:
	dune build purescript_main.exe

explain:
	menhir --base /tmp/parser --dump --explain purescript_parser.mly
	cat /tmp/purescript_parser.conflicts

clean:
	dune clean

.PHONY: all clean explain purescript_main.exe
