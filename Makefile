all: purescript_main.exe
	dune exec ./purescript_main.exe test.pp

tests: purescript_main.exe
	for f in tests/*.logo; do dune exec ./purescript_main.exe $$f; done

miniturtle.exe:
	dune build purescript_main.exe

explain:
	menhir --base /tmp/parser --dump --explain purescript_parseur.mly
	cat /tmp/purescript_parser.conflicts

clean:
	dune clean

.PHONY: all clean explain purescript_main.exe
