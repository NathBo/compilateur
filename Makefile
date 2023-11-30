V: purescript_main.exe
	dune exec ./purescript_main.exe testV.purs
N: purescript_main.exe
	dune exec ./purescript_main.exe testN.purs


tests1: purescript_main.exe
	./run-tests.sh -1 ./purescript_main.exe
tests2: purescript_main.exe
	./run-tests.sh -2 ./purescript_main.exe

testsAll: purescript_main.exe
	./run-tests.sh -all ./purescript_main.exe


explain:
	menhir --base /tmp/parser --dump --explain purescript_parser.mly
	cat /tmp/purescript_parser.conflicts

clean:
	dune clean

.PHONY: all clean explain purescript_main.exe
