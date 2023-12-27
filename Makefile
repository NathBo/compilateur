ppurs: src/purescript_main.ml src/purescript_ast.ml src/purescript_lexer.mll src/purescript_parser.mly src/purescript_typage.ml src/purescript_production_code.ml
	@dune build
	@mv src/purescript_main.exe ppurs

all: ppurs
	@./ppurs test.purs

tests1: ppurs
	@./run-tests.sh -1 ./ppurs
tests2: ppurs
	@./run-tests.sh -2 ./ppurs

testsAll: ppurs
	@./run-tests.sh -all ./ppurs
run: ppurs
	./ppurs test.purs 
	gcc -no-pie test.s -o test
	./test

clean:
	@dune clean
	@rm ppurs test.s test -f

.PHONY: all clean
