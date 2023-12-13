ppurs: purescript_main.ml purescript_ast.ml purescript_lexer.mll purescript_parser.mly purescript_typage.ml
	@dune build
	@mv purescript_main.exe ppurs

all: ppurs
	@./ppurs test.purs

tests1: ppurs
	@./run-tests.sh -1 ./ppurs
tests2: ppurs
	@./run-tests.sh -2 ./ppurs

testsAll: ppurs
	@./run-tests.sh -all ./ppurs

clean:
	@dune clean
	@rm ppurs -f

.PHONY: all clean
