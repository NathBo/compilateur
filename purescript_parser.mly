%{
	open Purescript_ast
%}


%token NEWLINE MODULE IMPORT EOF EQUAL
%token <Purescript_ast.lident> LIDENT
%token <Purescript_ast.constant> CONSTANT


%start file

%type <Purescript_ast.file> file

%%


file:
	| MODULE NEWLINE+ IMPORT NEWLINE+ d=list(decl) EOF
		{ Printf.printf "%d\n" (List.length d); {imports = Import; decls = d} }
;
decl:
	| d=defn NEWLINE+ {Ddefn d}
;
defn:
	| lid=LIDENT EQUAL e=expr { {lident = lid; patargs = []; expr=e } }
;
expr:
	| a=atom { Eatom a }
;
atom :
	| c=CONSTANT { Aconstant c }
;
