%{
	open Purescript_ast
%}


%token NEWLINE MODULE IMPORT EOF EQUAL
%token MINUS PLUS TIMES DIVIDE
%token <Purescript_ast.lident> LIDENT
%token <Purescript_ast.constant> CONSTANT

%left MINUS PLUS
%left DIVIDE TIMES


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
	| MINUS e=expr { Eminus e }
	| e1=expr b=binop e2=expr {Ebinop (b,e1,e2)}
;
atom :
	| c=CONSTANT { Aconstant c }
;
%inline binop:
	| MINUS { Bminus }
	| PLUS { Bplus }
	| TIMES { Btimes }
	| DIVIDE { Bdivide }
;
