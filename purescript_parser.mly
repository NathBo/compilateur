%{
	open Purescript_ast
%}

%token LEFT_BLOCK RIGHT_BLOCK MIDLE_BLOCK
%token MODULE IMPORT EOF EQUAL LEFT_PAR RIGHT_PAR TRUE FALSE
%token MINUS PLUS TIMES DIVIDE
%token IF THEN ELSE DO
%token <Purescript_ast.lident> LIDENT
%token <int> CONST_INT

%left MINUS PLUS
%left DIVIDE TIMES


%start file

%type <Purescript_ast.file> file

%%


file:
	| MIDLE_BLOCK* MODULE MIDLE_BLOCK+ IMPORT MIDLE_BLOCK* d=separated_list(MIDLE_BLOCK,decl) MIDLE_BLOCK* EOF
		{ {imports = Import; decls = d} }
;
decl:
	| d=defn {Ddefn d}
;
defn:
	| lid=LIDENT EQUAL e=expr { {lident = lid; patargs = []; expr=e } }
;
expr:
	| a=atom { Eatom a }
	| MINUS e=expr { Eminus e }
	| e1=expr b=binop e2=expr {Ebinop (b,e1,e2)}
	| DO LEFT_BLOCK l=separated_list(MIDLE_BLOCK, expr) RIGHT_BLOCK { Edo l }
;
atom :
	| c=constant { Aconstant c }
	| LEFT_PAR e=expr RIGHT_PAR { Aexpr e }
;
constant:
	| i=CONST_INT {Cint i}
	| TRUE {Cbool true}
	| FALSE {Cbool false}
%inline binop:
	| MINUS { Bminus }
	| PLUS { Bplus }
	| TIMES { Btimes }
	| DIVIDE { Bdivide }
;
