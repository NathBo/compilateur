%{
	open Purescript_ast
%}


%token PLUS
%token NEWLINE MODULE IMPORT EOF

%left T_PLUS



%start file

%type <Purescript_ast.file> file

%%


file:
	| MODULE
		{ {imports = Import; decls = [Dclass("C",["foo"],[Tarrow([],[],Patype(Auident("Int")))])]} }
;
