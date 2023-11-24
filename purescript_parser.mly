%{
	open Purescript_ast
%}


%token NEWLINE MODULE IMPORT EOF


%start file

%type <Purescript_ast.file> file

%%


file:
	| MODULE NEWLINE+ IMPORT NEWLINE d=list(decl) EOF
		{ Printf.printf "%d\n" (List.length d); {imports = Import; decls = d} }
;
decl:
	| NEWLINE { Printf.printf "add decl\n" ; Dclass("C",["foo"],[Tarrow([],[],Patype(Auident("Int")))]) }
;
