%{
	open Purescript_ast
%}


%token EOF T_Module T_case T_class T_data T_do T_else T_false T_forall T_if T_import T_in T_instance T_let T_module T_of T_then T_true T_where
%token <Purescript_ast.constant> T_cst
%token T_Plus
%token NEWLINE

%left T_Plus



%start file

%type <Purescript_ast.file> file

%%


file:
	| NEWLINE* "Module Main where" imp=list(imports) decl=nonempty_list(decl) NEWLINE* EOF
		{imports = imp, decls = decl}
;
