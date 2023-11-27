%{
	open Purescript_ast
%}

%token LEFT_BLOCK RIGHT_BLOCK MIDLE_BLOCK
%token MODULE IMPORT EOF EQUAL LEFT_PAR RIGHT_PAR TRUE FALSE IN CASE OF ARROW DATA VBAR INSTANCE COMMA WHERE DOUBLE_ARROW
%token MINUS PLUS TIMES DIVIDE DOUBLE_EQUAL DIV_EQUAL LESS LESS_E GREATER GREATER_E DIF AND_LOG OR_LOG
%token IF THEN ELSE DO LET
%token <Purescript_ast.lident> LIDENT
%token <Purescript_ast.lident> UIDENT
%token <string> STRING
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
	| DATA u=UIDENT l=list(LIDENT) EQUAL x=separated_nonempty_list(VBAR, uidentAtypeList ) { Ddata (u,l,x) }
	| INSTANCE i=instance WHERE LEFT_BLOCK x=separated_list(MIDLE_BLOCK, defn) RIGHT_BLOCK { Dinstance(i,x) }
;
(* for data : *)
uidentAtypeList:
	| u=UIDENT x=list(atype) { (u,x) }
;
defn:
	| lid=LIDENT a=list(patarg) EQUAL e=expr { {lident = lid; patargs = a; expr=e } }
;
ntype:
	| u=UIDENT a=list(atype) { {uident = u ; atypes = a} }
atype:
	| l=LIDENT { Alident l}
	| u=UIDENT { Auident u}
;
instance:
	| n=ntype { Intype n}
	| x=ntype DOUBLE_ARROW y=ntype {Iarrow (x,y) }
	| LEFT_PAR l=separated_nonempty_list(COMMA, ntype) RIGHT_PAR DOUBLE_ARROW n=ntype {Imularrow (l,n)}
;
expr:
	| a=atom { Eatom a }
	| MINUS e=expr { Eminus e }
	| e1=expr b=binop e2=expr {Ebinop (b,e1,e2)}
	| lid=LIDENT atm=nonempty_list(atom) { Elident (lid,atm) }
	| uid=UIDENT atm=nonempty_list(atom) { Euident (uid,atm) }
	| DO LEFT_BLOCK l=separated_list(MIDLE_BLOCK, expr) RIGHT_BLOCK { Edo l }
	| LET LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,binding) RIGHT_BLOCK IN e=expr { Elet (l,e) }
	| CASE e=expr OF LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,branch) RIGHT_BLOCK { Ecase (e,l) }
	| IF e1=expr THEN e2=expr ELSE e3=expr { Eif (e1,e2,e3) }
;
atom :
	| c=constant { Aconstant c }
	| LEFT_PAR e=expr RIGHT_PAR { Aexpr e }
;
constant:
	| i=CONST_INT {Cint i}
	| TRUE {Cbool true}
	| FALSE {Cbool false}
	| s=STRING {Cstring s}
;

binding:
	| l=LIDENT EQUAL e=expr { {lident=l;expr=e} }
;
branch:
	| p=pattern ARROW e=expr { {pattern=p ; expr=e} } 
;
pattern:
	| p=patarg	{ Ppatarg p }
;
patarg:
	| c=constant { Pconstant c }
	| l=LIDENT { Plident l }
	| u=UIDENT { Puident u }
	| LEFT_PAR p=pattern RIGHT_PAR { Ppattern p }

%inline binop:
	| DOUBLE_EQUAL {Bequals}
	| DIF {Bnotequals}
	| LESS {Binf}
	| LESS_E {Binfeq}
	| GREATER {Bsup}
	| GREATER_E {Bsupeq}
	| MINUS { Bminus }
	| PLUS { Bplus }
	| TIMES { Btimes }
	| DIVIDE { Bdivide }
	| AND_LOG {Band}
	| OR_LOG {Bor}
;
