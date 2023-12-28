%{
	open Purescript_ast

	let triplet1 (a,b,c) = a
	let triplet2 (a,b,c) = b
	let triplet3 (a,b,c) = c
%}

%token LEFT_BLOCK RIGHT_BLOCK MIDLE_BLOCK
%token MODULE IMPORT EOF EQUAL LEFT_PAR RIGHT_PAR TRUE FALSE IN CASE OF ARROW DATA VBAR INSTANCE COMMA WHERE DOUBLE_ARROW DOUBLE_COLON FORALL DOT CLASS
%token MINUS PLUS TIMES DIVIDE DOUBLE_EQUAL LESS LESS_E GREATER GREATER_E DIF AND_LOG OR_LOG CONS
%token IF THEN ELSE DO LET
%token <Purescript_ast.ident> LIDENT
%token <Purescript_ast.ident> UIDENT
%token <string> STRING
%token <int> CONST_INT

%nonassoc IN ELSE
%left OR_LOG
%left AND_LOG
%nonassoc DOUBLE_EQUAL GREATER GREATER_E LESS LESS_E DIF
%left MINUS PLUS CONS
%left DIVIDE TIMES


%start file

%type <Purescript_ast.file> file

%%


file:
	| MIDLE_BLOCK* MODULE MIDLE_BLOCK IMPORT MIDLE_BLOCK d=separated_list(MIDLE_BLOCK,decl)  EOF
		{ let p = {startpos = $startpos ; endpos = $endpos } in 
			{imports = (Import p); decls = d; pos = p } }
;
decl:
	| d=defn { let p = {startpos = $startpos ; endpos = $endpos} in  Ddefn (d,p)}
	| t=tdecl { let p = {startpos = $startpos ; endpos = $endpos} in  Dtdecl (t,p) }
	| DATA u=UIDENT l=list(LIDENT) EQUAL x=separated_nonempty_list(VBAR, uidentAtypeList ) {let p = {startpos = $startpos ; endpos = $endpos} in  Ddata (u,l,x,p) }
	| CLASS u=UIDENT l=list(LIDENT) WHERE LEFT_BLOCK d=separated_list(MIDLE_BLOCK,tdecl) RIGHT_BLOCK {let p = {startpos = $startpos ; endpos = $endpos} in  Dclass (u,l,d,p) }
	| INSTANCE i=instance WHERE LEFT_BLOCK x=separated_list(MIDLE_BLOCK, defn) RIGHT_BLOCK {let p = {startpos = $startpos ; endpos = $endpos} in Dinstance(i,x,p) }
;
uidentAtypeList:
	| u=UIDENT x=list(atype) { (u,x) }
;
defn:
	| lid=LIDENT a=list(patarg) EQUAL e=expr { let p = {startpos = $startpos ; endpos = $endpos} in {ident = lid; patargs = a; expr=e; pos=p } }
;

tdecl:
	| a=LIDENT DOUBLE_COLON b=pairNtypeArrow
		{ let p = {startpos = $startpos ; endpos = $endpos} in
			{dident=a; identlist=[]; ntypelist=(triplet1 b); purtypelist=(triplet2 b); purtype=(triplet3 b); pos=p} }
	| a=LIDENT DOUBLE_COLON FORALL c=list(LIDENT) DOT b=pairNtypeArrow
		{ let p = {startpos = $startpos; endpos=$endpos} in {dident=a; identlist=c; ntypelist=(triplet1 b); purtypelist=(triplet2 b); purtype=(triplet3 b); pos=p } }
;

pairNtypeArrow:
	| a=ntype DOUBLE_ARROW b=pairNtypeArrow{ a::(triplet1 b),(triplet2 b),(triplet3 b) }
	| b=tdeclEnd { [],(fst b),(snd b) }
;
tdeclEnd:
	| a=purtype ARROW b=tdeclEnd { a::(fst b),(snd b) }
	| a=purtype { [],a }
;

ntype:
	| u=UIDENT a=list(atype) { let p = {startpos = $startpos ; endpos = $endpos} in {nident = u ; atypes = a; pos = p} } 
;
ntypeMany:
	| u=UIDENT a=nonempty_list(atype) { let p = {startpos = $startpos ; endpos = $endpos} in {nident = u ; atypes = a; pos = p} } 
;

atype:
	| l=LIDENT {let p = {startpos = $startpos ; endpos = $endpos} in  Aident (l,p)}
	| u=UIDENT {let p = {startpos = $startpos ; endpos = $endpos} in  Aident (u,p)}
	| LEFT_PAR t=purtype RIGHT_PAR { let p = {startpos = $startpos ; endpos = $endpos} in Apurtype (t,p) }
;
purtype:  (* un uident peut être vu comme un atype ou un ntype, j'ai choisi atype *)
	| a=atype { let p = {startpos = $startpos ; endpos = $endpos} in Patype (a,p)}
	| n=ntypeMany { let p = {startpos = $startpos ; endpos = $endpos} in Pntype (n,p)}
;
instance:
	| n=ntype {let p = {startpos = $startpos ; endpos = $endpos} in Intype (n,p)}
	| x=ntype DOUBLE_ARROW y=ntype {let p = {startpos = $startpos ; endpos = $endpos} in Iarrow (x,y,p) }
	| LEFT_PAR l=separated_nonempty_list(COMMA, ntype) RIGHT_PAR DOUBLE_ARROW n=ntype {let p = {startpos = $startpos ; endpos = $endpos} in Imularrow (l,n,p)}
;
patarg:
	| c=constant { let p = {startpos = $startpos ; endpos = $endpos} in Pconstant (c,p) }
	| l=LIDENT { let p = {startpos = $startpos ; endpos = $endpos} in Plident (l,p) }
	| u=UIDENT { let p = {startpos = $startpos ; endpos = $endpos} in Puident (u,p) }
	| LEFT_PAR a=pattern RIGHT_PAR { let p = {startpos = $startpos ; endpos = $endpos} in Ppattern (a,p) }
;
pattern:
	| a=patarg	{let p = {startpos = $startpos ; endpos = $endpos} in Ppatarg (a,p) }
	| u=UIDENT a=nonempty_list(patarg) { let p = {startpos = $startpos ; endpos = $endpos} in Pmulpatarg (u,a,p) }
;
constant:
	| i=CONST_INT {let p = {startpos = $startpos ; endpos = $endpos} in Cint (i,p)}
	| TRUE {let p = {startpos = $startpos ; endpos = $endpos} in Cbool (true,p)}
	| FALSE {let p = {startpos = $startpos ; endpos = $endpos} in Cbool (false,p)}
	| s=STRING {let p = {startpos = $startpos ; endpos = $endpos} in Cstring (s,p)}
;
atom :
	| c=constant { let p = {startpos = $startpos ; endpos = $endpos} in Aconstant (c,p) }
	| l=LIDENT { let p = {startpos = $startpos ; endpos = $endpos} in Alident (l,p)}
	| u=UIDENT { let p = {startpos = $startpos ; endpos = $endpos} in Auident (u,p)}
	| LEFT_PAR e=expr RIGHT_PAR { let p = {startpos = $startpos ; endpos = $endpos} in Aexpr (e,p) }
	| LEFT_PAR e=expr DOUBLE_COLON t=purtype RIGHT_PAR { let p = {startpos = $startpos ; endpos = $endpos} in Aexprtype (e,t,p) }
;

expr:
	| a=atom { let p = {startpos = $startpos ; endpos = $endpos} in Eatom (a,p) }
	| MINUS e=expr { let p = {startpos = $startpos ; endpos = $endpos} in Ebinop(Bminus p,Eatom(Aconstant(Cint (0,p),p),p),e,p) }
	| e1=expr b=binop e2=expr {let p = {startpos = $startpos ; endpos = $endpos} in Ebinop (b,e1,e2,p)}
	| lid=LIDENT atm=nonempty_list(atom) { let p = {startpos = $startpos ; endpos = $endpos} in Elident (lid,atm,p) }
	| uid=UIDENT atm=nonempty_list(atom) { let p = {startpos = $startpos ; endpos = $endpos} in Euident (uid,atm,p) }
	| IF e1=expr THEN e2=expr ELSE e3=expr { let p = {startpos = $startpos ; endpos = $endpos} in Eif (e1,e2,e3,p) }
	| DO LEFT_BLOCK l=separated_list(MIDLE_BLOCK, expr) RIGHT_BLOCK { let p = {startpos = $startpos ; endpos = $endpos} in Edo (l,p) }
	| LET LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,binding) RIGHT_BLOCK IN e=expr { let p = {startpos = $startpos ; endpos = $endpos} in Elet (l,e,p) }
	| CASE e=expr OF LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,branch) RIGHT_BLOCK { let p = {startpos = $startpos ; endpos = $endpos} in Ecase (e,l,p) }
;
binding:
	| l=LIDENT EQUAL e=expr { let p = {startpos = $startpos ; endpos = $endpos} in {ident=l;bindexpr=e;pos=p} }
;
branch:
	| a=pattern ARROW e=expr { let p = {startpos = $startpos ; endpos = $endpos} in {pattern=a ; expr=e; pos=p} } 
;
%inline binop:
	| DOUBLE_EQUAL {let p = {startpos = $startpos ; endpos = $endpos} in Bequals p}
	| DIF {let p = {startpos = $startpos ; endpos = $endpos} in Bnotequals p}
	| LESS {let p = {startpos = $startpos ; endpos = $endpos} in Binf p}
	| LESS_E {let p = {startpos = $startpos ; endpos = $endpos} in Binfeq p}
	| GREATER {let p = {startpos = $startpos ; endpos = $endpos} in Bsup p}
	| GREATER_E {let p = {startpos = $startpos ; endpos = $endpos} in Bsupeq p}
	| MINUS {let p = {startpos = $startpos ; endpos = $endpos} in Bminus p}
	| PLUS {let p = {startpos = $startpos ; endpos = $endpos} in Bplus p}
	| TIMES {let p = {startpos = $startpos ; endpos = $endpos} in Btimes p}
	| DIVIDE {let p = {startpos = $startpos ; endpos = $endpos} in Bdivide p}
	| AND_LOG {let p = {startpos = $startpos ; endpos = $endpos} in Band p}
	| OR_LOG {let p = {startpos = $startpos ; endpos = $endpos} in Bor p}
	| CONS {let p = {startpos = $startpos ; endpos = $endpos} in Bcons p}
;
