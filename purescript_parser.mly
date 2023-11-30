%{
	open Purescript_ast
	
(*	let genereDecl a b c d =
		let lst=(DOUBLE_ARROW,c)::d in
		let rec recupereFin = function
			| [] -> raise Parsing_error
			| [(ARROW,a)] -> [],a
			| (ARROW,t)::l -> let a,b = recupereFin l in t::a,b
			| (DOUBLE_ARROW,_)::_ -> raise Parsing_error
			| _ -> raise Parsing_error
		in
		let toNtype = function
			| Pntype a -> a
			| Patype (Alident _ ) | Patype (Apurtype _ ) -> Printf.printf "error 1\n";raise Parsing_error

			| Patype (Auident a) -> {uident=a; atypes=[]}
		in
		let rec splitList = function
			| [] -> raise Parsing_error
			| [(_,t)] -> [],[],t
			| (ARROW,t)::l ->
				let (f1,f2) = recupereFin l in
				[],t::f1,f2
			| (DOUBLE_ARROW,t1)::(DOUBLE_ARROW,t2)::l ->
					let t1' = toNtype t1 in let a,b,c = splitList ((DOUBLE_ARROW,t2)::l) in t1'::a,b,c
			| (DOUBLE_ARROW,t1)::(ARROW,t2)::l -> let fin1,fin2 = recupereFin ((ARROW,t2)::l) in [],t1::fin1,fin2
			| _ -> raise Parsing_error
		in
		Printf.printf "go split\n";		
		let x,y,z = splitList lst in Printf.printf "split fini\n";
			{dlident=a; lidentlist=b; ntypelist=x; purtypelist=y; purtype=z}
*)

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
		{ {imports = Import; decls = d} }
;
decl:
	| d=defn {Ddefn d}
	| t=tdecl {Dtdecl t}
	| DATA u=UIDENT l=list(LIDENT) EQUAL x=separated_nonempty_list(VBAR, uidentAtypeList ) { Ddata (u,l,x) }
	| CLASS u=UIDENT l=list(LIDENT) WHERE LEFT_BLOCK d=separated_list(MIDLE_BLOCK,tdecl) RIGHT_BLOCK { Dclass (u,l,d) }
	| INSTANCE i=instance WHERE LEFT_BLOCK x=separated_list(MIDLE_BLOCK, defn) RIGHT_BLOCK { Dinstance(i,x) }
;
(* for data : *)
uidentAtypeList:
	| u=UIDENT x=list(atype) { (u,x) }
;
defn:
	| lid=LIDENT a=list(patarg) EQUAL e=expr { {ident = lid; patargs = a; expr=e } }
;

tdecl:
	(*| a=LIDENT DOUBLE_COLON d=list(pairNtypeArrow) e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }
	
	| a=LIDENT DOUBLE_COLON e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} } *)
	


(*	| a=LIDENT DOUBLE_COLON FORALL x=nonempty_list(LIDENT) DOT b=UIDENT c=list(atype) d=list(pairArrowNtype) DOUBLE_ARROW e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }
	
	| a=LIDENT DOUBLE_COLON FORALL x=nonempty_list(LIDENT) DOT e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }
*)
	| a=LIDENT
		{ {dident=a; identlist=[]; ntypelist=[]; purtypelist=[]; purtype=Patype (Aident "a") } }
	(*
	| a=LIDENT DOUBLE_COLON c=purtype d=list(pairArrowType)	{ genereDecl a [] c d }
	| a=LIDENT DOUBLE_COLON FORALL b=nonempty_list(LIDENT) DOT c=purtype d=list(pairArrowType)	{ genereDecl a b c d }
	*)

;
(* for tdecl *)

(*pairArrowType:
	| ARROW t=purtype { (ARROW,t) }
	| DOUBLE_ARROW t=purtype { (DOUBLE_ARROW,t) }
*)
ntype:
	| u=UIDENT a=list(atype) { {nident = u ; atypes = a} } 
;
ntypeMany:
	| u=UIDENT a=nonempty_list(atype) { {nident = u ; atypes = a} } 
;

atype:
	| l=LIDENT { Aident l}
	| u=UIDENT { Aident u}
	| LEFT_PAR t=purtype RIGHT_PAR { Apurtype t }
;
purtype:  (* TODO : un uident peut être vu comme un atype ou un ntype, j'ai fait un choix aleatoire *)
	| a=atype {Patype a}
	| n=ntypeMany {Pntype n}
;
instance:
	| n=ntype { Intype n}
	| x=ntype DOUBLE_ARROW y=ntype {Iarrow (x,y) }
	| LEFT_PAR l=separated_nonempty_list(COMMA, ntype) RIGHT_PAR DOUBLE_ARROW n=ntype {Imularrow (l,n)}
;
patarg:
	| c=constant { Pconstant c }
	| l=LIDENT { Pident l }
	| u=UIDENT { Pident u }
	| LEFT_PAR p=pattern RIGHT_PAR { Ppattern p }
;
pattern:
	| p=patarg	{ Ppatarg p }
	| u=UIDENT p=nonempty_list(patarg) { Pmulpatarg (u,p) }
;
constant:
	| i=CONST_INT {Cint i}
	| TRUE {Cbool true}
	| FALSE {Cbool false}
	| s=STRING {Cstring s}
;
atom :
	| c=constant { Aconstant c }
	| l=LIDENT { Aident l}
	| u=UIDENT { Aident u}
	| LEFT_PAR e=expr RIGHT_PAR { Aexpr e }
	| LEFT_PAR e=expr DOUBLE_COLON t=purtype RIGHT_PAR { Aexprtype (e,t) }

;

expr:
	| a=atom { Eatom a }
	| MINUS e=expr { Ebinop(Binf,Eatom(Aconstant(Cint 0)),e) }
	| e1=expr b=binop e2=expr {Ebinop (b,e1,e2)}
	| lid=LIDENT atm=nonempty_list(atom) { Eident (lid,atm) }
	| uid=UIDENT atm=nonempty_list(atom) { Eident (uid,atm) }
	| IF e1=expr THEN e2=expr ELSE e3=expr { Eif (e1,e2,e3) }
	| DO LEFT_BLOCK l=separated_list(MIDLE_BLOCK, expr) RIGHT_BLOCK { Edo l }
	| LET LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,binding) RIGHT_BLOCK IN e=expr { Elet (l,e) }
	| CASE e=expr OF LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,branch) RIGHT_BLOCK { Ecase (e,l) }
;
binding:
	| l=LIDENT EQUAL e=expr { {ident=l;expr=e} }
;
branch:
	| p=pattern ARROW e=expr { {pattern=p ; expr=e} } 
;
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
	| CONS {Bcons}
;
