%{
	open Purescript_ast
	let rec separe_fin = function
		| [] -> failwith "liste vide"
		| [a] -> ([],a)
		| [a;b] -> ([a],b)
		| a::b -> let (e,f) = separe_fin b in (a::e,f)
%}

%token LEFT_BLOCK RIGHT_BLOCK MIDLE_BLOCK
%token MODULE IMPORT EOF EQUAL LEFT_PAR RIGHT_PAR TRUE FALSE IN CASE OF ARROW DATA VBAR INSTANCE COMMA WHERE DOUBLE_ARROW DOUBLE_COLON FORALL DOT CLASS
%token MINUS PLUS TIMES DIVIDE DOUBLE_EQUAL DIV_EQUAL LESS LESS_E GREATER GREATER_E DIF AND_LOG OR_LOG
%token IF THEN ELSE DO LET
%token <Purescript_ast.lident> LIDENT
%token <Purescript_ast.lident> UIDENT
%token <string> STRING
%token <int> CONST_INT

%nonassoc IN ELSE
%left OR_LOG
%left AND_LOG
%nonassoc DOUBLE_EQUAL DIV_EQUAL GREATER GREATER_E LESS LESS_E 
%left MINUS PLUS DIF
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
	| CLASS u=UIDENT l=list(LIDENT) WHERE LEFT_BLOCK d=separated_list(MIDLE_BLOCK,tdecl) RIGHT_BLOCK { Printf.printf "FINI !\n";Dclass (u,l,d) }
	| INSTANCE i=instance WHERE LEFT_BLOCK x=separated_list(MIDLE_BLOCK, defn) RIGHT_BLOCK { Dinstance(i,x) }
;
(* for data : *)
uidentAtypeList:
	| u=UIDENT x=list(atype) { (u,x) }
;
defn:
	| lid=LIDENT a=list(patarg) EQUAL e=expr { {lident = lid; patargs = a; expr=e } }
;
tdecl:
	(*| l=LIDENT DOUBLE_COLON a=list(LIDENT) b=list(pairNtypeArrow) fin1=purtype fin2=list(pairPurTypeArrow)
			(* {{dlident="l"; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=(Patype (Alident "ll"))} } *)
		{ let c,d=separe_fin (fin1::fin2) in
			{dlident=l; lidentlist=a; ntypelist=b; purtypelist=c; purtype=d} } *)
	(*| l=LIDENT DOUBLE_COLON FORALL a=nonempty_list(LIDENT) DOT b=list(pairNtypeArrow) c=list(pairPurTypeArrow) d=purtype
			{ {dlident=l; lidentlist=a; ntypelist=b; purtypelist=c; purtype=d} }
	| l=LIDENT DOUBLE_COLON b=list(pairNtypeArrow) c=list(pairPurTypeArrow) d=purtype
			{ {dlident=l; lidentlist=[]; ntypelist=b; purtypelist=c; purtype=d} } *)
(*	| a=LIDENT DOUBLE_COLON c=list(pairNtypeArrow) fin1=purtype fin2=list(pairPurTypeArrow) 
			{Printf.printf "taille liste %d\n" (List.length (fin1::fin2));
 				let (x,y) = separe_fin (fin1::fin2) in 
				{dlident=a; lidentlist=[]; ntypelist=c; purtypelist=x; purtype=y} }
	| a=LIDENT DOUBLE_COLON FORALL b=nonempty_list(LIDENT) DOT c=list(pairNtypeArrow) fin1=purtype fin2=list(pairPurTypeArrow) 
			{ let (x,y) = separe_fin (fin1::fin2) in Printf.printf "taille liste %d\n" (List.length (fin1::fin2));
				{dlident=a; lidentlist=b; ntypelist=c; purtypelist=x; purtype=y} }
*)	
	
	| a=LIDENT DOUBLE_COLON b=UIDENT c=list(atype) d=list(pairArrowNtype) DOUBLE_ARROW e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }
	
	| a=LIDENT DOUBLE_COLON e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }


	| a=LIDENT DOUBLE_COLON FORALL x=nonempty_list(LIDENT) DOT b=UIDENT c=list(atype) d=list(pairArrowNtype) DOUBLE_ARROW e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }
	
	| a=LIDENT DOUBLE_COLON FORALL x=nonempty_list(LIDENT) DOT e=purtype f=list(pairArrowPurType)
			{ {dlident=a; lidentlist=[]; ntypelist=[]; purtypelist=[]; purtype=e} }

;
(* for tdecl *)
pairNtypeArrow:
	| n=ntype DOUBLE_ARROW { n }
;
pairArrowNtype:
	| DOUBLE_ARROW n=ntype {n}
;
pairArrowPurType:
	| ARROW p=purtype { p }
;
ntype:
	| u=UIDENT a=list(atype) { {uident = u ; atypes = a} } 
;
ntypeMany:
	| u=UIDENT a=nonempty_list(atype) { {uident = u ; atypes = a} } 
;

atype:
	| l=LIDENT { Alident l}
	| u=UIDENT { Auident u}
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
	| l=LIDENT { Plident l }
	| u=UIDENT { Puident u }
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
	| l=LIDENT { Alident l}
	| u=UIDENT {Auident u}
	| LEFT_PAR e=expr RIGHT_PAR { Aexpr e }
	| LEFT_PAR e=expr DOUBLE_COLON t=purtype RIGHT_PAR { Aexprtype (e,t) }

;

expr:
	| a=atom { Eatom a }
	| MINUS e=expr { Eminus e }
	| e1=expr b=binop e2=expr {Ebinop (b,e1,e2)}
	| lid=LIDENT atm=nonempty_list(atom) { Elident (lid,atm) }
	| uid=UIDENT atm=nonempty_list(atom) { Euident (uid,atm) }
	| IF e1=expr THEN e2=expr ELSE e3=expr { Eif (e1,e2,e3) }
	| DO LEFT_BLOCK l=separated_list(MIDLE_BLOCK, expr) RIGHT_BLOCK { Edo l }
	| LET LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,binding) RIGHT_BLOCK IN e=expr { Elet (l,e) }
	| CASE e=expr OF LEFT_BLOCK l=separated_nonempty_list(MIDLE_BLOCK,branch) RIGHT_BLOCK { Ecase (e,l) }
;
binding:
	| l=LIDENT EQUAL e=expr { {lident=l;expr=e} }
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
	| DIV_EQUAL {Bdivequal}
;
