type position =
	{startpos: Lexing.position; endpos: Lexing.position}

and file =
	{imports : imports; decls : decl list; pos : position}


and imports = Import of position

and decl =
	| Ddefn of defn * position
	| Dtdecl of tdecl * position
	| Ddata of ident * (ident list) * ((ident * (atype list)) list) * position
	| Dclass of ident * (ident list) * tdecl list * position
	| Dinstance of instance * defn list * position

and defn =
	{ident : ident; patargs : patarg list; expr : expr; pos : position}

and tdecl =
	{dident : ident; identlist : ident list; ntypelist : ntype list; purtypelist : purtype list; purtype : purtype; pos : position}

and ntype =
	{nident : ident; atypes : atype list; pos : position}

and atype =
	| Aident of ident * position
	| Apurtype of purtype * position

and purtype =		 (*remplace type car type est un mot clef en Ocaml*)
	| Patype of atype * position
	| Pntype of ntype * position

and instance =
	| Intype of ntype * position
	| Iarrow of ntype * ntype * position
	| Imularrow of ntype list * ntype * position

and patarg =
	| Pconstant of constant * position
	| Plident of ident * position
	| Puident of ident * position
	| Ppattern of pattern * position

and pattern =
	| Ppatarg of patarg * position
	| Pmulpatarg of ident * patarg list * position

and constant = 
	| Cbool of bool * position
	| Cint of int * position
	| Cstring of string * position

and atom =
	| Aconstant of constant * position
	| Alident of ident * position
	| Auident of ident * position
	| Aexpr of expr * position
	| Aexprtype of expr * purtype * position

and expr =
	| Eatom of atom * position
	| Ebinop of binop * expr * expr * position
	| Elident of ident * atom list	* position
	| Euident of ident * atom list	* position
	| Eif of expr * expr * expr * position
	| Edo of expr list * position
	| Elet of binding list * expr * position
	| Ecase of expr * branch list * position

and binding =
	{ident : ident; bindexpr : expr; pos : position}

and branch =
	{pattern : pattern; expr : expr; pos : position}

and binop =
	| Bequals of position
	| Bnotequals of position
	| Binf of position
	| Binfeq of position
	| Bsup of position
	| Bsupeq of position
	| Bplus	of position
	| Bminus of position
	| Btimes of position
	| Bdivide of position
	| Band of position
	| Bor of position
	| Bcons of position


and ident = string


open Format

let string_of_bool b =
	if b then "true"
	else "false"

let print_constant fmt c = match c with
	| Cbool (b,_) -> fprintf fmt "boolean %s" (string_of_bool b)
	| Cint (n,_) -> fprintf fmt "int %d" n
	| Cstring (s,_) -> fprintf fmt "string %s" s

let print_binop fmt b = match b with
	| Bdivide _ -> fprintf fmt "/"
	| Bequals _ -> fprintf fmt "=="
	| Binf _ -> fprintf fmt "<"
	| Binfeq _ -> fprintf fmt	"<="
	| Bminus _ -> fprintf fmt "-"
	| Bnotequals _ -> fprintf fmt "!="
	| Bplus _ -> fprintf fmt "+"
	| Bsup _ -> fprintf fmt ">"
	| Bsupeq _ -> fprintf fmt ">="
	| Btimes _ -> fprintf fmt "*"
	| Band _ -> fprintf fmt "&&"
	| Bor _ -> fprintf fmt "||"
	| Bcons _ -> fprintf fmt "<>"

let print_ident fmt s =
	fprintf fmt "%s" s


let rec print_atom fmt a = match a with
	| Aconstant (c,_) -> fprintf fmt "%a" print_constant c
	| Alident (l,_) | Auident (l,_) -> fprintf fmt "%s" l
	| Aexpr (e,_) -> fprintf fmt "%a" print_expr e
	| Aexprtype (e,t,_) -> fprintf fmt "%a :: %a" print_expr e print_purtype t

and print_expr fmt e = match e with
	| Ebinop (b,e1,e2,_) -> fprintf fmt "(%a %a %a)" print_expr e1 print_binop b print_expr e2
	| Elident (s,a,_) | Euident (s,a,_) -> fprintf fmt "%s [@[<hov>%a@]]" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_atom) a
	| Eif (e1,e2,e3,_) -> fprintf fmt "if %a then %a else %a" print_expr e1 print_expr e2 print_expr e3
	| Edo (e,_) -> fprintf fmt "do {@[<hov>%a@]}" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_expr) e
	| Elet (b,e,_) -> fprintf fmt "let {@[<hov>%a@]} in %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_bindings) b print_expr e
	| Eatom (a,_) -> print_atom fmt a
	| Ecase (e,b,_) -> fprintf fmt "case %a of {@[<hov>%a@]}" print_expr e	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@") print_branch) b

and print_bindings fmt b =
	fprintf fmt "%s = %a" b.ident print_expr b.bindexpr

and print_branch fmt b =
	fprintf fmt "%a -> %a" print_pattern b.pattern print_expr b.expr

and print_patarg fmt p =match p with
	| Pconstant (c,_) -> fprintf fmt "%a" print_constant c
	| Plident (s,_) | Puident (s,_) -> fprintf fmt "%s" s
	| Ppattern (p,_) -> fprintf fmt "%a" print_pattern p

and print_pattern fmt p = match p with
	| Ppatarg (p,_) -> fprintf fmt "%a" print_patarg p
	| Pmulpatarg (s,plist,_) -> fprintf fmt "%s(@[<hov>%a@])" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_patarg) plist

and print_atype fmt a = match a with
	| Aident (s,_) -> fprintf fmt "%s" s
	| Apurtype (p,_) -> fprintf fmt "%a" print_purtype p

and print_ntype fmt n =
	fprintf fmt "%s(@[<hov>%a@])" n.nident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_atype) n.atypes

and print_purtype fmt p = match p with
	| Patype (a,_) -> fprintf fmt "%a" print_atype a
	| Pntype (n,_) -> fprintf fmt "%a" print_ntype n

and print_tdecl fmt t =
	fprintf fmt "%s :: forall @[<hov>%a@] (@[<hov>%a@]) => (@[<hov>%a@]) -> %a" t.dident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) t.identlist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ntype) t.ntypelist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_purtype) t.purtypelist print_purtype t.purtype

and print_instance fmt i = match i with
	| Intype (n,_) -> fprintf fmt "%a" print_ntype n
	| Iarrow (n1,n2,_) -> fprintf fmt "%a => %a" print_ntype n1 print_ntype n2
	| Imularrow (nlist,n,_) -> fprintf fmt "(@[<hov>%a@]) => %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") print_ntype) nlist print_ntype n

and print_defn fmt d =
	fprintf fmt "%s (@[<hov>%a@]) = %a" d.ident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_patarg) d.patargs print_expr d.expr

and print_ualist fmt (u,alist) =
	fprintf fmt "%s (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_atype) alist

and print_decl fmt d = match d with
	| Ddata (u,llist,ua,_) -> fprintf fmt "typconstant env envtyps envinstances c%s (@[<hov>%a@]) = (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) llist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ualist) ua
	| Dclass (u,llist,tlist,_) -> fprintf fmt "%s (@[<hov>%a@]) where {@[<hov>%a@]}" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) llist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tdecl) tlist
	| Dinstance (i,dlist,_) -> fprintf fmt "%a where {[<hov>%a@]}" print_instance i Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_defn) dlist
	| Dtdecl (t,_) -> fprintf fmt "%a" print_tdecl t
	| Ddefn (d,_) -> fprintf fmt "%a" print_defn d

and print_file fmt f =
	fprintf fmt "@[<hov>%a@]" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")print_decl) f.decls
