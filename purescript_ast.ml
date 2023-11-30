type file =
  {imports : imports; decls : decl list}

and imports = Import

and decl =
  | Ddefn of defn
  | Dtdecl of tdecl
  | Ddata of ident * (ident list) * ((ident * (atype list)) list)
  | Dclass of ident * (ident list) * tdecl list
  | Dinstance of instance * defn list

and defn =
  {ident : ident; patargs : patarg list; expr : expr}

and tdecl =
  {dident : ident; identlist : ident list; ntypelist : ntype list; purtypelist : purtype list; purtype : purtype}

and ntype =
  {nident : ident; atypes : atype list}

and atype =
  | Aident of ident
  | Apurtype of purtype

and purtype =     (*remplace type car type est un mot clef en Ocaml*)
  | Patype of atype
  | Pntype of ntype

and instance =
  | Intype of ntype
  | Iarrow of ntype * ntype
  | Imularrow of ntype list * ntype

and patarg =
  | Pconstant of constant
  | Pident of ident
  | Ppattern of pattern

and pattern =
  | Ppatarg of patarg
  | Pmulpatarg of ident * patarg list

and constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

and atom =
  | Aconstant of constant
  | Aident of ident
  | Aexpr of expr
  | Aexprtype of expr * purtype

and expr =
  | Eatom of atom
  | Ebinop of binop * expr * expr
  | Eident of ident * atom list 
  | Eif of expr * expr * expr
  | Edo of expr list
  | Elet of binding list * expr
  | Ecase of expr * branch list

and binding =
  {ident : ident; bindexpr : expr}

and branch =
  {pattern : pattern; expr : expr}

and binop = Bequals | Bnotequals | Binf | Binfeq | Bsup | Bsupeq | Bplus | Bminus | Btimes | Bdivide | Band | Bor | Bcons


and ident = string


open Format

let string_of_bool b =
  if b then "true"
  else "false"

let print_constant fmt c = match c with
  | Cbool b -> fprintf fmt "%s" (string_of_bool b)
  | Cint n -> fprintf fmt "%d" n
  | Cstring s -> fprintf fmt "%s" s

let print_binop fmt b = match b with
  | Bdivide -> fprintf fmt "/"
  | Bequals -> fprintf fmt "=="
  | Binf -> fprintf fmt "<"
  | Binfeq -> fprintf fmt  "<="
  | Bminus -> fprintf fmt "-"
  | Bnotequals -> fprintf fmt "!="
  | Bplus -> fprintf fmt "+"
  | Bsup -> fprintf fmt ">"
  | Bsupeq -> fprintf fmt ">="
  | Btimes -> fprintf fmt "*"
  | Band -> fprintf fmt "&&"
  | Bor -> fprintf fmt "||"
  | Bcons -> fprintf fmt "<>"

let print_ident fmt s =
  fprintf fmt "%s" s


let rec print_atom fmt a = match a with
  | Aconstant c -> fprintf fmt "%a" print_constant c
  | Aident l -> fprintf fmt "%s" l
  | Aexpr e -> fprintf fmt "%a" print_expr e
  | Aexprtype (e,t) -> fprintf fmt "%a :: %a" print_expr e print_purtype t

and print_expr fmt e = match e with
  | Ebinop (b,e1,e2) -> fprintf fmt "(%a %a %a)" print_expr e1 print_binop b print_expr e2
  | Eident (s,a) -> fprintf fmt "%s [@[<hov>%a@]]" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_atom) a
  | Eif (e1,e2,e3) -> fprintf fmt "if %a then %a else %a" print_expr e1 print_expr e2 print_expr e3
  | Edo e -> fprintf fmt "do {@[<hov>%a@]}" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_expr) e
  | Elet (b,e) -> fprintf fmt "let {@[<hov>%a@]} in %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_bindings) b print_expr e
  | Eatom a -> print_atom fmt a (* TODO : a modifier j'ai rajouté ça vite fait sans bien comprendre la syntaxe *)
  | Ecase (e,b) -> fprintf fmt "case %a of {@[<hov>%a@]}" print_expr e  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@") print_branch) b

and print_bindings fmt b =
  fprintf fmt "%s = %a" b.ident print_expr b.bindexpr

and print_branch fmt b =
  fprintf fmt "%a -> %a" print_pattern b.pattern print_expr b.expr

and print_patarg fmt p =match p with
  | Pconstant c -> fprintf fmt "%a" print_constant c
  | Pident s -> fprintf fmt "%s" s
  | Ppattern p -> fprintf fmt "%a" print_pattern p

and print_pattern fmt p = match p with
  | Ppatarg p -> fprintf fmt "%a" print_patarg p
  | Pmulpatarg (s,plist) -> fprintf fmt "%s(@[<hov>%a@])" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_patarg) plist

and print_atype fmt a = match a with
  | Aident s -> fprintf fmt "%s" s
  | Apurtype p -> fprintf fmt "%a" print_purtype p

and print_ntype fmt n =
  fprintf fmt "%s(@[<hov>%a@])" n.nident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_atype) n.atypes

and print_purtype fmt p = match p with
  | Patype a -> fprintf fmt "%a" print_atype a
  | Pntype n -> fprintf fmt "%a" print_ntype n

and print_tdecl fmt t =
  fprintf fmt "%s :: forall @[<hov>%a@] (@[<hov>%a@]) => (@[<hov>%a@]) -> %a" t.dident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ident) t.identlist
  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ntype) t.ntypelist
  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_purtype) t.purtypelist print_purtype t.purtype

and print_instance fmt i = match i with
  | Intype n -> fprintf fmt "%a" print_ntype n
  | Iarrow (n1,n2) -> fprintf fmt "%a => %a" print_ntype n1 print_ntype n2
  | Imularrow (nlist,n) -> fprintf fmt "(@[<hov>%a@]) => %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") print_ntype) nlist print_ntype n

and print_defn fmt d =
  fprintf fmt "%s (@[<hov>%a@]) = %a" d.ident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_patarg) d.patargs print_expr d.expr

and print_ualist fmt (u,alist) =
  fprintf fmt "%s (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_atype) alist

and print_decl fmt d = match d with
  | Ddata (u,llist,ua) -> fprintf fmt "%s (@[<hov>%a@]) = (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ident) llist
  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ualist) ua
  | Dclass (u,llist,tlist) -> fprintf fmt "%s (@[<hov>%a@]) where {@[<hov>%a@]}" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ident) llist
  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_tdecl) tlist
  | Dinstance (i,dlist) -> fprintf fmt "%a where {[<hov>%a@]}" print_instance i Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_defn) dlist
  | Dtdecl t -> fprintf fmt "%a" print_tdecl t
  | Ddefn d -> fprintf fmt "%a" print_defn d

and print_file fmt f =
  fprintf fmt "@[<hov>%a@]" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")print_decl) f.decls



(*
let e = Ebinop(Bplus,Eident("oui",[Aconstant (Cint 1);Aconstant (Cstring "non")]),Eident("vrai",[Aconstant (Cbool false)]))

let () = printf "e = @[%a@]@." print_expr e


let ex =
  {imports = Import;decls = [Dclass("C",[],[{dident = "foo"; identlist = [];ntypelist = [];purtypelist = []; purtype = Patype(Auident("String"))}])]}

let() = printf "e = @[%a@]@." print_file ex
*)

(* pour le parseur *)
exception Parsing_error



