type file =
  {imports : imports; decls : decl list}

and imports = Import

and decl =
  | Ddefn of defn
  | Dtdecl of tdecl
  | Ddata of uident * (lident list) * ((uident * (atype list)) list)
  | Dclass of uident * (lident list) * tdecl list
  | Dinstance of instance * defn list

and defn =
  {lident : lident; patargs : patarg list; expr : expr}

and tdecl =
  | Tforall of lident * lident list
  | Tarrow of ntype list * purtype list * purtype

and ntype =
  {uident : uident; atypes : atype list}

and atype =
  | Alident of lident
  | Auident of uident
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
  | Plident of lident
  | Puident of uident
  | Ppattern of pattern

and pattern =
  | Ppatarg of patarg
  | Pmulpatarg of uident * patarg list

and constant =
  | Cbool of bool
  | Cint of int
  | Cstring of string

and atom =
  | Aconstant of constant
  | Alident of lident
  | Auident of uident
  | Aexpr of expr
  | Aexprtype of expr * purtype

and expr =
  | Eminus of expr
  | Ebinop of binop * expr * expr
  | Elident of lident * atom list   (*j'ai separe les 2 cas*)
  | Euident of uident * atom list
  | Eif of expr * expr * expr
  | Edo of expr list
  | Elet of binding list * expr
  | Ecase of expr * branch list

and binding =
  {lident : lident; expr : expr}

and branch =
  {pattern : pattern; expr : expr}

and binop = Bequals | Bnotequals | Binf | Binfeq | Bsup | Bsupeq | Bplus | Bminus | Btimes | Bdivide

and uident = string

and lident = string


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

let print_ident fmt s =
  fprintf fmt "%s" s


let rec print_atom fmt a = match a with
  | Aconstant c -> fprintf fmt "%a" print_constant c
  | Alident l -> fprintf fmt "%s" l
  | Auident u -> fprintf fmt "%s" u
  | Aexpr e -> fprintf fmt "%a" print_expr e
  | _ -> failwith "pas encore implemente"

and print_expr fmt e = match e with
  | Eminus e -> fprintf fmt "-%a" print_expr e
  | Ebinop (b,e1,e2) -> fprintf fmt "(%a %a %a)" print_expr e1 print_binop b print_expr e2
  | Elident (s,a) | Euident (s,a) -> fprintf fmt "%s [@[<hov>%a@]]" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_atom) a
  | Eif (e1,e2,e3) -> fprintf fmt "if %a then %a else %a" print_expr e1 print_expr e2 print_expr e3
  | Edo e -> fprintf fmt "do {@[<hov>%a@]}" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_expr) e
  | Elet (b,e) -> fprintf fmt "let {@[<hov>%a@]} in %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")  print_bindings) b print_expr e
  | _ -> failwith "Pas implemente"

and print_bindings fmt b =
  fprintf fmt "%s = %a" b.lident print_expr b.expr

and print_patarg fmt p =match p with
  | Pconstant c -> fprintf fmt "%a" print_constant c
  | Plident s | Puident s -> fprintf fmt "%s" s
  | Ppattern p -> fprintf fmt "%a" print_pattern p

and print_pattern fmt p = match p with
  | Ppatarg p -> fprintf fmt "%a" print_patarg p
  | Pmulpatarg (s,plist) -> fprintf fmt "%s(@[<hov>%a@])" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_patarg) plist

and print_atype fmt a = match a with
  | Alident s | Auident s -> fprintf fmt "%s" s
  | Apurtype p -> failwith "plus tard"

and print_ntype fmt n =
  fprintf fmt "%s(@[<hov>%a@])" n.uident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_atype) n.atypes

and print_purtype fmt p = match p with
  | Patype a -> fprintf fmt "%a" print_atype a
  | Pntype n -> fprintf fmt "%a" print_ntype n

and print_tdecl fmt t = match t with
  | Tforall (s,llist) -> fprintf fmt "%s :: forall @[<hov>%a@]" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ident) llist
  | Tarrow (nlist,plist,p) -> fprintf fmt "(@[<hov>%a@]) => (@[<hov>%a@]) -> %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_ntype) nlist
  Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_purtype) plist print_purtype p

and print_instance fmt i = match i with
  | Intype n -> fprintf fmt "%a" print_ntype n
  | Iarrow (n1,n2) -> fprintf fmt "%a => %a" print_ntype n1 print_ntype n2
  | Imularrow (nlist,n) -> fprintf fmt "(@[<hov>%a@]) => %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") print_ntype) nlist print_ntype n

and print_defn fmt d =
  fprintf fmt "%s (@[<hov>%a@]) = %a" d.lident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")  print_patarg) d.patargs print_expr d.expr

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




let e = Ebinop(Bplus,Elident("oui",[Aconstant (Cint 1);Aconstant (Cstring "non")]),Elident("vrai",[Aconstant (Cbool false)]))

let () = printf "e = @[%a@]@." print_expr e


let ex =
  {imports = Import;decls = [Dclass("C",["foo"],[Tarrow([],[],Patype(Auident("Int")))])]}

let() = printf "e = @[%a@]@." print_file ex






