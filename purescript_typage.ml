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


let ex =
  {imports = Import;decls = [Dclass("C",["foo"],[Tarrow([],[],Patype(Auident("Int")))])]}

