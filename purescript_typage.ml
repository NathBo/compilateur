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
    {dlident : lident; lidentlist : lident list; ntypelist : ntype list; purtypelist : purtype list; purtype : purtype}

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


type typ =
  | Unit
  | Int
  | String
  | Boolean
  | Effect of typ
  | Tarrow of typ*typ
  | Tvar of tvar

and tvar = {id : int; def : typ option}


type typfunctions =
  {flidents : lident list; instancelist : typinstance list; typlist : typ list;typ : typ; matching : (motif list * expr)list}

and typinstance =
  {typinstancelist : typinstance list; typlist : typ list; matching : (lident * motif list * expr) list}

and motif =
  | Mconst of constant
  | Mident of string

and typclass =
  {variablesdetypes : string list; fonctionsdeclasse : (string * typ list)list}



module Smap = Map.Make(String)

type envtyps = typ Smap.t

let globalenvtyps = Smap.empty




(*
let globalenvfunctions = Smap.add "not" {flidents = []; instancelist = []; typlist = [Boolean];typ = Boolean;matching = []} Smap.empty
let globalenvfunctions = Smap.add "mod" {flidents = []; instancelist = []; typlist = [Int;Int];typ = Int;matching = []} globalenvfunctions
let globalenvfunctions = Smap.add "log" {flidents = []; instancelist = []; typlist = [String];typ = Effect(Unit);matching = []} globalenvfunctions
let globalenvfunctions = Smap.add "pure" {flidents = ["a"]; instancelist = []; typlist = [Tenv "a"];typ = Effect(Tenv "a");matching = []} globalenvfunctions

let globalenvclasses = Smap.add "Show" {variablesdetypes = ["a"]; fonctionsdeclasse = [("show",[Tenv "a";String])]} Smap.empty
let globalenvfunctions = Smap.add "show" {flidents = ["a"]; instancelist = []; typlist = [Tenv "a"];typ = String;matching = []} globalenvfunctions

let globalenvinstances = Smap.add "Show" [([Boolean],{typinstancelist = []; typlist = [Boolean]; matching = []});([Int],{typinstancelist = []; typlist = [Int]; matching = []})]
*)

module V = struct
  type t = tvar
  let compare v1 v2 = Stdlib.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end



let rec head t = match t with
  | Tvar {id = _; def = Some t2} -> head t2
  | _ -> t


let rec canon t = match t with
| Tvar {id = _; def = Some t2} -> head t2
| Tarrow (t1,t2) -> Tarrow (canon t1, canon t2)
| _ -> t

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))


let rec occur tv t = match head t with
  | Tvar tv2 -> V.equal tv tv2
  | Tarrow (t1,t2) -> occur tv t1 || occur tv t2
  | _ -> false


let defdestyps = Hashtbl.create 256


let rec unify t1 t2 = match head t1, head t2 with
  | Unit,Unit | Int,Int | String,String | Boolean,Boolean -> head t1
  | Effect t1,Effect t2 -> Effect (unify t1 t2)
  | Tarrow (a1,a2),Tarrow(b1,b2) -> Tarrow(unify a1 b1, unify a2 b2)
  | Tvar tv,tau -> if occur tv tau
    then unification_error t1 t2
    else Hashtbl.add defdestyps tv.id tau;tau
  | _,Tvar _ -> unify t2 t1
  | _ -> unification_error t1 t2


module Vset = Set.Make(V)


let rec fvars t = ()





let ex =
  {imports = Import;decls = [Dclass("C",[],[{dlident = "foo"; lidentlist = [];ntypelist = [];purtypelist = []; purtype = Patype(Auident("String"))}])]}


