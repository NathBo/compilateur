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
  | Eatom of atom
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

and binop = Bequals | Bnotequals | Binf | Binfeq | Bsup | Bsupeq | Bplus | Bminus | Btimes | Bdivide | Bcons | Band | Bor

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

and tvar = {id : int; mutable def : typ option}


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




let rec unify t1 t2 = match head t1, head t2 with
  | Unit,Unit | Int,Int | String,String | Boolean,Boolean -> ()
  | Effect t1,Effect t2 -> unify t1 t2
  | Tarrow (a1,a2),Tarrow(b1,b2) -> unify a1 b1;unify a2 b2
  | Tvar tv1,Tvar tv2 when V.equal tv1 tv2 -> ()
  | Tvar tv,tau -> if occur tv tau
    then unification_error t1 t2
    else tv.def <- Some t2
  | _,Tvar _ -> unify t2 t1
  | _ -> unification_error t1 t2


module Vset = Set.Make(V)


let rec fvars t = match head t with
  | Unit | Int | String | Boolean -> Vset.empty
  | Effect t -> fvars t
  | Tarrow (t1,t2) -> Vset.union (fvars t1) (fvars t2)
  | Tvar tv -> Vset.singleton tv


type schema = { vars : Vset.t; typ : typ }
module Smap = Map.Make(String)
type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = 
{
  bindings = Smap.empty;
  fvars = Vset.empty
}

let add s t envi = 
  {bindings = Smap.add s {vars = Vset.empty; typ = t} envi.bindings;fvars = Vset.union envi.fvars (fvars t)}


let grossunion s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let add_gen s t envi =
  let vt = fvars t in
  let bindings = { vars = Vset.diff vt (grossunion envi.fvars); typ = t } in
  {bindings = Smap.add s bindings envi.bindings; fvars = envi.fvars}

module Vmap = Map.Make(V)

let find x env =
  let tx = Smap.find x env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | Tvar x as t -> (try Vmap.find x s with Not_found -> t)
    | Int | String | Unit | Boolean -> t
    | Effect t -> Effect (subst t)
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
  in
  subst tx.typ

exception BadType of expr
exception NotDefined of string

let badtype e = raise (BadType e)
let notdefined s = raise (NotDefined s)


let rec typexpr env expr = match expr with
  | Ebinop (b,e1,e2) -> (match b with
    | Bequals | Bnotequals -> let t = typexpr env e1 in if List.mem t [Int;String;Boolean] then (if typexpr env e2 <> t then badtype e2 else Boolean) else badtype e1
    | Binf | Binfeq | Bsup | Bsupeq -> if typexpr env e1 <> Int then badtype e1 else if typexpr env e2 <> Int then badtype e2 else Boolean
    | Bplus | Bminus | Btimes | Bdivide -> if typexpr env e1 <> Int then badtype e1 else if typexpr env e2 <> Int then badtype e2 else Int
  )
  | Eatom a -> (match a with
    | Alident s | Auident s -> find s env
    | Aconstant c -> (match c with
      | Cbool _ -> Boolean
      | Cint _ -> Int
      | Cstring _ -> String
    )
    | Aexpr e -> typexpr env e
    | _ -> failwith "Pas implémenté"
  )
  | Eif (e1,e2,e3) -> if typexpr env e1 <> Boolean then badtype e1 else let t = typexpr env e2 in if typexpr env e3 <> t then badtype e3 else t
  | Edo elist -> (List.iter (fun e -> if typexpr env e <> Effect Unit then badtype e else ()) elist;Effect Unit)
  | Elet (blist,e) -> failwith "j'ai pas compris"
  | _ -> failwith "Pas implémenté"




let ex =
  {imports = Import;decls = [Dclass("C",[],[{dlident = "foo"; lidentlist = [];ntypelist = [];purtypelist = []; purtype = Patype(Auident("String"))}])]}

let exsimple = Ebinop(Bplus,Eatom(Aconstant (Cint 1)),Eatom(Aconstant (Cint 2)))
