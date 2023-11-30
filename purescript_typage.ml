open Purescript_ast

(*type file =
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
*)

type typ =
  | Unit
  | Int
  | String
  | Boolean
  | Tcustom of string * typ list
  | Tarrow of typ list*typ
  | Tvar of tvar

and tvar = {id : int; mutable def : typ option}





type typfunctions =
  {flidents : ident list; instancelist : typinstance list; typlist : typ list;typ : typ; matching : (motif list * expr)list}

and typinstance =
  {typinstancelist : typinstance list; typlist : typ list; matching : (ident * motif list * expr) list}

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
| Tarrow (tlist,t2) -> Tarrow (List.map canon tlist, canon t2)
| _ -> t

let rec print_typ t = match canon t with
  | Int -> print_string "Int\n"
  | String -> print_string "String\n"
  | Unit -> print_string "Unit\n"
  | Boolean -> print_string "Boolean\n"
  | Tarrow (tlist,t) -> print_string "(";List.iter print_typ tlist; print_string ") -> ";print_typ t;print_string "\n"
  | Tcustom (s,tlist) -> print_string s;print_string " of (";List.iter print_typ tlist;print_string ")\n"
  | Tvar {id = id; def = None} -> print_int id;print_string " polymorphe\n"
  | _ -> failwith "Non"

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))


let rec grosor l = match l with
  | [] -> false
  | x::q -> x || grosor q


let rec occur tv t = match head t with
  | Tvar tv2 -> V.equal tv tv2
  | Tarrow (tlist,t2) -> grosor (List.map (occur tv) tlist) || occur tv t2
  | _ -> false




(*let rec unify t1 t2 = match head t1, head t2 with
  | Unit,Unit | Int,Int | String,String | Boolean,Boolean -> ()
  | Effect t1,Effect t2 -> unify t1 t2
  | Tarrow (a1,a2),Tarrow(b1,b2) -> failwith "voyons si unify est utile avant de se casser la tete"
  | Tvar tv1,Tvar tv2 when V.equal tv1 tv2 -> ()
  | Tvar tv,tau -> if occur tv tau
    then unification_error t1 t2
    else tv.def <- Some t2
  | _,Tvar _ -> unify t2 t1
  | _ -> unification_error t1 t2*)


module Vset = Set.Make(V)


let rec fvars t = match head t with
  | Unit | Int | String | Boolean -> Vset.empty
  | Tcustom (_,tlist) -> List.fold_left Vset.union Vset.empty (List.map fvars tlist)
  | Tarrow (tlist,t2) -> Vset.union (List.fold_left Vset.union Vset.empty (List.map fvars tlist)) (fvars t2)
  | Tvar tv -> Vset.singleton tv


type schema = { vars : Vset.t; typ : typ }
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
    | Tcustom (s,tlist) -> Tcustom(s,List.map subst tlist)
    | Tarrow (tlist, t2) -> Tarrow (List.map subst tlist, subst t2)
  in
  subst tx.typ

exception BadType of expr
exception NotDefined of string

let badtype e = raise (BadType e)
let notdefined s = raise (NotDefined s)


let rec smapaddlist env l = match l with
  | [],[] -> env
  | s::q1,t::q2 -> smapaddlist (Smap.add s t env) (q1,q2)
  | _ -> failwith "bah non"




let envfonctions = ref (smapaddlist Smap.empty (["not";"mod";"log"],[Tarrow([Boolean],Boolean);Tarrow([Int;Int],Int);Tarrow([String],Tcustom("Effect",[Unit]))]))


let rec typexpr env envtyps expr = match expr with
  | Ebinop (b,e1,e2) -> (match b with
    | Bequals | Bnotequals -> let t = typexpr env envtyps e1 in if List.mem t [Int;String;Boolean] then (if typexpr env envtyps e2 <> t then badtype e2 else Boolean) else badtype e1
    | Binf | Binfeq | Bsup | Bsupeq -> if typexpr env envtyps e1 <> Int then badtype e1 else if typexpr env envtyps e2 <> Int then badtype e2 else Boolean
    | Bplus | Bminus | Btimes | Bdivide -> if typexpr env envtyps e1 <> Int then badtype e1 else if typexpr env envtyps e2 <> Int then badtype e2 else Int
    | Bor | Band -> if typexpr env envtyps e1 <> Boolean then badtype e1 else if typexpr env envtyps e2 <> Boolean then badtype e2 else Boolean
    | Bcons -> if typexpr env envtyps e1 <> String then badtype e1 else if typexpr env envtyps e2 <> String then badtype e2 else String
  )
  | Eatom a -> typatom env envtyps a
  | Eif (e1,e2,e3) -> if typexpr env envtyps e1 <> Boolean then badtype e1 else let t = typexpr env envtyps e2 in if typexpr env envtyps e3 <> t then badtype e3 else t
  | Edo elist -> (List.iter (fun e -> if typexpr env envtyps e <> Tcustom ("Effect",[Unit]) then badtype e else ()) elist;Tcustom ("Effect",[Unit]))
  | Elet (blist,e) ->
    let rec aux env envtyps l = match l with
      | b::q -> let t = typexpr env envtyps b.bindexpr in aux (add_gen b.ident t env) envtyps q
      | [] -> env in
    typexpr (aux env envtyps blist) envtyps e
  | Ecase (e,blist) -> let t = typexpr env envtyps e in (match blist with
    | [] -> failwith "Pattern vide"
    | b::q -> let t' = typbranch env envtyps t b in List.iter (fun x-> if typbranch env envtyps t x <> t' then failwith "bad pattern type" else ()) q; t'
  )
  | Eident (f,alist) -> match Smap.find f !envfonctions with
    | Tarrow(tlist,t) -> let rec aux tlist alist = (match tlist,alist with
      | [],[] -> ()
      | t::q1,a::q2 -> if typatom env envtyps a <> t then failwith "mauvais argument de fonction" else aux q1 q2
      | _ -> failwith "pas possible") in aux tlist alist;t
    | _ -> failwith "pas possible"



and typatom env envtyps a = match a with
  | Aident s -> find s env
  | Aconstant c -> typconstant env envtyps c
  | Aexpr e -> typexpr env envtyps e
  | Aexprtype (e,p) -> let t = typexpr env envtyps e in if t <> typpurtyp env envtyps p then badtype e else t

and typconstant env envtyps c = match c with
  | Cbool _ -> Boolean
  | Cint _ -> Int
  | Cstring _ -> String

and typbranch env envtyps t b =
  let env = ensuretyppattern env envtyps t b.pattern in
  typexpr env envtyps b.expr

and ensuretyppattern env envtyps t p = match p with
  | Ppatarg p -> ensuretyppatarg env envtyps t p
  | Pmulpatarg _ -> failwith "compliqué ça"

and ensuretyppatarg env envtyps t p = match p with
  | Pconstant c -> if typconstant env envtyps c <> t then failwith "Mauvais patterne" else env
  | Pident s -> add s t env
  | Ppattern p -> ensuretyppattern env envtyps t p

and typpurtyp env envtyps pt = match pt with
  | Patype a -> typatype env envtyps a
  | Pntype n -> typntype env envtyps n

and typntype env envtyps n =
  let t,arite = Smap.find n.nident envtyps in
  if List.length n.atypes <> arite
  then failwith "pas la bonne arité"
  else match t with
    | Tcustom (s,_) -> Tcustom(s,List.map (typatype env envtyps) n.atypes)
    | _ -> t

and typatype env envtyps a = match a with
  | Apurtype p -> typpurtyp env envtyps p
  | Aident s -> let t,arite = Smap.find s envtyps in if arite = 0 then t else failwith "devrait etre un tyoe d'arite 0"

and typtdecl env envtyps td =
  print_typ (typpurtyp env envtyps td.purtype);
  Tarrow(List.map (typpurtyp env envtyps) td.purtypelist,typpurtyp env envtyps td.purtype)

and typdfn env envtyps df tlist t =
  let conflit s _ _ = failwith "conflit dans les patternes" in
  let rec aux envi ptlist tlist = match ptlist,tlist with
    | [],[] -> envi
    | pt::q1,t::q2 -> let a = ensuretyppatarg env envtyps t pt in
    let env = aux envi q1 q2 in
    {bindings = Smap.union conflit a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars}
    | _ -> failwith "Pas le bon nombre d'argument" in
  let env = aux empty df.patargs tlist in
  print_string (df.ident^"\n");List.iter print_typ tlist;print_typ t;print_int (List.length df.patargs);
  if typexpr env envtyps df.expr<>t
  then failwith "pas le type censé etre renvoyé"
  else ()
  
and typdecl env envtyps d = match d with
  | Dtdecl td -> let t = typtdecl env !envtyps td in envfonctions := Smap.add td.dident t !envfonctions
  | Ddefn df -> (match Smap.find df.ident !envfonctions with
    | Tarrow(tlist,t) -> typdfn env !envtyps df tlist t
    | _ -> failwith "pas possible")
  | _ -> failwith "pas implémenté"



and typfile f =
  let env = empty in
  let envtyps = ref (smapaddlist Smap.empty (["Int";"String";"Unit";"Boolean";"Effect"],[(Int,0);(String,0);(Unit,0);(Boolean,0);(Tcustom("Effect",[Unit]),1)])) in
  List.iter (typdecl env envtyps) f.decls




let ex =
  {imports = Import;decls = [Dclass("C",[],[{dident = "foo"; identlist = [];ntypelist = [];purtypelist = []; purtype = Patype(Aident("String"))}])]}

let exsimple = Ebinop(Bplus,Eatom(Aconstant (Cbool true)),Eatom(Aconstant (Cint 2)))

let ex2 = Eif(Eatom(Aconstant (Cbool true)),Eatom(Aconstant (Cstring "yes")),Eatom(Aconstant (Cint 1)))

let exfile = {imports = Import; decls = 
  [
    Dtdecl{dident = "main";identlist = [];ntypelist = [];purtypelist = [];purtype = Patype(Aident "Unit")};
    Ddefn{ident = "main";patargs = [];expr = Eident("log",[Aconstant(Cstring "")])}
  ]}




