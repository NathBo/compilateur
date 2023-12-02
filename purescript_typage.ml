open Purescript_ast
(*
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
  | Plident of ident
  | Puident of ident
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
  | Alident of ident
  | Auident of ident
  | Aexpr of expr
  | Aexprtype of expr * purtype

and expr =
  | Eatom of atom
  | Ebinop of binop * expr * expr
  | Elident of ident * atom list 
  | Euident of ident * atom list 
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
  | Tgeneral of string

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
  | Tgeneral s -> print_endline s
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
  | Tgeneral s -> Vset.empty


type schema = { vars : Vset.t; typ : typ }
type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = 
{
  bindings = Smap.empty;
  fvars = Vset.empty
}

let add s t envi = 
  if s = "_" then envi
  else {bindings = Smap.add s {vars = Vset.empty; typ = t} envi.bindings;fvars = Vset.union envi.fvars (fvars t)}


let grossunion s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let add_gen s t envi =
  if s = "_" then envi
  else
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
    | Tgeneral s -> Tgeneral s
  in
  subst tx.typ

let mem x env =
  Smap.mem x env.bindings

exception BadType of expr
exception NotDefined of string

let badtype e = raise (BadType e)
let notdefined s = raise (NotDefined s)


type constructor = {cident : string; ctlist : typ list; ctyp : typ; cenvvartyps : (typ*int) Smap.t}

let (envconstructors:(constructor Smap.t ref)) = ref Smap.empty


let rec smapaddlist env l = match l with
  | [],[] -> env
  | s::q1,t::q2 -> smapaddlist (Smap.add s t env) (q1,q2)
  | _ -> failwith "bah non"




let envfonctions = ref (smapaddlist Smap.empty (["not";"mod";"log";"pure"],
[
  (Tarrow([Boolean],Boolean),Smap.empty);
  (Tarrow([Int;Int],Int),Smap.empty);
  (Tarrow([String],Tcustom("Effect",[Unit])),Smap.empty);
  (Tarrow([Tgeneral "a"],Tcustom("Effect",[Tgeneral "a"])),Smap.add "a" ((Tgeneral "a"),0) Smap.empty)
]))


let rec substitute dejapris t = match head t with
  | Tgeneral s -> Smap.find s dejapris
  | Tcustom (s,tlist) -> Tcustom (s,List.map (substitute dejapris) tlist)
  | _ -> t

let lastdefined = ref ""


let (deflist:defn list ref) = ref []


let smaptolist sm =
  let rec aux s x l = (s,x)::l in
  Smap.fold aux sm []

  

let print_smap sm =
  let rec aux s sch =
    print_endline s;print_typ sch.typ in
  Smap.iter aux sm


let rec alldifferent l = match l with
  | [] -> true
  | x::q -> not (List.mem x q) && alldifferent q

let rec grosand l = match l with
  | [] -> true
  | b::q -> b&&(grosand q)


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
    | b::q -> let t' = typbranch env envtyps t b in List.iter (fun x-> if typbranch env envtyps t x <> t' then failwith "bad pattern type" else ()) q;
    if not (checkexaustive env envtyps t (List.map (fun b -> b.pattern) blist)) then failwith "Pattern non exhaustif" else t'
  )
  | Elident (f,alist) -> if mem f env then failwith (f^" n'est pas une fonction")
  else(match fst(Smap.find f !envfonctions) with
    | Tarrow(tlist,t) -> let dejapris = ref Smap.empty in
      let rec aux tlist alist = (match tlist,alist with
      | [],[] -> ()
      | t::q1,a::q2 -> (match t with
        | Tgeneral s -> let t' = typatom env envtyps a in
          (match t' with
            | Tgeneral _ -> aux q1 q2
            | _ -> if Smap.mem s !dejapris && t' <> Smap.find s !dejapris
              then failwith ("types incompatibles entre eux dans l'appel de "^f)
              else dejapris := Smap.add s t' !dejapris;aux q1 q2)
        | _ -> if typatom env envtyps a <> t then failwith ("mauvais argument de fonction pour "^f) else aux q1 q2)
      | _ -> failwith ("la fonction "^f^" ne prend pas autant d'arguments")) in
      aux tlist alist; substitute !dejapris t
    | _ -> failwith "pas possible")
  | Euident (s,alist) -> let constr = Smap.find s !envconstructors in
    let rec aux alist tlist = match alist,tlist with
      | [],[] -> ()
      | a::q1,t::q2 -> if typatom env envtyps a <> t then failwith ("types pas compatible dans l'utilisation du constructeur "^s) else aux q1 q2
      | _ -> failwith ("La liste d'atome n'est pas de la bonne longueur dans l'utilisation du constructeur "^s) in
    aux alist constr.ctlist;constr.ctyp



and typatom env envtyps a = match a with
  | Alident s -> find s env
  | Auident s -> (Smap.find s !envconstructors).ctyp
  | Aconstant c -> typconstant env envtyps c
  | Aexpr e -> typexpr env envtyps e
  | Aexprtype (e,p) -> let t = typexpr env envtyps e in if t <> typpurtyp env envtyps p then badtype e else t

and typconstant env envtyps c = match c with
  | Cbool _ -> Boolean
  | Cint _ -> Int
  | Cstring _ -> String

and typbranch env envtyps t b =
  let env = ensuretyppattern empty envtyps t b.pattern in
  typexpr env envtyps b.expr

and ensuretyppattern env envtyps t p = match p with
  | Ppatarg p -> ensuretyppatarg env envtyps t p
  | Pmulpatarg (s,plist) -> let constr = Smap.find s !envconstructors in
    if constr.ctyp <> t
    then failwith (s^"n'est pas un constructeur du bon type")
    else let rec aux envi plist tlist = match plist,tlist with
      | [],[] -> envi
      | p::q1,t::q2 -> let a = ensuretyppatarg env envtyps t p in
        let conflit s _ _ = failwith ("l'ident "^s^" est utilisé plusieurs fois") in
        let env = aux envi q1 q2 in
        {bindings = Smap.union conflit a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars}
      | _ -> failwith ("pas la bonne taille de pattern dans l'utilisation du constructeur "^constr.cident) in
    aux env plist constr.ctlist
      

and ensuretyppatarg env envtyps t p = match p with
  | Pconstant c -> if typconstant env envtyps c <> t then failwith "Mauvais patterne" else env
  | Plident s -> add s t env
  | Puident s -> if (Smap.find s !envconstructors).ctyp <> t then failwith (s^" n'est pas un constructeur du bon type") else env
  | Ppattern p -> ensuretyppattern env envtyps t p


and checkexaustive env envtyps t plist =
  let rec contientident l = match l with
    | [] -> false
    | Ppatarg (Plident _)::_ -> true
    | Ppatarg (Ppattern p)::q -> contientident [p] || contientident q
    | _::q -> contientident q in
  let rec contientbool b l = match l with
    | [] -> false
    | Ppatarg (Pconstant (Cbool x))::_ when x = b -> print_endline ("y a eu un "^(string_of_bool b));true
    | Ppatarg (Ppattern p)::q -> contientbool b [p] || contientbool b q
    | _::q -> contientbool b q in
  let rec contientconstr cs l = match l with
  | [] -> false
  | Pmulpatarg (s,_)::_ when s = cs -> true
  | Ppatarg (Puident s)::_ when s = cs -> true
  | Ppatarg (Ppattern p)::q -> contientconstr cs [p] || contientconstr cs q
  | _::q -> contientconstr cs q in
  if contientident plist then true
  else match t with
    | Boolean -> print_endline (string_of_bool(contientbool false plist));contientbool true plist && contientbool false plist
    | Tcustom (s,[]) -> grosand (List.map (fun (_,constr) -> (not (constr.ctyp = t) || contientconstr constr.cident plist)) (smaptolist !envconstructors) )
    | _ -> false





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
  | Aident s -> let t,arite = Smap.find s envtyps in if arite = 0 then t else failwith "devrait etre un type d'arite 0"

and typtdecl env envtyps td =
  if Smap.mem td.dident !envfonctions
    then failwith (td.dident^" est définie 2 fois")
else if not (alldifferent td.identlist)
    then failwith ("toutes les variables de types ne sont pas differentes dans l'appel de "^td.dident)
else
  let rec aux envtyps l = match l with
    | [] -> envtyps
    | s::q -> aux (Smap.add s (Tgeneral s,0) envtyps) q in
  let envvartyps = aux Smap.empty td.identlist in
  let conflit s _ _ = failwith "conflit dans les patternes" in
  let envtyps = Smap.union conflit envtyps envvartyps in
  print_typ (typpurtyp env envtyps td.purtype);
  lastdefined := td.dident;
  Tarrow(List.map (typpurtyp env envtyps) td.purtypelist,typpurtyp env envtyps td.purtype),envvartyps

and typdfn env envtyps (df:defn) tlist t =
  if Smap.mem df.ident !envfonctions && !lastdefined <> df.ident
  then failwith (df.ident^" est définie 2 fois")
else
  (deflist := df:: !deflist;
  let patarglist = df.patargs in
  let conflit s _ _ = failwith ("l'ident "^s^" est utilisé plusieurs fois") in
  let rec aux envi ptlist tlist = match ptlist,tlist with
    | [],[] -> envi
    | pt::q1,t::q2 -> let a = ensuretyppatarg empty envtyps t pt in
    let env = aux envi q1 q2 in
    print_smap a.bindings;
    {bindings = Smap.union conflit a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars}
    | _ -> failwith "Pas le bon nombre d'arguments" in
  let env = aux empty patarglist tlist in
  let conflit s _ _ = (failwith ("conflit dans les forall avec "^s)) in
  let envtyps = Smap.union conflit envtyps (snd (Smap.find df.ident !envfonctions)) in
  print_string (df.ident^"\n");List.iter print_typ tlist;print_typ t;print_int (List.length df.patargs);
  if typexpr env envtyps df.expr<>t
  then (print_typ (typexpr env envtyps df.expr);failwith ("pas le type censé etre renvoyé par "^df.ident))
  else ())

and typdata env envtyps s slist ialist =
  if Smap.mem s !envtyps then failwith ("conflit dans la définition du type "^s)
  else if not (alldifferent slist)
  then failwith ("toutes les variables de types ne sont pas differentes dans la définition du type "^s)
else
  let rec aux envtyps l = match l with
    | [] -> envtyps
    | s::q -> aux (Smap.add s (Tgeneral s,0) envtyps) q in
  let conflit s _ _ = (failwith ("conflit dans les forall avec "^s)) in
  let envvartyps = aux Smap.empty slist in
  let t = Tcustom (s,[]) in
  let envtypsact = Smap.union conflit envvartyps !envtyps in
  let rec aux2 l = match l with
    | [] -> ()
    | (i,alist)::q -> if Smap.mem i !envconstructors
      then failwith ("Le constructeur "^i^" est défini plusieurs fois")
      else envconstructors := Smap.add i {cident = i; ctlist = List.map (typatype env envtypsact) alist; ctyp = t; cenvvartyps = envvartyps} !envconstructors;aux2 q in
  aux2 ialist;
  envtyps := Smap.add s (t,0) !envtyps

and checkforenddef envtyps =
  let rec isanident p = match p with
    | Plident _ -> true
    | Ppattern (Ppatarg p ) -> isanident p
    | _ -> false in
  let rec getchanging dejatrouve i l = match l with
    | [] -> -1
    | x::q when isanident x -> getchanging dejatrouve (i+1) q
    | x::q when dejatrouve -> failwith "Dans les fonctions, on filtre au plus une valeur"
    | x::q -> let _ = getchanging true (i+1) q in i in
  let rec aux posdufiltrage (df:defn) =
    Ppatarg (List.nth df.patargs posdufiltrage) in
  if !lastdefined = ""
  then ()
  else begin let conflit s _ _ = (failwith ("conflit dans les forall avec "^s)) in
  let envtyps = Smap.union conflit envtyps (snd (Smap.find !lastdefined !envfonctions)) in
    match !deflist with
      | [] -> failwith ("La déclaration de "^(!lastdefined)^" doit être suivie de sa définition");
      | x::q -> let posdufiltrage = getchanging false 0 x.patargs in print_string "On filtre en ";print_int posdufiltrage;
        if posdufiltrage = -1
        then (if List.length !deflist >1 then failwith (!lastdefined^" est définie 2 fois") else ())
        else(match fst(Smap.find !lastdefined !envfonctions) with
      | Tarrow(tlist,t) -> if not (checkexaustive empty envtyps (List.nth tlist posdufiltrage) (List.map (aux posdufiltrage) !deflist)) then failwith ("Patterne non exhaustif dans la definition de "^(!lastdefined));
      lastdefined := ""; deflist := []
      | _ -> failwith "pas possible")
  end
  
and typdecl env envtyps d = match d with
  | Dtdecl td -> checkforenddef !envtyps;let f = typtdecl env !envtyps td in envfonctions := Smap.add td.dident f !envfonctions
  | Ddefn df -> (match fst(Smap.find df.ident !envfonctions) with
    | Tarrow(tlist,t) -> typdfn env !envtyps df tlist t
    | _ -> failwith "pas possible")
  | Ddata (s,slist,ialist) -> checkforenddef !envtyps;typdata env envtyps s slist ialist
  | _ -> failwith "pas implémenté"



and typfile f =
  let env = empty in
  let envtyps = ref (smapaddlist Smap.empty (["Int";"String";"Unit";"Boolean";"Effect"],[(Int,0);(String,0);(Unit,0);(Boolean,0);(Tcustom("Effect",[Unit]),1)])) in
  List.iter (typdecl env envtyps) f.decls;
  checkforenddef !envtyps;
  if not (Smap.mem "main" !envfonctions) then failwith "Pas de fonction main définie" else ()







