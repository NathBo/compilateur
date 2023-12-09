open Purescript_ast
(*
type tfile =
  {imports : imports; decls : decl list}

and timports = Import

and tdecl =
  | TDdefn of tdefn
  | TDtdecl of ttdecl
  | TDdata of ident * (ident list) * ((ident * (atype list)) list)
  | TDclass of ident * (ident list) * ttdecl list
  | TDinstance of tinstance * tdefn list

and tdefn =
  {ident : ident; tpatargs : tpatarg list; texpr : texpr}

and tdecl =
  {tdident : ident; tidentlist : ident list; tntypelist : tntype list; tpurtypelist : tpurtype list; tpurtype : tpurtype}

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


exception TypingError of string * position

let typingerror s pos = raise (TypingError (s,pos))



let smapfind s smap pos =
  if Smap.mem s smap
  then Smap.find s smap
  else typingerror ("L'identificateur "^s^" n'est pas défini") pos


let string_to_list s = List.init (String.length s) (String.get s)


let isalident s = List.mem s.[0] (string_to_list "azertyuiopqsdfghjklmwxcvbn")


let (globalenvinstances : (typ list*(ident*(typ list))list ) list Smap.t ref) = ref (Smap.add "Show" ([([Int],[]);([Boolean],[])]) Smap.empty)


let print_bool b =
  if b then print_endline "true"
  else print_endline "false"

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

exception UnificationFailure of typ * typ * position



let rec grosor l = match l with
  | [] -> false
  | x::q -> x || grosor q


let rec occur tv t = match head t with
  | Tvar tv2 -> V.equal tv tv2
  | Tarrow (tlist,t2) -> grosor (List.map (occur tv) tlist) || occur tv t2
  | _ -> false




let unifyable tlist1 tlist2 =
  let (assoc:typ Smap.t ref) = ref Smap.empty in
  let rec aux tlist1 tlist2 = match tlist1,tlist2 with
    | [],[] -> true
    | Unit::q1,Unit::q2 | Int::q1,Int::q2 | String::q1,String::q2 | Boolean::q1,Boolean::q2 -> aux q1 q2
    | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> aux l1 l2 && aux q1 q2
    | Tgeneral s::q1,t::q2 -> if Smap.mem s !assoc
      then aux ((Smap.find s !assoc)::q1) (t::q2)
      else (assoc := Smap.add s t !assoc;aux q1 q2)
    | t::q1,Tgeneral s::q2 -> aux tlist2 tlist1
    | _ -> false in
  aux tlist1 tlist2
  

let unification tlist1 tlist2 pos =
  let (assoc:typ Smap.t ref) = ref Smap.empty in
  let rec aux tlist1 tlist2  = match tlist1,tlist2 with
    | [],[] -> ()
    | t1::q1,t2::q2 when t1=t2 -> aux q1 q2
    | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> aux l1 l2;aux q1 q2
    | Tgeneral s::q1,t::q2 -> if Smap.mem s !assoc
      then aux ((Smap.find s !assoc)::q1) (t::q2)
      else (assoc := Smap.add s t !assoc;aux q1 q2)
      | t::q1,Tgeneral s::q2 -> aux tlist2 tlist1
      | _ -> typingerror "Pas d'unification possible" pos in
  aux tlist1 tlist2;
  !assoc


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


let envunion f env1 env2 =
  {bindings = Smap.union f env1.bindings env2.bindings;fvars = Vset.union env1.fvars env2.fvars}




let grossunion s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let add_gen s t envi =
  if s = "_" then envi
  else
    let vt = fvars t in
    let bindings = { vars = Vset.diff vt (grossunion envi.fvars); typ = t } in
    {bindings = Smap.add s bindings envi.bindings; fvars = envi.fvars}

module Vmap = Map.Make(V)

let find x env pos =
  let tx = smapfind x env.bindings pos in
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




type constructor = {cident : string; ctlist : typ list; ctyp : typ; cenvvartyps : (typ*int) Smap.t}

let (envconstructors:(constructor Smap.t ref)) = ref Smap.empty


let rec smapaddlist env l = match l with
  | [],[] -> env
  | s::q1,t::q2 -> smapaddlist (Smap.add s t env) (q1,q2)
  | _ -> failwith "bah non"







let rec substitute dejapris pos t = match head t with
  | Tgeneral s -> if Smap.mem s dejapris then smapfind s dejapris pos else Tgeneral s
  | Tcustom (s,tlist) -> Tcustom (s,List.map (substitute dejapris pos) tlist)
  | _ -> t

let lastdefined = ref ""


let (deflist:defn list ref) = ref []


let smaptolist sm =
  let rec aux s x l = (s,x)::l in
  Smap.fold aux sm []

  
let envfonctions = ref (smapaddlist Smap.empty (["not";"mod";"log";"pure";"show"],
[
(Tarrow([Boolean],Boolean),Smap.empty,None,Smap.empty);
(Tarrow([Int;Int],Int),Smap.empty,None,Smap.empty);
  (Tarrow([String],Tcustom("Effect",[Unit])),Smap.empty,None,Smap.empty);
  (Tarrow([Tgeneral "a"],Tcustom("Effect",[Tgeneral "a"])),Smap.add "a" ((Tgeneral "a"),0) Smap.empty,None,Smap.empty);
  (Tarrow([Tgeneral "a"],String),Smap.add "a" ((Tgeneral "a"),0) Smap.empty,Some "Show",Smap.empty)
]))



let (envclasses : (ident list*(typ Smap.t)*typ list)Smap.t ref) = ref (Smap.add "Show" (["a"],(Smap.add "show" (Tarrow([Tgeneral "a"],String)) Smap.empty),[]) Smap.empty)


let print_smap sm =
  let rec aux s sch =
    print_endline s;print_typ sch.typ in
  Smap.iter aux sm


let fstr (a,_,_,_) = a


let sndr (_,b,_,_) = b

let thrd (_,_,c,_) = c

let fourth (_,_,_,d) = d


let rec alldifferent l = match l with
  | [] -> true
  | x::q -> not (List.mem x q) && alldifferent q

let rec grosand l = match l with
  | [] -> true
  | b::q -> b&&(grosand q)




let rec typfile f =
  let env = add "unit" Unit empty in
  let envtyps = ref (smapaddlist Smap.empty (["Int";"String";"Unit";"Boolean";"Effect"],[(Int,0);(String,0);(Unit,0);(Boolean,0);(Tcustom("Effect",[Unit]),1)])) in
  List.iter (typdecl env envtyps !globalenvinstances) f.decls;
  checkforenddef !envtyps !globalenvinstances f.pos;
  if not (Smap.mem "main" !envfonctions) then typingerror "Pas de fonction main définie" f.pos else ()


and typdecl env envtyps envinstances d = match d with
  | Dtdecl (td,pos) -> checkforenddef !envtyps !globalenvinstances pos;let a,b,d = typtdecl env !envtyps Smap.empty td in envfonctions := Smap.add td.dident (a,b,None,d) !envfonctions
  | Ddefn (df,pos) -> (match fstr(smapfind df.ident !envfonctions pos) with
    | Tarrow(tlist,t) -> typdfn env !envtyps !globalenvinstances true df tlist t
    | _ -> failwith "pas possible")
  | Ddata (s,slist,ialist,pos) -> checkforenddef !envtyps !globalenvinstances pos;typdata env envtyps envinstances pos s slist ialist
  | Dclass (s,slist,tdlist,pos) -> checkforenddef !envtyps !globalenvinstances pos;typclass env !envtyps !globalenvinstances pos s slist tdlist
  | Dinstance (i,deflist,pos) -> checkforenddef !envtyps !globalenvinstances pos;typinstance env !envtyps !globalenvinstances i deflist



and typexpr env envtyps (envinstances:(typ list * (ident * typ list) list) list Smap.t) (general:bool) expr = match expr with
  | Ebinop (b,e1,e2,pos) -> (match b with
    | Bequals pos | Bnotequals pos -> let t = typexpr env envtyps envinstances general e1 in if List.mem t [Int;String;Boolean;Unit] then (if typexpr env envtyps envinstances general e2 <> t then typingerror "Mauvais type" pos else Boolean) else typingerror "Mauvais type" pos
    | Binf pos | Binfeq pos | Bsup pos | Bsupeq pos -> if typexpr env envtyps envinstances general e1 <> Int then typingerror "Mauvais type" pos else if typexpr env envtyps envinstances general e2 <> Int then typingerror "Mauvais type" pos else Boolean
    | Bplus pos | Bminus pos | Btimes pos | Bdivide pos -> if typexpr env envtyps envinstances general e1 <> Int then typingerror "Mauvais type" pos else if typexpr env envtyps envinstances general e2 <> Int then typingerror "Mauvais type" pos else Int
    | Bor pos | Band pos -> if typexpr env envtyps envinstances general e1 <> Boolean then typingerror "Mauvais type" pos else if typexpr env envtyps envinstances general e2 <> Boolean then typingerror "Mauvais type" pos else Boolean
    | Bcons pos -> if typexpr env envtyps envinstances general e1 <> String then typingerror "Mauvais type" pos else if typexpr env envtyps envinstances general e2 <> String then typingerror "Mauvais type" pos else String
  )
  | Eatom (a,pos) -> typatom env envtyps envinstances general a
  | Eif (e1,e2,e3,pos) -> if typexpr env envtyps envinstances general e1 <> Boolean then typingerror "Mauvais type" pos else let t = typexpr env envtyps envinstances general e2 in if typexpr env envtyps envinstances general e3 <> t then typingerror "Mauvais type" pos else t
  | Edo (elist,pos) -> (List.iter (fun e -> if typexpr env envtyps envinstances general e <> Tcustom ("Effect",[Unit]) then typingerror "Mauvais type" pos else ()) elist;Tcustom ("Effect",[Unit]))
  | Elet (blist,e,pos) ->
    let rec aux env envtyps envinstances l = match l with
      | b::q -> let t = typexpr env envtyps envinstances general b.bindexpr in aux (add_gen b.ident t env) envtyps envinstances q
      | [] -> env in
    typexpr (aux env envtyps envinstances blist) envtyps envinstances  general e
  | Ecase (e,blist,pos) -> let t = typexpr env envtyps envinstances general e in (match blist with
    | [] -> typingerror "Pattern vide" pos
    | b::q -> let t' = typbranch env envtyps envinstances t general b in List.iter (fun x-> if typbranch env envtyps envinstances t general x <> t' then typingerror "bad pattern type" pos else ()) q;
    if not (checkexaustivelist env envtyps envinstances [t] (List.map (fun x -> [x]) (List.map (fun b -> b.pattern) blist))) then typingerror "Pattern non exhaustif" pos else t'
  )
  | Elident (f,alist,pos) -> let envinstances = Smap.union noconseqconflit envinstances (fourth (smapfind f !envfonctions pos))  in if mem f env then typingerror (f^" n'est pas une fonction") pos
  else ((match thrd (smapfind f !envfonctions pos) with
  | None -> ()
  | Some cident -> let instances = smapfind cident envinstances pos in find_compatible_instance pos envinstances cident f general instances (List.map (typatom env envtyps envinstances general) alist));
    let tlist,t = (match fstr(smapfind f !envfonctions pos) with
    | Tarrow(tlist,t) -> tlist,t
    | _ -> failwith "pas possible") in
    let dejapris = ref Smap.empty in
      let rec aux tlist alist = (match tlist,alist with
      | [],[] -> ()
      | t::q1,a::q2 -> (match t with
        | Tgeneral s -> let t' = typatom env envtyps envinstances general a in
          (match t' with
            | Tgeneral _ -> aux q1 q2
            | _ -> if Smap.mem s !dejapris && t' <> smapfind s !dejapris pos
              then typingerror ("types incompatibles entre eux dans l'appel de "^f) pos
              else dejapris := Smap.add s t' !dejapris;aux q1 q2)
        | _ -> if not( unifyable [typatom env envtyps envinstances general a] [t]) then typingerror ("mauvais argument de fonction pour "^f) pos else aux q1 q2)
      | _ -> typingerror ("la fonction "^f^" ne prend pas autant d'arguments") pos) in
      aux tlist alist; substitute !dejapris pos t)
  | Euident (s,alist,pos) -> let constr = smapfind s !envconstructors pos in
    let matching = unification (List.map (typatom env envtyps envinstances general) alist) constr.ctlist pos in
    substitute matching pos constr.ctyp



and find_compatible_instance pos envinstances cident f (general:bool) instances tlist =
  let slist,classenvfonctions,flist = smapfind cident !envclasses pos in
  let ftlist = match smapfind f classenvfonctions pos with
    | Tarrow(tlist,t) -> tlist
    | _ -> failwith "pas possible" in
  let rec constrsmap sm tlist1 tlist2 = match tlist1,tlist2 with
    | [],[] -> sm
    | Tgeneral s::q1, t::q2 -> Smap.add s t (constrsmap sm q1 q2)
    | t1::q1,t2::q2 when t1 = t2 -> constrsmap sm q1 q2
    | _ -> typingerror ("L'appel de "^f^" n'est pas compatible avec sa définition dans la classe "^cident) pos in
  let sm = constrsmap Smap.empty ftlist tlist in
  let listdestvar = List.map (fun s -> smapfind s sm pos) slist in
  let rec compatible tlist1 tlist2 = match tlist1,tlist2 with
    | t1::q1,t2::q2 when t1 = t2 -> compatible q1 q2
    | [],[] -> true
    | t1::q1,t2::q2 -> false
    | _ -> false in
  let rec linstanceestdispo envinstances tlist ilist = match ilist with
    | [] -> false
    | tl::q -> ((compatible tl tlist && (not general)) || (unifyable tl tlist && general)) || linstanceestdispo envinstances tlist q in
  let rec touteslesinstancessontdispos pos envinstances averif = match averif with
    | [] -> true
    | (s,tlist)::q -> linstanceestdispo envinstances tlist (List.map fst (smapfind s envinstances pos)) in
  let rec cherchercompatible pos tlistlist tlist = match tlistlist with
    | [] -> typingerror ("Pas d'instance compatible pour la classe "^cident^" dans l'appel de "^f) pos
    | tl::q when ((compatible (fst tl) tlist && (not general)) || (unifyable (fst tl) tlist && general)) && (touteslesinstancessontdispos pos envinstances (snd tl)) -> ()
    | tl::q -> cherchercompatible pos q tlist in
  cherchercompatible pos instances listdestvar



and typatom env envtyps envinstances (general:bool) a = match a with
  | Alident (s,pos) -> find s env pos
  | Auident (s,pos) -> (smapfind s !envconstructors pos).ctyp
  | Aconstant (c,pos) -> typconstant env envtyps envinstances c
  | Aexpr (e,pos) -> typexpr env envtyps envinstances general e
  | Aexprtype (e,p,pos) -> let t = typexpr env envtyps envinstances general e in if t <> typpurtyp env envtyps envinstances p then typingerror "Mauvais type" pos else t

and typconstant env envtyps envinstances c = match c with
  | Cbool _ -> Boolean
  | Cint _ -> Int
  | Cstring _ -> String

and typbranch env envtyps envinstances t (general:bool) b =
  let conflit s a _ = Some a in
  let env = envunion conflit (ensuretyppattern empty envtyps envinstances t b.pattern) env in
  typexpr env envtyps envinstances general b.expr

and ensuretyppattern env envtyps envinstances t p = match p with
  | Ppatarg (p,pos) -> ensuretyppatarg env envtyps envinstances t p
  | Pmulpatarg (s,plist,pos) -> let constr = smapfind s !envconstructors pos in
    if not (unifyable [t]  [get_typ_constr_multi env envtyps envinstances constr plist t s])
    then (typingerror (s^" n'est pas un constructeur du bon type")) pos
    else let rec aux envi plist tlist = match plist,tlist with
      | [],[] -> envi
      | p::q1,t::q2 -> let a = ensuretyppatarg env envtyps envinstances t p in
        let conflit pos s _ _ = typingerror ("l'ident "^s^" est utilisé plusieurs fois") pos in
        let env = aux envi q1 q2 in
        {bindings = Smap.union (conflit pos) a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars}
      | _ -> typingerror ("pas la bonne taille de pattern dans l'utilisation du constructeur "^constr.cident) pos in
    aux env plist constr.ctlist
      

and ensuretyppatarg env envtyps envinstances t p = match p with
  | Pconstant (c,pos) -> if not (unifyable [typconstant env envtyps envinstances c] [t]) then typingerror "Mauvais patterne" pos else env
  | Plident (s,pos) -> add s t env
  | Puident (s,pos) -> (match t,(smapfind s !envconstructors pos).ctyp with
    | Tcustom(s1,l1),Tcustom(s2,l2) -> if s1<>s2 then typingerror (s^" n'est pas un constructeur du bon type custom") pos else env
    | t1,t2 -> if t1<>t2 then (typingerror (s^" n'est pas un constructeur du bon type") pos )else env)
  | Ppattern (p,pos) -> ensuretyppattern env envtyps envinstances t p

and get_typ_constr_multi env envtyps envinstances (constr:constructor) plist tacomp s =
  let t = constr.ctyp in
  let t' = typpatarg env envtyps envinstances (List.hd plist) in
  match tacomp with
    | Tcustom(s,_::_) -> Tcustom(s,[t'])
    | _ -> t

and typpatarg env envtyps envinstances p = match p with
  | Pconstant (c,pos) -> typconstant env envtyps envinstances c
  | Plident (s,pos) -> Tgeneral("a")
  | _ -> failwith "pas implémenté"




and checkexaustivelist env envtyps envinstances tlist plistlist =
  let rec isaident p = match p with
    | Ppatarg (Plident _,pos) -> true
    | Ppatarg (Ppattern (p,_),pos) -> isaident p
    | _ -> false in
  let rec isabool b p = match p with
    | Ppatarg (Pconstant (Cbool (x,_),_),pos) when x = b -> true
    | Ppatarg (Ppattern (p,_),pos) -> isabool b p
    | _ -> false in
  let rec isaconstr c p = match p with
    | Ppatarg (Puident (s,_),pos) when s = c -> true
    | Pmulpatarg (s,_,pos) when s = c -> true
    | Ppatarg (Ppattern (p,_),pos) -> isaconstr c p
    | _ -> false in
  let rec contientidentlist f i l = match l with
    | [] -> []
    | x::q when f (List.nth x i) -> x::contientidentlist f i q
    | _::q -> contientidentlist f i q in
  let rec getthelists l = match l with
    | [] -> []
    | (Pmulpatarg (_,x,pos)::_)::q -> (List.map (fun t -> Ppatarg (t,pos)) x)::getthelists q
    | (Ppatarg(Puident _,pos)::_)::q -> getthelists q
    | (Ppatarg(Ppattern (p,pos),_)::a)::q -> getthelists ((p::a)::q)
    | _ -> failwith "bah alors mais d'ou ca arrive ca" in       (*plus sur de pk cette erreur est la*)
  let rec aux i l =
    if l = [] then false
    else if i>=List.length tlist
    then true
    else (match List.nth tlist i with
      | Boolean -> aux (i+1) (contientidentlist isaident i l) || (aux (i+1) (contientidentlist (isabool true) i l)&&aux (i+1) (contientidentlist (isabool false) i l))
      | Tcustom (s,_) -> aux (i+1) (contientidentlist isaident i l) || grosand (List.map (fun (_,constr) -> not (constr.ctyp = List.nth tlist i) || (aux (i+1) (contientidentlist (isaconstr constr.cident) i l)) && checkexaustivelist env envtyps envinstances constr.ctlist (getthelists (contientidentlist (isaconstr constr.cident) i l))) (smaptolist !envconstructors))
      | _ -> aux (i+1) (contientidentlist isaident i l)) in
  List.length tlist = 0 || aux 0 plistlist


and typpurtyp env envtyps envinstances pt = match pt with
  | Patype (a,pos) -> typatype env envtyps envinstances a
  | Pntype (n,pos) -> typntype env envtyps envinstances n

and typntype env envtyps envinstances n =
  let t,arite = smapfind n.nident envtyps n.pos in
  if List.length n.atypes <> arite
  then typingerror "pas la bonne arité" n.pos
  else match t with
    | Tcustom (s,_) -> Tcustom(s,List.map (typatype env envtyps envinstances) n.atypes)
    | _ -> t

and typatype (env:env) envtyps envinstances a = match a with
  | Apurtype (p,pos) -> typpurtyp env envtyps envinstances p
  | Aident (s,pos) -> let t,arite = smapfind s envtyps pos in if arite = 0 then t else typingerror "devrait etre un type d'arite 0" pos

and typtdecl env envtyps envinstances td =
  if Smap.mem td.dident !envfonctions
    then typingerror (td.dident^" est définie 2 fois") td.pos
else if not (alldifferent td.identlist)
    then typingerror ("toutes les variables de types ne sont pas differentes dans l'appel de "^td.dident) td.pos
else
  let rec aux envtyps l = match l with
    | [] -> envtyps
    | s::q -> aux (Smap.add s (Tgeneral s,0) envtyps) q in
  let envvartyps = aux Smap.empty td.identlist in
  let conflit pos s _ _ = typingerror "conflit dans les patternes" pos in
  let envtyps = Smap.union (conflit td.pos) envtyps envvartyps in
  let rec addinstance envinstances nlist = match nlist with
      | [] -> envinstances
      | n::q -> Smap.add n.nident ((List.map (typatype env envtyps envinstances) n.atypes,[])::(smapfind n.nident envinstances td.pos)) (addinstance envinstances q) in
  let envinstances = addinstance envinstances td.ntypelist in
  lastdefined := td.dident;
  Tarrow(List.map (typpurtyp env envtyps envinstances) td.purtypelist,typpurtyp env envtyps envinstances td.purtype),envvartyps,envinstances

and typdfn env envtyps envinstances addtodeflist (df:defn) tlist t =
  if addtodeflist && Smap.mem df.ident !envfonctions && !lastdefined <> df.ident
  then (typingerror (df.ident^" est définie 2 fois") df.pos)
else
  (if addtodeflist then deflist := df:: !deflist;
  let conflit pos s _ _ = (typingerror ("conflit dans les forall avec "^s) pos) in
  let envtyps = if addtodeflist then Smap.union (conflit df.pos) envtyps (sndr (smapfind df.ident !envfonctions df.pos)) else envtyps in
  let patarglist = df.patargs in
  let conflit pos s _ _ = typingerror ("l'ident "^s^" est utilisé plusieurs fois") pos in
  let rec aux envi envtyps (ptlist:patarg list) tlist = match ptlist,tlist with
    | [],[] -> envi
    | pt::q1,t::q2 -> let a = ensuretyppatarg empty envtyps envinstances t pt in
    let env = aux envi envtyps q1 q2 in
    {bindings = Smap.union (conflit df.pos) a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars}
    | _ -> typingerror "Pas le bon nombre d'arguments" df.pos in
  let env = aux env envtyps patarglist tlist in
  if typexpr env envtyps envinstances addtodeflist df.expr<>t
  then (typingerror ("pas le type censé etre renvoyé par "^ (df.ident)) df.pos)
  else ())

and typdata env envtyps envinstances pos s slist ialist =
  if Smap.mem s !envtyps then typingerror ("conflit dans la définition du type "^s) pos
  else if not (alldifferent slist)
  then typingerror ("toutes les variables de types ne sont pas differentes dans la définition du type "^s) pos
else
  let rec aux envtyps l = match l with
    | [] -> envtyps
    | s::q -> aux (Smap.add s (Tgeneral s,0) envtyps) q in
  let conflit pos s _ _ = (typingerror ("conflit dans les forall avec "^s)) pos in
  let envvartyps = aux Smap.empty slist in
  let envtypsact = Smap.union (conflit pos) envvartyps !envtyps in
  let t = Tcustom (s,List.map (fun s -> fst(smapfind s envvartyps pos)) slist) in
  envtyps := Smap.add s (t,List.length slist) !envtyps;
  let envtypsact = Smap.add s (t,List.length slist) envtypsact in
  let rec aux2 l = match l with
    | [] -> ()
    | (i,alist)::q -> if Smap.mem i !envconstructors
      then typingerror ("Le constructeur "^i^" est défini plusieurs fois") pos
      else envconstructors := Smap.add i {cident = i; ctlist = List.map (typatype env envtypsact envinstances) alist; ctyp = t; cenvvartyps = envvartyps} !envconstructors;aux2 q in
  aux2 ialist
  

and checkforenddef envtyps envinstances pos = 
  let rec isanident p = match p with
    | Plident (s,pos) -> true
    | Ppattern (Ppatarg (p,_) ,pos) -> isanident p
    | _ -> false in
  let rec getchanging dejatrouve i l = match l with
    | [] -> -1
    | x::q when isanident x -> getchanging dejatrouve (i+1) q
    | x::q when dejatrouve -> typingerror "Dans les fonctions, on filtre au plus une valeur" pos
    | x::q -> let _ = getchanging true (i+1) q in i in
  let rec aux posdufiltrage (df:defn) =
    Ppatarg (List.nth df.patargs posdufiltrage,pos) in
  if !lastdefined = ""
  then ()
  else begin let conflit s _ _ = (typingerror ("conflit dans les forall avec "^s)) pos in
  let envtyps = Smap.union conflit envtyps (sndr (smapfind !lastdefined !envfonctions pos)) in
  deflist := List.rev !deflist;
    match !deflist with
      | [] -> typingerror ("La déclaration de "^(!lastdefined)^" doit être suivie de sa définition") pos;
      | x::q -> let posdufiltrage = getchanging false 0 x.patargs in 
        if posdufiltrage = -1
        then (if List.length !deflist >1 then typingerror (!lastdefined^" est définie 2 fois") pos else lastdefined := ""; deflist := [])
        else(match fstr(smapfind !lastdefined !envfonctions pos) with
      | Tarrow(tlist,t) -> if not (checkexaustivelist empty envtyps envinstances [(List.nth tlist posdufiltrage)] (List.map (fun x -> [x]) (List.map (aux posdufiltrage) !deflist))) then typingerror ("Patterne non exhaustif dans la definition de "^(!lastdefined)) pos;
      lastdefined := ""; deflist := []
      | _ -> failwith "pas possible")
  end


and typclass env envtyps envinstances pos s slist tdlist =
  if Smap.mem s !envclasses
    then typingerror ("La classe "^s^" est définie 2 fois") pos;
  let (classenvfonctions : typ Smap.t ref) = ref Smap.empty in
  List.iter (fun td -> let a,b,d = typtdecl env envtyps envinstances td in lastdefined := "";deflist := [];classenvfonctions := Smap.add td.dident a !classenvfonctions;envfonctions := Smap.add td.dident (a,b,Some s,d) !envfonctions)
      (List.map (fun {dident = dident; identlist = identlist; ntypelist = ntypelist;purtypelist = purtypelist;purtype = purtype} -> {dident = dident; identlist = slist; ntypelist = ntypelist;purtypelist = purtypelist;purtype = purtype;pos = pos}) tdlist);
  envclasses := Smap.add s (slist,!classenvfonctions,[]) !envclasses;globalenvinstances := Smap.add s [] !globalenvinstances


and noconseqconflit _ a _ = Some a

and noconseqconflit2 _ a _ = Some a

and ajouter envtyps alist = match alist with
  | (Aident (s,pos))::q when isalident s -> Smap.add s (Tgeneral(s),0) (ajouter envtyps q)
  | (Aident _)::q -> ajouter envtyps q
  | (Apurtype (p,pos))::q -> Smap.union noconseqconflit2 (ajouter envtyps q) (ajouterpurtyp envtyps p)
  | [] -> envtyps

and ajouterpurtyp envtyps p = match p with
  | Patype (a,_) -> ajouter envtyps [a]
  | Pntype (n,_) -> ajouterntyp envtyps n

and ajouterntyp envtyps n = ajouter envtyps n.atypes

and ajouterntyplist envtyps nlist = match nlist with
  | [] -> envtyps
  | n::q -> Smap.union noconseqconflit2 (ajouterntyp envtyps n) (ajouterntyplist Smap.empty q)



and typinstance env envtyps envinstances i deflist = match i with
  | Imularrow (nlist,n,pos) -> (let clident = n.nident in
    let slist,classenvfonctions,flist = smapfind clident !envclasses pos in  
    let rec addinstance envinstances envtyps nlist = match nlist with
      | [] -> envinstances
      | n::q -> Smap.add n.nident ((List.map (typatype env envtyps envinstances) n.atypes,[])::(smapfind n.nident envinstances pos)) (addinstance envinstances envtyps q) in
    let envtyps = ajouterntyplist envtyps nlist in
    let envtyps = ajouter envtyps n.atypes in
    let envinstances = addinstance envinstances envtyps nlist in
    let rec aux5 tlist1 tlist2 =
      if unifyable tlist1 tlist2
      then typingerror ("2 instances différentes de "^n.nident^" peuvent être unifiées") pos in
    List.iter (aux5 (List.map (typatype env envtyps envinstances) n.atypes)) (List.map fst (smapfind n.nident !globalenvinstances pos));
    let rec aux slist tlist = match slist,tlist with
      | [],[] -> Smap.empty
      | s::q1,t::q2 -> Smap.add s t (aux q1 q2)
      | _ -> typingerror "pas le bon nombre de types" pos in
    let substtable = aux slist (List.map (typatype env envtyps envinstances) n.atypes) in
    let subst t pos =match t with
      | Tgeneral s -> smapfind s substtable pos
      | _ -> t in
    List.iter (fun (df:defn) -> (match fstr(smapfind df.ident !envfonctions pos) with 
      | Tarrow(tlist,t) -> typdfn env envtyps envinstances false df (List.map (fun t -> subst t df.pos) tlist) (subst t df.pos)
      | _ -> failwith "c'est pas possible")) deflist;
    let rec isanident p = match p with
      | Plident (s,_) -> true
      | Ppattern (Ppatarg (p,_) ,_) -> isanident p
      | _ -> false in
    let rec aux4 s (l:defn list) = match l with
      | [] -> []
      | df::q when df.ident = s -> df::(aux4 s q)
      | df::q -> aux4 s q in
    let rec getchanging dejatrouve i l = match l with
      | [] -> -1
      | x::q when isanident x -> getchanging dejatrouve (i+1) q
      | x::q when dejatrouve -> typingerror "Dans les fonctions, on filtre au plus une valeur" pos
      | x::q -> let _ = getchanging true (i+1) q in i in
    let rec aux posdufiltrage (df:defn) =
      Ppatarg (List.nth df.patargs posdufiltrage,pos) in
    let aux3 s t = match t with
      | Tarrow (tlist,_) ->
      let l = aux4 s deflist in
      (match l with
      | [] -> typingerror ("Il manque une définition de "^s) pos;
      | x::q -> let posdufiltrage = getchanging false 0 x.patargs in
      if posdufiltrage = -1 then ()
      else if not (checkexaustivelist env envtyps envinstances [subst (List.nth tlist posdufiltrage) pos] (List.map (fun x -> [x]) (List.map (aux posdufiltrage) l)))
      then (typingerror ("pas exhaustif dans l'appel de l'instance pour la classe "^clident) pos))
      | _ -> failwith "pas possible" in
    let rec aux6 nlist = match nlist with
      | [] -> []
      | n::q -> (n.nident,List.map (typatype env envtyps envinstances)n.atypes)::aux6 q in
    let listdesoblig = aux6 nlist in
    Smap.iter aux3 classenvfonctions;globalenvinstances := Smap.add n.nident ((List.map (typatype env envtyps envinstances) n.atypes,listdesoblig)::(smapfind n.nident !globalenvinstances n.pos)) !globalenvinstances)

  | Iarrow (n1,n2,pos) -> typinstance env envtyps envinstances (Imularrow([n1],n2,pos)) deflist
  | Intype (n,pos) -> typinstance env envtyps envinstances (Imularrow([],n,pos)) deflist
  











