open Purescript_ast

type typ =
  | Unit
  | Int
  | String
  | Boolean
  | Tcustom of string * typ list
  | Tarrow of typ list*typ
  | Tvar of tvar
  | Tgeneral of string
  | Tany

and tvar = {id : int; mutable def : typ option}



let rec string_of_typlist tl = match tl with
  | t::q -> string_of_typ t^" "^string_of_typlist q
  | [] -> ""

and string_of_typ t = match t with
  | Unit -> "unit"
  | Int -> "Int"
  | String -> "String"
  | Boolean -> "Boolean"
  | Tcustom (s,tl) -> "("^string_of_typlist tl^")"^s
  | Tarrow (tl,t) -> "("^string_of_typlist tl^")"^string_of_typ t
  | Tvar v -> string_of_int v.id
  | Tgeneral s -> "'"^s
  | Tany -> "any"



(*l'ast de l'arbre renvoyé par le typage*)  
type tfile =
  {timports : timports; tvdecls : tvdecl list}

and timports = TImport

and tvdecl =
  | TDdefn of tdefn
  | TDtdecl of ttdecl
  | TDdata of ident * (ident list) * ((ident * (tatype list)) list)
  | TDclass of ident * (ident list) * ttdecl list
  | TDinstance of tinstance * tdefn list

and tdefn =
  {tident : ident; tpatargs : tpatarg list; texpr : texpr}

and ttdecl =
  {tdident : ident; tidentlist : ident list; tntypelist : tntype list; tpurtypelist : tpurtype list; tpurtype : tpurtype}

and tntype =
  {tnident : ident; tatypes : tatype list}

and tatype =
  | TAident of ident
  | TApurtype of tpurtype

and tpurtype =   
  | TPatype of tatype
  | TPntype of tntype

and tinstance =
  | TIntype of tntype
  | TIarrow of tntype * tntype
  | TImularrow of tntype list * tntype

and tpatarg =
  | TPconstant of tconstant
  | TPlident of ident
  | TPuident of ident
  | TPpattern of tpattern

and tpattern =
  | TPpatarg of tpatarg
  | TPmulpatarg of ident * tpatarg list

and tconstant =
  | TCbool of bool
  | TCint of int
  | TCstring of string

and tatom =
  | TAconstant of tconstant*typ
  | TAlident of ident*typ
  | TAuident of ident*typ
  | TAexpr of texpr*typ
  | TAexprtype of texpr * tpurtype*typ

and texpr =
  | TEatom of tatom*typ
  | TEbinop of binop * texpr * texpr*typ
  | TElident of ident * tatom list *typ
  | TEuident of ident * tatom list *typ
  | TEif of texpr * texpr * texpr*typ
  | TEdo of texpr list*typ
  | TElet of tbinding list * texpr*typ
  | TEcase of texpr * tbranch list*typ

and tbinding =
  {tident : ident; tbindexpr : texpr}

and tbranch =
  {tpattern : tpattern; texpr : texpr}


open Format


(*le pretty printer de l'arbre de typage*)
let print_tconstant fmt c = match c with
	| TCbool (b) -> fprintf fmt "boolean %s" (string_of_bool b)
	| TCint (n) -> fprintf fmt "int %d" n
	| TCstring (s) -> fprintf fmt "string %s" s

let rec print_tatom fmt a = match a with
	| TAconstant (c,_) -> fprintf fmt "%a" print_tconstant c
	| TAlident (l,_) | TAuident (l,_) -> fprintf fmt "%s" l
	| TAexpr (e,_) -> fprintf fmt "%a" print_texpr e
	| TAexprtype (e,t,_) -> fprintf fmt "%a :: %a" print_texpr e print_tpurtype t

and print_texpr fmt e = match e with
	| TEbinop (b,e1,e2,_) -> fprintf fmt "(%a %a %a)" print_texpr e1 print_binop b print_texpr e2
	| TElident (s,a,_) | TEuident (s,a,_) -> fprintf fmt "%s [@[<hov>%a@]]" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_tatom) a
	| TEif (e1,e2,e3,_) -> fprintf fmt "if %a then %a else %a" print_texpr e1 print_texpr e2 print_texpr e3
	| TEdo (e,_) -> fprintf fmt "do {@[<hov>%a@]}" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_texpr) e
	| TElet (b,e,_) -> fprintf fmt "let {@[<hov>%a@]} in %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ")	print_tbindings) b print_texpr e
	| TEatom (a,_) -> print_tatom fmt a
	| TEcase (e,b,_) -> fprintf fmt "case %a of {@[<hov>%a@]}" print_texpr e	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@") print_tbranch) b

and print_tbindings fmt (b:tbinding) =
	fprintf fmt "%s = %a" b.tident print_texpr b.tbindexpr

and print_tbranch fmt b =
	fprintf fmt "%a -> %a" print_tpattern b.tpattern print_texpr b.texpr

and print_tpatarg fmt p =match p with
	| TPconstant (c) -> fprintf fmt "%a" print_tconstant c
	| TPlident (s) | TPuident (s) -> fprintf fmt "%s" s
	| TPpattern (p) -> fprintf fmt "%a" print_tpattern p

and print_tpattern fmt p = match p with
	| TPpatarg (p) -> fprintf fmt "%a" print_tpatarg p
	| TPmulpatarg (s,plist) -> fprintf fmt "%s(@[<hov>%a@])" s Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tpatarg) plist

and print_tatype fmt a = match a with
	| TAident (s) -> fprintf fmt "%s" s
	| TApurtype (p) -> fprintf fmt "%a" print_tpurtype p

and print_tntype fmt n =
	fprintf fmt "%s(@[<hov>%a@])" n.tnident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tatype) n.tatypes

and print_tpurtype fmt p = match p with
	| TPatype (a) -> fprintf fmt "%a" print_tatype a
	| TPntype (n) -> fprintf fmt "%a" print_tntype n

and print_ttdecl fmt t =
	fprintf fmt "%s :: forall @[<hov>%a@] (@[<hov>%a@]) => (@[<hov>%a@]) -> %a" t.tdident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) t.tidentlist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tntype) t.tntypelist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tpurtype) t.tpurtypelist print_tpurtype t.tpurtype

and print_tinstance fmt i = match i with
	| TIntype (n) -> fprintf fmt "%a" print_tntype n
	| TIarrow (n1,n2) -> fprintf fmt "%a => %a" print_tntype n1 print_tntype n2
	| TImularrow (nlist,n) -> fprintf fmt "(@[<hov>%a@]) => %a" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") print_tntype) nlist print_tntype n

and print_tdefn fmt d =
	fprintf fmt "%s (@[<hov>%a@]) = %a" d.tident Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tpatarg) d.tpatargs print_texpr d.texpr

and print_tualist fmt (u,alist) =
	fprintf fmt "%s (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tatype) alist

and print_tvdecl fmt d = match d with
	| TDdata (u,llist,ua) -> fprintf fmt "data %s (@[<hov>%a@]) = (@[<hov>%a@])" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) llist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tualist) ua
	| TDclass (u,llist,tlist) -> fprintf fmt "%s (@[<hov>%a@]) where {@[<hov>%a@]}" u Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ident) llist
	Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_ttdecl) tlist
	| TDinstance (i,dlist) -> fprintf fmt "%a where {[<hov>%a@]}" print_tinstance i Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")	print_tdefn) dlist
	| TDtdecl (t) -> fprintf fmt "%a" print_ttdecl t
	| TDdefn (d) -> fprintf fmt "%a" print_tdefn d

and print_tfile fmt f =
	fprintf fmt "@[<hov>%a@]\n" Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ")print_tvdecl) f.tvdecls




module Smap = Map.Make(String)


(*le message d'erreur du typage*)
exception TypingError of string * position

let typingerror s pos = raise (TypingError (s,pos))


(*on utilise cette fonction au lieu de Smap.find pour pouvoir renvoyer une erreur custom avec la position et pas juste NotFound*)
let smapfind s smap pos =
  if Smap.mem s smap
  then Smap.find s smap
  else typingerror ("L'identificateur "^s^" n'est pas défini") pos

let smapfind2 s smap pos =
  if Smap.mem s smap
    then Smap.find s smap
    else typingerror ("L'identificateur "^s^" n'est quoicoupas défini") pos


let string_to_list s = List.init (String.length s) (String.get s)


let isalident s = List.mem s.[0] (string_to_list "azertyuiopqsdfghjklmwxcvbn")


(*l'environnement global des instances, avec en premier les types qui remplacent les types polymorphes et en second les instances dont cette instance a besoin*)
let (globalenvinstances : (typ list*(ident*(typ list))list ) list Smap.t ref) = ref (Smap.add "Show" ([([Int],[]);([Boolean],[])]) Smap.empty)


let print_bool b =
  if b then print_endline "true"
  else print_endline "false"



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
    | Tany::q1,_::q2 | _::q1,Tany::q2 -> aux q1 q2
    | Unit::q1,Unit::q2 | Int::q1,Int::q2 | String::q1,String::q2 | Boolean::q1,Boolean::q2 -> aux q1 q2
    | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> aux l1 l2 && aux q1 q2
    | Tgeneral s::q1,t::q2 -> if Smap.mem s !assoc
      then aux ((Smap.find s !assoc)::q1) (t::q2)
      else (assoc := Smap.add s t !assoc;aux q1 q2)
    | t::q1,Tgeneral s::q2 -> aux tlist2 tlist1
    | t1::_,t2::_-> false
    | _ -> false in
  aux tlist1 tlist2
  

let unification tlist1 tlist2 pos =
  let (assoc:typ Smap.t ref) = ref Smap.empty in
  let rec aux tlist1 tlist2  = match tlist1,tlist2 with
    | [],[] -> ()
    | Tany::q1,_::q2 | _::q1,Tany::q2 -> aux q1 q2
    | t1::q1,t2::q2 when t1=t2 -> aux q1 q2
    | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> aux l1 l2;aux q1 q2
    | Tgeneral s::q1,t::q2 -> if Smap.mem s !assoc
      then aux ((Smap.find s !assoc)::q1) (t::q2)
      else (assoc := Smap.add s t !assoc;aux q1 q2)
    | t::q1,Tgeneral s::q2 -> aux tlist2 tlist1
    | t1::_,t2::_ -> typingerror ("Pas d'unification possible entre les types "^string_of_typ t1^" et "^string_of_typ t2) pos
    | _ -> typingerror "Pas d'unification possible" pos in
  aux tlist1 tlist2;
  !assoc


module Sset = Set.Make(String)

module Vset = Set.Make(V)


let rec fvars t = match head t with
  | Unit | Int | String | Boolean | Tany -> Vset.empty
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
    | Int | String | Unit | Boolean | Tany -> t
    | Tcustom (s,tlist) -> Tcustom(s,List.map subst tlist)
    | Tarrow (tlist, t2) -> Tarrow (List.map subst tlist, subst t2)
    | Tgeneral s -> Tgeneral s
  in
  subst tx.typ

let mem x env =
  Smap.mem x env.bindings




type constructor = {cident : string; ctlist : typ list; ctyp : typ; cenvvartyps : (typ*int) Smap.t;catypes : atype list}

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
let (tdeflist:tdefn list ref) = ref []


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


let rec listmapl f l = match l with
    | [] -> []
    | x::q -> let a = (f x) in a@ listmapl f q


let rec compatible tl1 tl2 = match tl1,tl2 with
  | [],[] -> true
  | Tany::q1,_::q2 | _::q1,Tany::q2 -> compatible q1 q2
  | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> compatible l1 l2 && compatible q1 q2
  | t1::q1,t2::q2 -> t1 = t2 && compatible q1 q2
  | _ -> false



let rec typfile f =
  let env = add "unit" Unit empty in
  let envtyps = ref (smapaddlist Smap.empty (["Int";"String";"Unit";"Boolean";"Effect"],[(Int,0);(String,0);(Unit,0);(Boolean,0);(Tcustom("Effect",[Unit]),1)])) in
  let arbretype = listmapl (typdecl env envtyps !globalenvinstances) f.decls in
  let a = checkforenddef env !envtyps !globalenvinstances f.pos true in
  if not (Smap.mem "main" !envfonctions) then typingerror "Pas de fonction main définie" f.pos else {timports = TImport; tvdecls = arbretype@a}


and typdecl env envtyps envinstances d = match d with
  | Dtdecl (td,pos) -> let ab = checkforenddef env !envtyps !globalenvinstances pos true in let a,b,c,d = typtdecl env !envtyps Smap.empty td in envfonctions := Smap.add td.dident (a,b,None,c) !envfonctions;ab@[TDtdecl(d)]
  | Ddefn (df,pos) -> (match fstr(smapfind df.ident !envfonctions pos) with
    | Tarrow(tlist,t) -> let _ = typdfn env !envtyps !globalenvinstances true df tlist t in []
    | _ -> failwith "pas possible")
  | Ddata (s,slist,ialist,pos) -> let a = checkforenddef env !envtyps !globalenvinstances pos true in let rep = typdata env envtyps envinstances pos s slist ialist in a@[TDdata(s,slist,rep)]
  | Dclass (s,slist,tdlist,pos) -> let a = checkforenddef env !envtyps !globalenvinstances pos true in let rep = typclass env !envtyps !globalenvinstances pos s slist tdlist in a@[TDclass(s,slist,rep)]
  | Dinstance (i,deflist,pos) -> let a = checkforenddef env !envtyps !globalenvinstances pos true in let l = typinstance env !envtyps !globalenvinstances i deflist in a@l



and typexpr env envtyps (envinstances:(typ list * (ident * typ list) list) list Smap.t) (general:bool) expr = match expr with
  | Ebinop (b,e1,e2,pos) -> (let t1,e1' = typexpr env envtyps envinstances general e1 in let t2,e2' = typexpr env envtyps envinstances general e2 in match b with
    | Bequals pos | Bnotequals pos ->  if List.mem t1 [Int;String;Boolean;Unit] then (if t1 <> t2 then typingerror ("Les types "^string_of_typ t1^" et "^string_of_typ t2^" ne sont pas compatibles") pos else (Boolean,TEbinop (b,e1',e2',Boolean))) else typingerror ("Le type "^string_of_typ t1^" n'est pas compatible avec l'égalité") pos
    | Binf pos | Binfeq pos | Bsup pos | Bsupeq pos -> if t1 <> Int then typingerror ("On s'attendait à du Int mais du "^string_of_typ t1^" a été donné") pos else if t2 <> Int then typingerror ("On s'attendait à du Int mais du "^string_of_typ t2^" a été donné") pos else (Boolean,TEbinop (b,e1',e2',Boolean))
    | Bplus pos | Bminus pos | Btimes pos | Bdivide pos -> if t1 <> Int then typingerror ("On s'attendait à du Int mais du "^string_of_typ t1^" a été donné") pos else if t2 <> Int then typingerror ("On s'attendait à du Int mais du "^string_of_typ t2^" a été donné") pos else (Int,TEbinop(b,e1',e2',Int))
    | Bor pos | Band pos -> if t1 <> Boolean then typingerror ("On s'attendait à du Boolean mais du "^string_of_typ t1^" a été donné") pos else if t2 <> Boolean then typingerror ("On s'attendait à du Boolean mais du "^string_of_typ t2^" a été donné") pos else (Boolean,TEbinop(b,e1',e2',Boolean))
    | Bcons pos -> if t1 <> String then typingerror ("On s'attendait à du String mais du "^string_of_typ t1^" a été donné") pos else if t2 <> String then typingerror ("On s'attendait à du String mais du "^string_of_typ t2^" a été donné") pos else (String,TEbinop(b,e1',e2',String))
  )
  | Eatom (a,pos) -> let t,a' = typatom env envtyps envinstances general a in (t,TEatom(a',t))
  | Eif (e1,e2,e3,pos) -> let t1,e1' = typexpr env envtyps envinstances general e1 in if t1 <> Boolean then typingerror ("On s'attendait à du Boolean mais du "^string_of_typ t1^" a été donné") pos else let t2,e2' = typexpr env envtyps envinstances general e2 in let t3,e3' = typexpr env envtyps envinstances general e3 in if t3 <> t2 then typingerror ("Les types "^string_of_typ t2^" et "^string_of_typ t3^" ne sont pas compatibles") pos else (t2,TEif(e1',e2',e3',t2))
  | Edo (elist,pos) -> (let el = List.map (fun e -> let t,e' = typexpr env envtyps envinstances general e in if t <> Tcustom ("Effect",[Unit]) then typingerror ("On s'attendait à du Effect Unit mais du "^string_of_typ t^" a été donné") pos else e') elist in (Tcustom ("Effect",[Unit]),TEdo(el,Tcustom ("Effect",[Unit]))))
  | Elet (blist,e,pos) ->
    let rec aux env envtyps envinstances l = match l with
      | b::q -> let t,el= typexpr env envtyps envinstances general b.bindexpr in let a,r = aux (add_gen b.ident t env) envtyps envinstances q in a,{tbindexpr = el;tident = b.ident}::r
      | [] -> env,[] in
    let env,el = aux env envtyps envinstances blist in
    let t,e' = typexpr env envtyps envinstances  general e in (t,TElet(el,e',t))
  | Ecase (e,blist,pos) -> let t,e' = typexpr env envtyps envinstances general e in (match blist with
    | [] -> typingerror "Pattern vide" pos
    | b::q -> let t',_ = typbranch env envtyps envinstances t general b in let l = List.map (fun x-> let a,b = typbranch env envtyps envinstances t general x in if a <> t' then typingerror ("Les types "^string_of_typ a^" et "^string_of_typ t'^" ne sont pas compatibles") pos else b) blist in
    if not (checkexaustivelist env envtyps envinstances [t] (List.map (fun x -> [x]) (List.map (fun b -> b.pattern) blist))) then typingerror "Pattern non exhaustif" pos else (t',TEcase(e',l,t))
  )
  | Elident (f,alist,pos) -> let envinstances = Smap.union noconseqconflit envinstances (fourth (smapfind f !envfonctions pos))  in if mem f env then typingerror (f^" n'est pas une fonction") pos
  else (let name = (match thrd (smapfind f !envfonctions pos) with
  | None -> f
  | Some cident -> let instances = smapfind cident envinstances pos in find_compatible_instance pos envinstances cident f general instances (List.map (fun a -> fst(typatom env envtyps envinstances general a)) alist)) in
    let tlist,t = (match fstr(smapfind f !envfonctions pos) with
    | Tarrow(tlist,t) -> tlist,t
    | _ -> failwith "pas possible") in
    let dejapris = ref Smap.empty in
      let rec aux tlist alist = (match tlist,alist with
      | [],[] -> ()
      | t::q1,a::q2 -> (match t with
        | Tgeneral s -> let t',_ = typatom env envtyps envinstances general a in
          (match t' with
            | Tgeneral _ -> aux q1 q2
            | _ -> if Smap.mem s !dejapris && t' <> smapfind s !dejapris pos
              then typingerror ("Dans l'appel de "^f^" les types "^string_of_typ t'^" et "^string_of_typ (smapfind s !dejapris pos)^" ne sont pas compatibles") pos
              else dejapris := Smap.add s t' !dejapris;aux q1 q2)
        | _ -> let b = fst (typatom env envtyps envinstances general a) in if not( unifyable [b] [t]) then typingerror ("mauvais argument dans l'appel de "^f^", on s'attendait à du "^string_of_typ t^" mais du "^string_of_typ b^" a été donné") pos else aux q1 q2)
      | _ -> typingerror ("la fonction "^f^" ne prend pas autant d'arguments") pos) in
      aux tlist alist; let trep = substitute !dejapris pos t in (trep,TElident(name,List.map (fun a -> snd(typatom env envtyps envinstances general a)) alist,trep)))
  | Euident (s,alist,pos) -> let constr = smapfind s !envconstructors pos in
    let matchingtt =  (List.map (typatom env envtyps envinstances general) alist)  in
    let matchingfst = unification (List.map fst matchingtt) constr.ctlist pos in
    let matchingsnd = List.map snd matchingtt in
    let trep = substitute matchingfst pos constr.ctyp in (trep,TEuident(s,matchingsnd,trep))



and find_compatible_instance pos envinstances cident f (general:bool) instances tlist =
  let l = List.length (smapfind cident envinstances pos) in
  let slist,classenvfonctions,flist = smapfind cident !envclasses pos in
  let ftlist = match smapfind f classenvfonctions pos with
    | Tarrow(tlist,t) -> tlist
    | _ -> failwith "pas possible" in
  let rec constrsmap sm tlist1 tlist2 = match tlist1,tlist2 with
    | [],[] -> sm
    | Tgeneral s::q1, t::q2 -> Smap.add s t (constrsmap sm q1 q2)
    | t1::q1,t2::q2 when t1 = t2 -> constrsmap sm q1 q2
    | t1::_,t2::_ -> typingerror ("Mauvais argument dans l'appel de "^f^" définie par la classe "^cident^", les types "^string_of_typ t1^" et "^string_of_typ t2^" ne sont pas compatibles") pos
    | _ -> typingerror ("L'appel de "^f^" n'est pas compatible avec sa définition dans la classe "^cident) pos in
  let sm = constrsmap Smap.empty ftlist tlist in
  let listdestvar = List.map (fun s -> smapfind s sm pos) slist in
  let rec compatible dejapris tlist1 tlist2 = match tlist1,tlist2 with
    | t1::q1,t2::q2 when t1 = t2 -> compatible dejapris q1 q2
    | _,[] | [],_ -> true
    | Tcustom(s1,l1)::q1,Tcustom(s2,l2)::q2 when s1 = s2 -> cherchercompatible instances l2 (List.length l2 -1) <> -1 && compatible dejapris q1 q2
    | t1::q1,t2::q2 -> false
    (*| _ -> print_endline "bizarre";false*)
  and linstanceestdispo envinstances tlist ilist = match ilist with
    | [] -> false
    | tl::q -> (((not general) && compatible Smap.empty tl tlist) || (general && unifyable tl tlist)) || linstanceestdispo envinstances tlist q
  and touteslesinstancessontdispos pos envinstances averif = match averif with
    | [] -> true
    | (s,tlist)::q -> linstanceestdispo envinstances tlist (List.map fst (smapfind s envinstances pos))
  and cherchercompatible tlistlist tlist i = match tlistlist with
    | [] -> i
    | tl::q when ((not general) && (compatible Smap.empty (fst tl) tlist) || (general && unifyable (fst tl) tlist)) && (touteslesinstancessontdispos pos envinstances (snd tl)) -> i
    | tl::q -> cherchercompatible q tlist (i-1) in
  let a = cherchercompatible instances listdestvar (l-1) in
  if(a = -1)
    then typingerror ("Pas d'instance compatible pour la classe "^cident^" dans l'appel de "^f^" avec les types "^string_of_typlist tlist^" en arguments") pos
    else "."^cident^"."^f^"."^(string_of_int a)



and typatom env envtyps envinstances (general:bool) a = match a with
  | Alident (s,pos) -> let t = find s env pos in (t,TAlident(s,t))
  | Auident (s,pos) -> let t = (smapfind s !envconstructors pos).ctyp in (t,TAuident(s,t))
  | Aconstant (c,pos) -> let t,c' = typconstant env envtyps envinstances c in (t,TAconstant(c',t))
  | Aexpr (e,pos) -> let t,e' = typexpr env envtyps envinstances general e in (t,TAexpr(e',t))
  | Aexprtype (e,p,pos) -> let t,e' = typexpr env envtyps envinstances general e in let t2,p' = typpurtyp env envtyps envinstances p in if t<>t2 then typingerror ("On s'attendait à du "^string_of_typ t2^" mais du "^string_of_typ t^" a été donné") pos else (t,TAexprtype(e',p',t))

and typconstant env envtyps envinstances c = match c with
  | Cbool (b,_) -> (Boolean,TCbool(b))
  | Cint (n,_) -> (Int,TCint(n))
  | Cstring (s,_) -> (String,TCstring(s))

and typbranch env envtyps envinstances t (general:bool) b =
  let conflit s a _ = Some a in
  let a,tp = ensuretyppattern empty envtyps envinstances t b.pattern in
  let env = envunion conflit a env in
  let t,texp = typexpr env envtyps envinstances general b.expr in
  t,{tpattern = tp;texpr = texp}

and ensuretyppattern env envtyps envinstances t p = match p with
  | Ppatarg (p,pos) -> let env2,p' = ensuretyppatarg env envtyps envinstances t p in (env2,TPpatarg(p'))
  | Pmulpatarg (s,plist,pos) -> let constr = smapfind s !envconstructors pos in
  let b = get_typ_constr_multi env envtyps envinstances constr plist t s pos in
    if not (unifyable [t]  [b])
    then (typingerror ("On s'attendait à du "^string_of_typ t^" mais "^s^" est un constructeur du type "^string_of_typ b)) pos
    else let rec aux envi plist tlist = match plist,tlist with
      | [],[] -> envi,[]
      | p::q1,t::q2 -> let a,p' = ensuretyppatarg env envtyps envinstances t p in
        let conflit pos s _ _ = typingerror ("l'ident "^s^" est utilisé plusieurs fois") pos in
        let env,l = aux envi q1 q2 in
        {bindings = Smap.union (conflit pos) a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars},p'::l
      | _ -> typingerror ("pas la bonne taille de pattern dans l'utilisation du constructeur "^constr.cident) pos in
    let mapl = unification [t] [b] pos in
    let rec replace mapl tl = match tl with
      | Tgeneral s::q -> if Smap.mem s mapl then smapfind s mapl pos::replace mapl q else Tgeneral s::replace mapl q
      | t::q -> t::replace mapl q
      | [] -> [] in
    let env,plist' = aux env plist (replace mapl constr.ctlist) in (env,TPmulpatarg(s,plist'))
      

and ensuretyppatarg env envtyps envinstances t p = match p with
  | Pconstant (c,pos) -> let t',c' = typconstant env envtyps envinstances c in if not (unifyable [t'] [t]) then typingerror ("On s'attendait à du "^string_of_typ t^" mais du "^string_of_typ t'^" a été donné") pos else env,TPconstant(c')
  | Plident (s,pos) -> add s t env,TPlident(s)
  | Puident (s,pos) -> (match t,(smapfind s !envconstructors pos).ctyp with
    | Tcustom(s1,[]),Tcustom(s2,[]) -> if s1<>s2 then typingerror (s^" est un constructeur du type "^s2^", or, le type "^s1^" était attendu") pos else env,TPuident(s)        
    | t1,t2 -> if not(compatible [t1] [t2]) then (typingerror ("On s'attendait à du "^string_of_typ t1^" mais "^s^" est un constructeur du type "^string_of_typ t2) pos )else env,TPuident(s))
  | Ppattern (p,pos) -> let env,p' = ensuretyppattern env envtyps envinstances t p in env,TPpattern(p')


(*cette fonction a l'air ridicule mais il fut un temps où elle était plus compliqué*)
and get_typ_constr_multi env envtyps envinstances (constr:constructor) plist tacomp s pos =
  let t = constr.ctyp in
  t

and typpatarg env envtyps envinstances p = match p with
  | Pconstant (c,pos) -> let t,c' = typconstant env envtyps envinstances c in t,TPconstant(c')
  | Plident (s,pos) -> Tgeneral("a"),TPlident(s)
  | Puident(s,pos) -> (Smap.find s !envconstructors).ctyp,TPuident(s) 
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
    | _ -> failwith "pas censé arriver" in       
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
  | Patype (a,pos) -> let t,a' = typatype env envtyps envinstances a in t,TPatype(a')
  | Pntype (n,pos) -> let t,n' = typntype env envtyps envinstances n in t,TPntype(n')

and typntype env envtyps envinstances n =
  let t,arite = smapfind n.nident envtyps n.pos in
  if List.length n.atypes <> arite
  then typingerror "pas la bonne arité" n.pos
  else match t with
    | Tcustom (s,_) -> let alist = List.map (typatype env envtyps envinstances) n.atypes in Tcustom(s,List.map fst alist),{tnident = n.nident;tatypes = List.map snd alist}
    | _ -> let alist = List.map (typatype env envtyps envinstances) n.atypes in t,{tnident = n.nident;tatypes = List.map snd alist}

and typatype (env:env) envtyps envinstances a = match a with
  | Apurtype (p,pos) -> let t,p' = typpurtyp env envtyps envinstances p in t,TApurtype(p')
  | Aident (s,pos) -> let t,arite = smapfind2 s envtyps pos in if arite = 0 then t,TAident(s) else typingerror "devrait etre un type d'arite 0" pos

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
      | n::q -> Smap.add n.nident ((List.map (fun a -> fst (typatype env envtyps envinstances a)) n.atypes,[])::(smapfind n.nident envinstances td.pos)) (addinstance envinstances q) in
  let envinstances = addinstance envinstances td.ntypelist in
  lastdefined := td.dident;
  Tarrow(List.map (fun p -> fst(typpurtyp env envtyps envinstances p) ) td.purtypelist,fst(typpurtyp env envtyps envinstances td.purtype)),envvartyps,envinstances,
  {tdident = td.dident;tidentlist = td.identlist;tntypelist = List.map (fun n -> snd(typntype env envtyps envinstances n)) td.ntypelist;tpurtypelist = List.map (fun p -> snd(typpurtyp env envtyps envinstances p)) td.purtypelist;tpurtype = snd(typpurtyp env envtyps envinstances td.purtype)}

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
    | [],[] -> envi,[]
    | pt::q1,t::q2 -> let a,p' = ensuretyppatarg empty envtyps envinstances t pt in
    let env,pl = aux envi envtyps q1 q2 in
    {bindings = Smap.union (conflit df.pos) a.bindings env.bindings; fvars = Vset.union a.fvars env.fvars},p'::pl
    | _ -> typingerror "Pas le bon nombre d'arguments" df.pos in
  let env,pl = aux env envtyps patarglist tlist in
  let zzz = fst(typexpr env envtyps envinstances addtodeflist df.expr) in
  if not (compatible [zzz] [t])
  then (typingerror ((df.ident)^" est censé renvoyer du "^string_of_typ t^" mais du "^string_of_typ zzz^" a été donné") df.pos)
  else if addtodeflist then (let a = {tident = df.ident;tpatargs = pl;texpr = snd(typexpr env envtyps envinstances addtodeflist df.expr)} in tdeflist := a:: (!tdeflist);a)
  else {tident = df.ident;tpatargs = pl;texpr = snd(typexpr env envtyps envinstances addtodeflist df.expr)})


and typdata env envtyps envinstances pos s slist ialist =
  if Smap.mem s !envtyps then typingerror ("conflit dans la définition du type "^s) pos
  else if not (alldifferent slist)
  then typingerror ("toutes les variables de types ne sont pas differentes dans la définition du type "^s) pos
else
  let rec aux envtyps l = match l with
    | [] -> envtyps
    | s::q -> aux (Smap.add s (Tgeneral s,0) envtyps) q in
  let conflit s a _ = Some a in
  let envvartyps = aux Smap.empty slist in
  let envtypsact = Smap.union (conflit) envvartyps !envtyps in
  let t = Tcustom (s,List.map (fun s -> fst(smapfind s envvartyps pos)) slist) in
  envtyps := Smap.add s (t,List.length slist) !envtyps;
  let envtypsact = Smap.add s (t,List.length slist) envtypsact in
  let rec mentionnesa alist = match alist with
    | [] -> Sset.empty
    | Aident(s,_)::q -> Sset.add s (mentionnesa q)
    | Apurtype(p,_)::q -> Sset.union (mentionnesp p) (mentionnesa q)
  and mentionnesp p = match p with
    | Patype(a,_) -> mentionnesa [a]
    | Pntype(n,_) -> mentionnesn n
  and mentionnesn n =
    Sset.add n.nident (mentionnesa n.atypes) in

  let rec remplacer mentions t = match t with
    | Tcustom(s,tl) ->  Tcustom(s,List.map (remplacer mentions) tl)
    | Tgeneral(s) -> if Sset.mem s mentions then Tgeneral(s) else Tany
    | t -> t in
  

  let rec aux2 l = match l with
    | [] -> ()
    | (i,alist)::q -> if Smap.mem i !envconstructors
      then typingerror ("Le constructeur "^i^" est défini plusieurs fois") pos
      else envconstructors := Smap.add i {cident = i; ctlist = List.map (fun a -> fst(typatype env envtypsact envinstances a)) alist; ctyp = remplacer (mentionnesa alist) t; cenvvartyps = envvartyps;catypes = alist} !envconstructors;aux2 q in
  aux2 ialist;List.map (fun (s,l) -> s,List.map (fun a -> snd(typatype env envtypsact envinstances a)) l) ialist
  

(*appelé quand on a fini d'énumérer toutes les définitions d'une fonction, vérifie l'exhaustivité et construit la nouvelle fonction qui n'a qu'une définition*)
and checkforenddef (env:env) envtyps envinstances pos param = 
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
  then []
  else begin let conflit s _ _ = (typingerror ("conflit dans les forall avec "^s)) pos in
  let envtyps = if param then Smap.union conflit envtyps (sndr (smapfind !lastdefined !envfonctions pos)) else envtyps in
  deflist := List.rev !deflist;
  tdeflist := List.rev !tdeflist;
    match !deflist with
      | [] -> typingerror ("La déclaration de "^(!lastdefined)^" doit être suivie de sa définition") pos;
      | x::q -> let posdufiltrage = getchanging false 0 x.patargs in 
        if not param then ()
        else if posdufiltrage = -1
        then (if List.length !deflist >1 then typingerror (!lastdefined^" est définie 2 fois") pos else ())
        else(match fstr(smapfind !lastdefined !envfonctions pos) with
      | Tarrow(tlist,t) -> if not (checkexaustivelist env envtyps envinstances [(List.nth tlist posdufiltrage)] (List.map (fun x -> [x]) (List.map (aux posdufiltrage) !deflist))) then typingerror ("Patterne non exhaustif dans la definition de "^(!lastdefined)) pos;      
      | _ -> failwith "pas possible")
  ;
  let posdufiltrage = getchanging false 0 (List.hd !deflist).patargs in
  if posdufiltrage <> -1
  then begin
  let tlist = ref [] in
  let t = ref Int in
  (match fstr(smapfind2 !lastdefined !envfonctions pos) with
      | Tarrow(tl,ty) -> tlist := tl;t := ty
      | _ -> failwith "pas possible");
  let blist = List.map (fun d -> {tpattern = TPpatarg( (List.nth d.tpatargs posdufiltrage)); texpr = d.texpr}) !tdeflist in
  let e = TEcase(TEatom(TAlident("$matching$",List.nth !tlist posdufiltrage),List.nth !tlist posdufiltrage),blist,!t) in
  let d = List.hd !tdeflist in
  let rec look_for_names patarglist old = match patarglist,old with
    | p::q1,TPlident("_")::q2 -> p::look_for_names q1 q2
    | p1::q1,p2::q2 -> p2::look_for_names q1 q2
    | [],[] -> []
    | _ -> failwith "pas possible"
  in
  let rec prendidents tdfl pl = match tdfl with
    | tdf::q -> prendidents q (look_for_names tdf.tpatargs pl)
    | [] -> pl
  in
  let rep = {tpatargs = replace_in_dfn 0 (prendidents (List.tl !tdeflist) d.tpatargs) posdufiltrage;tident = d.tident;texpr = e} in
  lastdefined := "";tdeflist := []; deflist := []; [TDdefn(rep)]
  end else (let first = List.hd !tdeflist in lastdefined := "";tdeflist := []; deflist := []; [TDdefn(first)]) end

and typclass env envtyps envinstances pos s slist tdlist =
  if Smap.mem s !envclasses
    then typingerror ("La classe "^s^" est définie 2 fois") pos;
  let (classenvfonctions : typ Smap.t ref) = ref Smap.empty in
  let rep = List.map (fun td -> let a,b,d,e = typtdecl env envtyps envinstances td in lastdefined := "";deflist := [];classenvfonctions := Smap.add td.dident a !classenvfonctions;envfonctions := Smap.add td.dident (a,b,Some s,d) !envfonctions;e)
      (List.map (fun {dident = dident; identlist = identlist; ntypelist = ntypelist;purtypelist = purtypelist;purtype = purtype} -> {dident = dident; identlist = slist; ntypelist = ntypelist;purtypelist = purtypelist;purtype = purtype;pos = pos}) tdlist) in
  envclasses := Smap.add s (slist,!classenvfonctions,[]) !envclasses;globalenvinstances := Smap.add s [] !globalenvinstances;rep

and replace_in_dfn i ptlist posdufiltrage = match ptlist with
  | x::q when i <> posdufiltrage -> x::replace_in_dfn (i+1) q posdufiltrage
  | _::q -> TPlident ("$matching$")::replace_in_dfn (i+1) q posdufiltrage
  | [] -> []



(*juste des fonctions pour dire que l'union de 2 Smap non disjointes est sans conséquence*)
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



and typinstance env envtyps envinstances i defpasreflist = match i with
  | Imularrow (nlist,n,pos) -> (let clident = n.nident in
    let l = List.length (smapfind clident envinstances pos) in
    let slist,classenvfonctions,flist = smapfind clident !envclasses pos in 
    let rec addinstance envinstances envtyps nlist = match nlist with
      | [] -> envinstances
      | n::q -> let envinstances = addinstance envinstances envtyps q in Smap.add n.nident ((List.map (fun n -> fst(typatype env envtyps envinstances n)) n.atypes,[])::(smapfind n.nident envinstances pos)) envinstances in
    let envtyps = ajouterntyplist envtyps nlist in
    let envtyps = ajouter envtyps n.atypes in
    let envinstances = addinstance envinstances envtyps (n::nlist) in
    let rec aux5 tlist1 tlist2 =
      if unifyable tlist1 tlist2
      then typingerror ("2 instances différentes de "^n.nident^" peuvent être unifiées") pos in
    List.iter (aux5 (List.map (fun a -> fst(typatype env envtyps envinstances a)) n.atypes)) (List.map fst (smapfind n.nident !globalenvinstances pos));
    let rec aux slist tlist = match slist,tlist with
      | [],[] -> Smap.empty
      | s::q1,t::q2 -> Smap.add s t (aux q1 q2)
      | _ -> typingerror "pas le bon nombre de types" pos in
    let substtable = aux slist (List.map (fun a -> fst(typatype env envtyps envinstances a)) n.atypes) in
    let subst t pos =match t with
      | Tgeneral s -> smapfind s substtable pos
      | _ -> t in
    let rep = List.map (fun (df:defn) -> (match fstr(smapfind df.ident !envfonctions pos) with 
      | Tarrow(tlist,t) -> typdfn env envtyps envinstances false df (List.map (fun t -> subst t df.pos) tlist) (subst t df.pos)
      | _ -> failwith "c'est pas possible")) defpasreflist in
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
      let l = aux4 s defpasreflist in
      (match l with
      | [] -> typingerror ("Il manque une définition de "^s) pos;
      | x::q -> let posdufiltrage = getchanging false 0 x.patargs in
      if posdufiltrage = -1 then ()
      else if not (checkexaustivelist env envtyps envinstances [subst (List.nth tlist posdufiltrage) pos] (List.map (fun x -> [x]) (List.map (aux posdufiltrage) l)))
      then (typingerror ("pas exhaustif dans l'appel de l'instance pour la classe "^clident) pos))
      | _ -> failwith "pas possible" in
    let rec aux6 nlist = match nlist with
      | [] -> []
      | n::q -> (n.nident,List.map (fun a -> fst(typatype env envtyps envinstances a))n.atypes)::aux6 q in
    let listdesoblig = aux6 nlist in
    Smap.iter aux3 classenvfonctions;globalenvinstances := Smap.add n.nident ((List.map (fun n -> fst(typatype env envtyps envinstances n)) n.atypes,listdesoblig)::(smapfind n.nident !globalenvinstances n.pos)) !globalenvinstances;
    let rec aux7 tv= match tv with
    | TDdefn(tdf) -> TDdefn{tident = "."^clident^"."^tdf.tident^"."^(string_of_int l);texpr = tdf.texpr;tpatargs = tdf.tpatargs}
     | _ -> failwith "pas possible" in
    let rec aux8 defl tdefl = match defl,tdefl with
      | [],[] -> checkforenddef env envtyps envinstances pos false
      | def::q1,tdef::q2 -> let a = if !lastdefined <> "" && !lastdefined <> tdef.tident then (let i = checkforenddef env envtyps envinstances pos in (lastdefined := tdef.tident); i) false else (lastdefined := tdef.tident;[]) in
        deflist := def::(!deflist); tdeflist := tdef::(!tdeflist);(lastdefined := tdef.tident);a @ aux8 q1 q2
      | _ -> failwith "pas possible" in
    let reponse = aux8 defpasreflist rep in deflist := [];tdeflist := [];
    List.map aux7 reponse)

  | Iarrow (n1,n2,pos) -> typinstance env envtyps envinstances (Imularrow([n1],n2,pos)) defpasreflist
  | Intype (n,pos) -> typinstance env envtyps envinstances (Imularrow([],n,pos)) defpasreflist
  
