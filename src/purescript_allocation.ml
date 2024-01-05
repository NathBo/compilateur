open X86_64
open Purescript_ast
open Purescript_typage
open Format

module Smap = Map.Make(String)
type local_env = ident Smap.t


(* definition du nouvel arbre *)
type a_file = a_tdecl list

and a_tdecl =
        | A_defn of a_defn
        (* TODO *)

and a_defn =
        {a_ident : ident; a_patargs : a_patargs list ; a_expr : a_expr ; tableau_activation : int}
and a_patargs =
        (* le int indique la position dans la pile *)
        | A_constant of a_constant * int
        | A_lident of ident * int
        (* TODO *)
and a_constant =
        | A_bool of bool
        | A_int of int
        | A_string of string
and a_expr = (* le dernier int stoque l'adresse du résultat *)
        | A_atom of a_atom * typ * int
        | A_lident of ident * a_atom list * typ * int
        | A_do of a_expr list * typ * int
        | A_binop of binop * a_expr * a_expr * typ * int
        | A_let of a_binding list * a_expr * typ * int
        | A_if of a_expr * a_expr * a_expr * typ * int
and a_atom =
        (* le dernier int stoque l'adresse *)
        | A_constant of a_constant * typ * int
        | A_lident of typ  * int
        | A_expr of a_expr * typ * int
and a_binding =
        {a_lident : int ; a_expr : a_expr}


let creer_8compteur () =
        let i = ref (0) in
        (fun () -> (incr i; -(!i*8)))

let creer_8compteur_plus () =
        let i = ref (1) in
        (fun () -> (incr i; !i*8))



let expr_adr : a_expr -> int = function
        | A_atom (_,_,x) -> x
        | A_lident (_, _, _, x) -> x
        | A_do (_,_,x) -> x
        | A_binop (_,_,_,_,x) -> x
        | A_let (_,_,_,x) -> x
        | A_if (_,_,_,_,x) -> x
let expr_typ : a_expr -> typ = function
        | A_atom (_,x,_) -> x
        | A_lident (_, _, x, _) -> x
        | A_do (_,x,_) -> x
        | A_binop (_,_,_,x,_) -> x
        | A_let (_,_,x,_) -> x
        | A_if (_,_,_,x,_) -> x
let atom_adr : a_atom -> int = function
        | A_constant (_,_,x) -> x
        | A_expr (_,_,x) -> x
        | A_lident (_,x) -> x
let atom_typ : a_atom -> typ = function
        | A_constant (_,x,_) -> x
        | A_expr (_,x,_) -> x
        | A_lident (x,_) -> x



let rec typage_to_alloc t = (List.filter_map traduit_tvdecl t) 

and traduit_tvdecl = function
        | TDdefn x -> Some (A_defn (traduit_tdefn x))
        | TDtdecl x -> None
        | _ -> failwith "pas encore def 1"

and traduit_tdefn (x:tdefn) : a_defn =

        let arg_compteur = creer_8compteur_plus () in
        let env = ref Smap.empty in

        let a_patargs = (List.map (fun patarg ->
                let r = traduit_tpatarg arg_compteur patarg in
                (match r with
                        | A_lident (nom, addr) -> env := Smap.add nom addr !env
                        | _ -> ()
                ); r
        ) x.tpatargs) in
        
        let cmpt = creer_8compteur () in
        let a_expr = traduit_texpr cmpt !env x.texpr in
        let taille = abs (cmpt ()) in
        {a_ident = x.tident; a_patargs = a_patargs ; a_expr = a_expr ; tableau_activation = taille}

and traduit_tpatarg compteur = function
        | TPconstant x -> A_constant (traduit_tconstant x, compteur () )
        | TPlident x -> A_lident (x, compteur ())
        | _ -> failwith "pas encore def 2"

and traduit_texpr compteur env = function
        | TEatom (x,y) ->
                        let atm = traduit_atom compteur env x in
                        A_atom (atm,y, compteur ())
        | TElident (x,y,z) -> 
                        let calculs_inter = List.map (traduit_atom compteur env) y in
                        A_lident (x, calculs_inter, z, compteur () )
        | TEdo (lst, typ) ->
                        let lst_result = List.map (traduit_texpr compteur env) lst in
                        A_do ( lst_result , typ, compteur () )
        | TEbinop (bi, e1, e2, typ) -> begin
                let a_e1 = traduit_texpr compteur env e1 in
                let a_e2 = traduit_texpr compteur env e2 in
                match bi with
                | Bdivide _ -> 
                        A_lident ("_divide", [A_expr (a_e1, expr_typ a_e1, expr_adr a_e1) ; A_expr (a_e2, expr_typ a_e2, expr_adr a_e2)], typ, compteur () )
                | Bcons _ -> 
                        A_lident ("_concat", [A_expr (a_e1, expr_typ a_e1, expr_adr a_e1) ; A_expr (a_e2, expr_typ a_e2, expr_adr a_e2)], typ, compteur () )
 
                | Bsup t -> A_binop (Binf t, a_e2, a_e1, typ, compteur ())
                | Bsupeq t -> A_binop (Binfeq t, a_e2, a_e1, typ, compteur ())
                | Bnotequals t -> 
                                let equal = A_binop (Bequals t, a_e1, a_e2, typ, compteur()) in
                                A_lident ("not", [A_expr (equal, expr_typ equal, expr_adr equal)], typ, compteur ())

                | _ -> A_binop (bi, a_e1, a_e2, typ, compteur () ) 
        end
        | TElet (lst, expr, typ) ->
                        (* calcul des positions sur la pile *)
                        Printf.printf "taille binding : %d\n" (List.length lst) ;
                        let env = List.fold_left (fun prev_env binding -> Smap.add binding.tident
                                (compteur ())
                        prev_env ) env lst in

                        (* calcul des valeurs *)
                        let a_binding = List.fold_left (fun l binding -> (
                                let expr = traduit_texpr compteur env binding.tbindexpr in
                                let adr_result = Smap.find binding.tident env in
                                {a_lident = adr_result ; a_expr = expr }
                        )::l) [] lst in

                        
                        let a_expr = traduit_texpr compteur env expr in
                        A_let (a_binding, a_expr, typ, compteur())
        | TEif (e1, e2, e3, typ)        ->
                        let a_e1 = traduit_texpr compteur env e1 in
                        let a_e2 = traduit_texpr compteur env e2 in
                        let a_e3 = traduit_texpr compteur env e3 in
                        A_if (a_e1, a_e2, a_e3, typ, compteur ())
                
        | _ -> failwith "pas encore def 3"

and traduit_tconstant = function
        | TCbool x -> A_bool x 
        | TCstring x -> A_string x
        | TCint x -> A_int x

and traduit_atom compteur env = function
        | TAconstant (x,y) -> A_constant (traduit_tconstant x, y, compteur ())
        | TAexpr (x,y) -> let expr = traduit_texpr compteur env x in A_expr (expr, y, expr_adr expr)
        | TAlident (ident, typ) -> begin
                match ident with
                | "unit" -> A_constant ( A_bool (true), typ, compteur () )
                | _ ->
                        let adr = Smap.find ident env in
                        A_lident (typ, adr)
        end
        | _ -> failwith "pas encore def 5"

(* print *)
let rec print_a_file fmt f =
        List.iter (fun x -> print_a_tdecl fmt x; fprintf fmt "\n") f
and print_a_tdecl fmt = function
        | A_defn x -> fprintf fmt "def : " ; print_a_defn fmt x
and print_a_defn fmt x =
        fprintf fmt "%s (taille %d) retourne : " x.a_ident x.tableau_activation ; print_typ (expr_typ x.a_expr) ; fprintf fmt " ("; List.iter (fun arg -> print_a_patarg fmt arg ; fprintf fmt ", ") x.a_patargs ; print_a_expr fmt x.a_expr

and print_a_patarg fmt = function
        | A_lident (nom,adr) -> fprintf fmt "arg (%s | %d)" nom adr
        | A_constant (cst,adr) -> fprintf fmt "arg (const | %d)" adr
and print_a_expr fmt = function
        | A_atom (atm, typ, addr) -> fprintf fmt "atom"
        | A_lident (fct, param, typ, pos) -> fprintf fmt "appel de %s et range en %d : do {" fct pos ; List.iter (print_a_atom fmt) param; fprintf fmt "}"
        | A_do (lst, typ, adr) -> fprintf fmt "do (stoque en %d) :  \n" adr ; List.iter (fun x -> (print_a_expr fmt x; fprintf fmt "\n")) lst
        | A_binop (_,_,_,_,_) -> fprintf fmt "binop"
        | A_let (_,expr,_,_) -> fprintf fmt "let... in " ; print_a_expr fmt expr
        | A_if (e1,e2,e3,typ,addr) -> fprintf fmt "if (%a) then (%a) else (%a)" print_a_expr e1 print_a_expr e2 print_a_expr e3
and print_a_atom fmt = function
        | A_expr (expr, typ, addr) -> fprintf fmt "calcul (stoque en %d) de " addr; print_a_expr fmt expr
        | A_constant (conct,typ, addr) -> fprintf fmt "const stoquee en %d " addr
        | A_lident (typ, adr) -> fprintf fmt "lident stoque en %d" adr






