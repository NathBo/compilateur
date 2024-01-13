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

and a_defn =
        {a_ident : ident; a_patargs : a_patarg list ; a_expr : a_expr ; tableau_activation : int}
and a_patarg =
        (* le int indique la position dans la pile *)
        | A_constant of a_constant * int
        | A_lident of ident * int
        | A_uident of int * int
        | A_pattern of a_pattern * int
and a_constant =
        | A_bool of bool * int
        | A_int of int * int
        | A_string of string * int
and a_expr = (* le dernier int stoque l'adresse du résultat *)
        | A_atom of a_atom * typ * int
        | A_lident of ident * a_atom list * typ * int
        | A_uident of Purescript_data_info.data_info * a_atom list * typ * int
        | A_do of a_expr list * typ * int
        | A_binop of binop * a_expr * a_expr * typ * int
        | A_let of a_binding list * a_expr * typ * int
        | A_if of a_expr * a_expr * a_expr * typ * int
        | A_case of a_expr * a_branch list * typ * int
        | A_nop
and a_atom =
        (* le dernier int stoque l'adresse *)
        | A_constant of a_constant * typ * int
        | A_lident of typ  * int
        | A_uident of int * typ  * int (* class with no argumements *)
        | A_expr of a_expr * typ * int
and a_binding =
        {a_adr : int ; a_expr : a_expr}
and a_branch =
        {a_pattern : a_pattern ; expr : a_expr}
and a_pattern =
        | A_patarg of a_patarg
        | A_mulpatarg of int * a_patarg list

(* utilitaires *)
let expr_adr : a_expr -> int = function
        | A_atom (_,_,x) -> x
        | A_lident (_, _, _, x) -> x
        | A_uident (_, _, _, x) -> x
        | A_do (_,_,x) -> x
        | A_binop (_,_,_,_,x) -> x
        | A_let (_,_,_,x) -> x
        | A_if (_,_,_,_,x) -> x
        | A_case (_,_,_,x) -> x
        | A_nop -> 0
let expr_typ : a_expr -> typ = function
        | A_atom (_,x,_) -> x
        | A_lident (_, _, x, _) -> x
        | A_uident (_, _, x, _) -> x
        | A_do (_,x,_) -> x
        | A_binop (_,_,_,x,_) -> x
        | A_let (_,_,x,_) -> x
        | A_if (_,_,_,x,_) -> x
        | A_case (_,_,x,_) -> x
        | A_nop -> Unit
let atom_adr : a_atom -> int = function
        | A_constant (_,_,x) -> x
        | A_expr (_,_,x) -> x
        | A_lident (_,x) -> x
        | A_uident (_,_,x) -> x
let atom_typ : a_atom -> typ = function
        | A_constant (_,x,_) -> x
        | A_expr (_,x,_) -> x
        | A_lident (x,_) -> x
        | A_uident (_,x,_) -> x
let const_adr : a_constant -> int = function
        | A_bool (_,x) -> x
        | A_int (_,x) -> x
        | A_string (_,x) -> x


(* traduction de l'arbre de typage vers l'arbre d'allocation *)
let rec typage_to_alloc t = 
        let class_dico = Purescript_data_info.build_class_dico t in
        Purescript_data_info.print_class_dico Format.std_formatter class_dico ;
        (List.filter_map (traduit_tvdecl class_dico) t) 

and traduit_tvdecl class_dico = function
        | TDdefn x -> Some (A_defn (traduit_tdefn class_dico x))
        | TDtdecl _ -> None
        | TDdata (_,_,_) -> None
        | _ -> failwith "pas encore def 1"

and traduit_tdefn dico x =
        let arg_compteur = Compteur.make 8 16 in
        let env = ref Smap.empty in
        
        let a_patargs = (List.map (fun patarg ->
                let r,e = traduit_tpatarg dico arg_compteur patarg in
                env := Smap.union (fun _ -> failwith "2 ident identiques") !env e;
                r
        ) x.tpatargs) in
        
        let cmpt = Compteur.make (-8) (-8) in
        let a_expr = traduit_texpr dico cmpt !env x.texpr in
        let taille = abs (Compteur.size cmpt) in
        {a_ident = x.tident; a_patargs = a_patargs ; a_expr = a_expr ; tableau_activation = taille}

and traduit_tpatarg dico compteur = function (* retourne une paire avec la traduction et le nouvel environnement *)
        | TPconstant x -> A_constant (traduit_tconstant dico compteur x, Compteur.get compteur ), Smap.empty
        | TPlident x -> let addr = Compteur.get compteur in (A_lident (x, addr ), Smap.singleton x addr)
        | TPuident x -> A_uident ((Smap.find x dico).hash, Compteur.get compteur), Smap.empty
        | TPpattern pattern -> 
                        let a_pattern, env = traduit_tpattern dico compteur pattern in
                        A_pattern (a_pattern, Compteur.get compteur) , env

and traduit_texpr dico compteur env = function
        | TEatom (x,y) ->
                        let atm = traduit_atom dico compteur env x in
                        A_atom (atm,y, atom_adr atm)
        | TElident (x,y,z) -> 
                        let calculs_inter = List.map (traduit_atom dico compteur env) y in
                        A_lident (x, calculs_inter, z, Compteur.get compteur )
        | TEuident (uident,lst,typ) ->
                        let calculs_inter = List.map (traduit_atom dico compteur env) lst in
                        A_uident (Smap.find uident dico , calculs_inter, typ, Compteur.get compteur )
        | TEdo (lst, typ) ->
                        let last_expr = ref A_nop in
                        let lst_result = List.map (fun expr ->
                                let cmpt_tmp = Compteur.copy compteur in
                                let e = traduit_texpr dico cmpt_tmp env expr in
                                last_expr := e ;
                                e
                        ) lst in
                        Compteur.union compteur;
                        A_do ( lst_result , typ, expr_adr !last_expr )
        | TEbinop (bi, e1, e2, typ) -> begin
                let a_e1 = traduit_texpr dico compteur env e1 in
                let a_e2 = traduit_texpr dico compteur env e2 in
                match bi with
                | Bdivide _ -> 
                        A_lident ("_divide", [A_expr (a_e1, expr_typ a_e1, expr_adr a_e1) ; A_expr (a_e2, expr_typ a_e2, expr_adr a_e2)], typ, Compteur.get compteur )
                | Bcons _ -> 
                        A_lident ("_concat", [A_expr (a_e1, expr_typ a_e1, expr_adr a_e1) ; A_expr (a_e2, expr_typ a_e2, expr_adr a_e2)], typ, Compteur.get compteur)
 
                | Bsup t -> A_binop (Binf t, a_e2, a_e1, typ, Compteur.get compteur)
                | Bsupeq t -> A_binop (Binfeq t, a_e2, a_e1, typ, Compteur.get compteur)
                | Bnotequals t -> 
                                let equal = A_binop (Bequals t, a_e1, a_e2, typ, Compteur.get compteur) in
                                A_lident ("not", [A_expr (equal, expr_typ equal, expr_adr equal)], typ, Compteur.get compteur)
                | _ -> A_binop (bi, a_e1, a_e2, typ, Compteur.get compteur ) 
        end
        | TElet (lst, expr, typ) ->
                        (* calcul des positions sur la pile *)
                        Printf.printf "taille binding : %d\n" (List.length lst) ;
                        let env = List.fold_left (fun prev_env binding -> Smap.add binding.tident
                                (Compteur.get compteur)
                        prev_env ) env lst in

                        (* calcul des valeurs *)
                        let a_binding = List.fold_left (fun l binding -> (
                                let compteur_tmp = Compteur.copy compteur in

                                let expr = traduit_texpr dico compteur_tmp env binding.tbindexpr in
                                let adr_result = Smap.find binding.tident env in
                                {a_adr = adr_result ; a_expr = expr }
                        )::l) [] lst in
                        Compteur.union compteur ;

                        
                        let a_expr = traduit_texpr dico compteur env expr in
                        A_let (a_binding, a_expr, typ, Compteur.get compteur)
        | TEif (e1, e2, e3, typ)        ->
                        let a_e1 = traduit_texpr dico compteur env e1 in
                        let c2 = Compteur.copy compteur in
                        let c3 = Compteur.copy compteur in
                        let a_e2 = traduit_texpr dico c2 env e2 in
                        let a_e3 = traduit_texpr dico c3 env e3 in
                        Compteur.union compteur ;
                        A_if (a_e1, a_e2, a_e3, typ, expr_adr a_e1)
        | TEcase (expr, branchs , typ) ->
                let e = traduit_texpr dico compteur env expr in
                let b = List.map (traduit_tbranch dico compteur env) branchs in
                A_case (e, b, typ, Compteur.get compteur)
                

and traduit_tconstant dico compteur = function
        | TCbool x -> A_bool (x, Compteur.get compteur) 
        | TCstring x -> A_string (x, Compteur.get compteur)
        | TCint x -> A_int (x, Compteur.get compteur)

and traduit_atom dico compteur env = function
        | TAconstant (x,y) -> let cst = traduit_tconstant dico compteur x in A_constant (cst, y, const_adr cst)
        | TAexpr (x,y) -> let expr = traduit_texpr dico compteur env x in A_expr (expr, y, expr_adr expr)
        | TAlident (ident, typ) -> begin
                match ident with
                | "unit" -> A_constant ( A_bool (true, Compteur.get compteur), typ, Compteur.get compteur)
                | _ ->
                        let adr = Smap.find ident env in
                        A_lident (typ, adr)
        end
        | TAuident (ident, typ) ->
                        let adr = Compteur.get compteur in 
                        let data_info = Smap.find ident dico in
                        if data_info.size <> 0 then failwith "il y a des parametres";
                        A_uident (data_info.hash , typ, adr)
        | _ -> failwith "pas encore def 5"

and traduit_tpattern dico compteur = function (* retourne une paire avec le a_patern et l'environnement a ajouter *)
        | TPpatarg patarg -> let a_patarg, env = traduit_tpatarg dico compteur patarg in (A_patarg a_patarg,env)
        | TPmulpatarg (ident, tpatargs) ->
                        let hash = (Smap.find ident dico).hash in
                        let env = ref Smap.empty in
                        let lst_patarg = ref [] in
                        List.iter (fun x ->
                                let p,e = traduit_tpatarg dico compteur x in
                                env := Smap.union (fun ident a1 a2 -> if ident = "_" then Some a1 else failwith "union fail") !env e;
                                lst_patarg := p :: !lst_patarg
                        ) tpatargs ;
                        lst_patarg := List.rev !lst_patarg ;
                        A_mulpatarg (hash, !lst_patarg), !env

and traduit_tbranch dico compteur env tbranch =
        let a_pattern, env_add = traduit_tpattern dico compteur tbranch.tpattern in
        let nouv_env = Smap.union (fun ident a1 a2 -> if ident = "_" then Some a1 else failwith "union fail") env env_add in
        {a_pattern = a_pattern ; expr = traduit_texpr dico compteur nouv_env tbranch.texpr}


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
        | A_uident (hash,adr) -> fprintf fmt "uident (%d,%d)" hash adr
        | A_pattern (a_pattern,adr) -> fprintf fmt "pat %a" print_a_pattern a_pattern
and print_a_expr fmt = function
        | A_atom (atm, typ, addr) -> fprintf fmt "atom : %a " print_a_atom atm
        | A_lident (fct, param, typ, pos) -> fprintf fmt "appel de %s et range en %d : do {" fct pos ; List.iter (print_a_atom fmt) param; fprintf fmt "}"
        | A_uident (info, param, typ, pos) -> fprintf fmt "appel de ... et range en %d : do {" pos ; List.iter (print_a_atom fmt) param; fprintf fmt "}"
        | A_do (lst, typ, adr) -> fprintf fmt "do (stoque en %d) :  \n" adr ; List.iter (fun x -> (print_a_expr fmt x; fprintf fmt "\n")) lst
        | A_binop (_,_,_,_,_) -> fprintf fmt "binop"
        | A_let (_,expr,_,_) -> fprintf fmt "let... in " ; print_a_expr fmt expr
        | A_if (e1,e2,e3,typ,addr) -> fprintf fmt "if (%a) then (%a) else (%a)" print_a_expr e1 print_a_expr e2 print_a_expr e3
        | A_case (expr, branchs, typ, adr) -> fprintf fmt "case {%a} et range en %d : \n" print_a_expr expr adr; List.iter (print_a_branch fmt) branchs
        | A_nop -> fprintf fmt "nop"
and print_a_atom fmt = function
        | A_expr (expr, typ, addr) -> fprintf fmt "calcul (stoque en %d) de " addr; print_a_expr fmt expr
        | A_constant (conct,typ, addr) -> fprintf fmt "const stoquee en %d " addr
        | A_lident (typ, adr) -> fprintf fmt "lident stoque en %d" adr
        | A_uident _ -> fprintf fmt "uident"
and print_a_branch fmt branch = fprintf fmt "  | %a -> %a \n" print_a_pattern branch.a_pattern print_a_expr branch.expr
and print_a_pattern fmt = function
        | A_patarg (a_patarg) -> fprintf fmt "patarg{%a}" print_a_patarg a_patarg
        | A_mulpatarg (hash, a_patargs) -> fprintf fmt "multpatarg:%d{ " hash ; List.iter (print_a_patarg fmt) a_patargs







