open X86_64
open Purescript_allocation

let next_mult_16 a = a + (a mod 16)
let creer_compteur () =
        let i = ref (-1) in
        (fun () -> (incr i; !i))
let compteur_str_const = creer_compteur ()
let compteur_if = creer_compteur ()
let compteur_inf = creer_compteur ()
let compteur_inf_eq = creer_compteur ()
let compteur_eq = creer_compteur ()
let compteur_eq_string = creer_compteur ()



let data = ref (
        (label "_printf_log") ++ (string "%s\n") ++
        (label "_string_constante") ++ (string "blabla") ++
        (label "_show_string_int") ++ (string "%dA") ++
        (label "_true") ++ (string "true") ++
        (label "_false") ++ (string "false")
)
let add_data x =
        data := !data ++ x


let rec traduit_a_file file =
        List.fold_left (fun acc tdecl -> acc ++ traduit_a_tdecl tdecl) Purescript_code_predefini.code_initial file

and traduit_a_tdecl = function
        | A_defn x -> traduit_a_defn x

and traduit_a_defn defn =
        label defn.a_ident ++
        enter (imm (next_mult_16 (abs defn.tableau_activation))) ++

        traduit_a_expr defn.a_expr ++

        (match expr_typ defn.a_expr with
                | Unit | Tcustom("Effect",[Unit]) -> movq (imm 0) (reg rax) 
                | _ -> movq (ind ~ofs:(expr_adr defn.a_expr) rbp) (reg rax)
        ) ++


        leave ++
        ret

and traduit_a_expr = function
        | A_atom (atm, typ, addr) ->
                traduit_a_atom atm ++
                movq2idx (atom_adr atm) rbp addr rbp
        | A_lident (fct, params, typ, addr) ->
                (* calcul des params *)
                List.fold_left (fun acc atom -> acc ++ (
                        traduit_a_atom atom
                )) nop params ++

                (* placer les params au bon endroit *)
                (* si nb impair de params *)
                (if ((List.length params) mod 2) = 1 then pushq (imm 0) else nop) ++

                List.fold_left (fun acc atom -> (
                        pushq (ind ~ofs:(atom_adr atom) rbp)
                ) ++ acc) nop params ++

                (* appel recursif *)
                let fct_string = match fct with
                        | "show" -> begin
                                match atom_typ (List.hd params) with
                                        | Int -> "_show_int"
                                        | Boolean -> "_show_bool"
                                        | _ -> "_show_undeg"
                                end
                        | _ -> fct in
                call fct_string ++ 

                List.fold_left (fun acc atom -> acc ++ (   (* reflechir au bon sens *)
                        popq r8
                )) nop params ++
                (if ((List.length params) mod 2) = 1 then popq r8 else nop) ++


                movq (reg rax) (ind ~ofs:addr rbp)
        | A_do (lst, typ, addr) ->
                List.fold_left (fun acc expr -> acc ++ (
                        traduit_a_expr expr
                )) nop lst ++
                movq (imm 0) (ind ~ofs:addr rbp)
        | A_binop (bi, e1, e2, typ, addr) -> begin
                traduit_a_expr e1 ++
                traduit_a_expr e2 ++
                let e1_adr = expr_adr e1 in
                let e2_adr = expr_adr e2 in
                match bi with
                        | Bplus _ ->
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                addq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Bminus _ ->
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                subq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Btimes _ ->
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                imulq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Bdivide _ ->
                                failwith "la division est maintenant une fonction"
                        | Binf _ -> 
                                let label_num = string_of_int (compteur_inf ()) in
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                movq (ind ~ofs:e2_adr rbp) (reg r9) ++
                                cmpq (reg r9) (reg r8) ++
                                jl ("_binop_inf_" ^ label_num) ++
                                movq (imm 0) (ind ~ofs:addr rbp) ++
                                jmp ("_binop_inf_fin_" ^ label_num) ++
                                label ("_binop_inf_" ^ label_num) ++
                                movq (imm 1) (ind ~ofs:addr rbp) ++
                                label ("_binop_inf_fin_" ^ label_num)
                        | Binfeq _ -> 
                                let label_num = string_of_int (compteur_inf_eq ()) in
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                movq (ind ~ofs:e2_adr rbp) (reg r9) ++
                                cmpq (reg r9) (reg r8) ++
                                jle ("_binop_infeq_" ^ label_num) ++
                                movq (imm 0) (ind ~ofs:addr rbp) ++
                                jmp ("_binop_infeq_fin_" ^ label_num) ++
                                label ("_binop_infeq_" ^ label_num) ++
                                movq (imm 1) (ind ~ofs:addr rbp) ++
                                label ("_binop_infeq_fin_" ^ label_num)
                        | Bequals _ -> begin
                                match (expr_typ e1) with
                                | Int | Boolean ->
                                        let label_num = string_of_int (compteur_eq ()) in
                                        movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                        movq (ind ~ofs:e2_adr rbp) (reg r9) ++
                                        cmpq (reg r8) (reg r9) ++
                                        je ("_binop_eq_" ^ label_num) ++
                                        movq (imm 0) (ind ~ofs:addr rbp) ++
                                        jmp ("_binop_eq_fin_" ^ label_num) ++
                                        label ("_binop_eq_" ^ label_num) ++
                                        movq (imm 1) (ind ~ofs:addr rbp) ++
                                        label ("_binop_eq_fin_" ^ label_num)
 
                                | String ->
                                        let label_num = string_of_int (compteur_eq_string ()) in
                                        movq (ind ~ofs:e1_adr rbp) (reg rdi) ++
                                        movq (ind ~ofs:e2_adr rbp) (reg rsi) ++
                                        call "strcmp" ++
                                        testq (reg rax) (reg rax) ++
                                        jz ("_binop_eq_string_0_" ^ label_num) ++
                                        movq (imm 0) (ind ~ofs:addr rbp) ++
                                        jmp ("_binop_eq_string_fin_" ^ label_num) ++
                                        label ("_binop_eq_string_0_" ^ label_num) ++
                                        movq (imm 1) (ind ~ofs:addr rbp) ++
                                        label ("_binop_eq_string_fin_" ^ label_num)
                                | Unit ->
                                        movq (imm 0) (ind ~ofs:addr rbp)

                                | _ -> failwith "égalite de ce type non supportée par Petit Purscript"
                        end
 
                        | _ -> failwith "operation binaire pas encore suportee"
        end
        | A_let (bindings, expr, typ, addr) ->
                List.fold_left (fun acc binding -> (
                        traduit_a_expr binding.a_expr ++
                        movq2idx (expr_adr binding.a_expr) rbp binding.a_lident rbp
                ) ++ acc) nop bindings ++

                traduit_a_expr expr ++
                movq2idx (expr_adr expr) rbp addr rbp
        | A_if (e1, e2, e3, typ, addr) ->
                        let lab_num = string_of_int (compteur_if ()) in
                        traduit_a_expr e1 ++
                        cmpq (imm 0) (ind ~ofs:(expr_adr e1) rbp) ++
                        je ("_if_lab_false_" ^ lab_num) ++
                        traduit_a_expr e2 ++
                        movq2idx (expr_adr e2) rbp addr rbp ++
                        jmp ("_if_lab_end_" ^ lab_num) ++
                        label ("_if_lab_false_" ^ lab_num) ++
                        traduit_a_expr e3 ++
                        movq2idx (expr_adr e3) rbp addr rbp ++
                        label ("_if_lab_end_" ^ lab_num)
                        

                        
and traduit_a_atom = function
        | A_expr (expr, typ, addr) ->
                let addr_result = expr_adr expr in
                traduit_a_expr expr ++
                movq2idx addr_result rbp addr rbp
        | A_constant (const, typ, addr) ->
                let cstPtr = match const with
                        | A_int i -> imm i
                        | A_string s -> let label_name = "_str_const_"^(string_of_int (compteur_str_const ())) in
                                        add_data ((label label_name) ++ (string s)) ;
                                        (ilab label_name)
                        | A_bool false -> imm 0
                        | A_bool true -> imm 1
                in
                movq cstPtr (ind ~ofs:addr rbp)
        | A_lident (typ, addr) -> nop


let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let text = traduit_a_file arbre_alloc in
                   
    {text = text ; data = !data}
