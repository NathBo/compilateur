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
let compteur_branch = creer_compteur ()
let compteur_lazy = creer_compteur ()
let compteur_match = creer_compteur ()



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
        | A_lident (fct, params, typ, addr) -> begin
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
                                        | _ -> "_show_undef"
                                end
                        | _ -> fct in
                call fct_string ++ 

                List.fold_left (fun acc atom -> acc ++ (
                        popq r8
                )) nop params ++
                (if ((List.length params) mod 2) = 1 then popq r8 else nop) ++

                movq (reg rax) (ind ~ofs:addr rbp)
        end
        | A_do (lst, typ, addr) ->
                List.fold_left (fun acc expr -> acc ++ (
                        traduit_a_expr expr
                )) nop lst ++
                movq (imm 0) (ind ~ofs:addr rbp)
        | A_binop (bi, e1, e2, typ, addr) -> begin
                traduit_a_expr e1 ++ (* on ne fait que e1 pour ne pas calculer e2 si pas besoin *)
                let e1_adr = expr_adr e1 in
                let e2_adr = expr_adr e2 in
                match bi with
                        | Bplus _ ->
                                traduit_a_expr e2 ++
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                addq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Bminus _ ->
                                traduit_a_expr e2 ++
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                subq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Btimes _ ->
                                traduit_a_expr e2 ++
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                imulq (ind ~ofs:e2_adr rbp) (reg r8) ++
                                movq (reg r8) (ind ~ofs:addr rbp)
                        | Bdivide _ | Bnotequals _ | Bsup _ | Bsupeq _ | Bcons _ ->
                                failwith "'/', '!=', '>', '>=', '<>' sont transformés lors de l'allocation"
                        | Binf _ -> 
                                traduit_a_expr e2 ++
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
                                traduit_a_expr e2 ++
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
                                traduit_a_expr e2 ++
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
                                        movq (imm 1) (ind ~ofs:addr rbp)

                                | _ -> failwith "égalite de ce type non supportée par Petit Purscript"
                        end
                        | Bor _ ->
                                let label_true = "_or_lazy_true_" ^ (string_of_int (compteur_lazy ())) in
                                let label_fin = "_or_lazy_" ^ (string_of_int (compteur_lazy ())) in
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                testq (reg r8) (reg r8) ++
                                jne label_true ++
                                movq2idx e2_adr rbp addr rbp ++
                                jmp label_fin ++
                                label label_true ++
                                movq (imm 1) (ind ~ofs:addr rbp) ++
                                label label_fin

                        | Band _ ->
                                let label_false = "_and_lazy_false_" ^ (string_of_int (compteur_lazy ())) in
                                let label_fin = "_and_lazy_" ^ (string_of_int (compteur_lazy ())) in
                                movq (ind ~ofs:e1_adr rbp) (reg r8) ++
                                testq (reg r8) (reg r8) ++
                                je label_false ++
                                traduit_a_expr e2 ++
                                movq2idx e2_adr rbp addr rbp ++
                                jmp label_fin ++
                                label label_false ++
                                movq (imm 0) (ind ~ofs:addr rbp) ++
                                label label_fin

 
        end
        | A_let (bindings, expr, typ, addr) ->
                List.fold_left (fun acc binding -> (
                        traduit_a_expr binding.a_expr ++
                        movq2idx (expr_adr binding.a_expr) rbp binding.a_adr rbp
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
        | A_case (expr, possibilites, typ, addr) -> begin
                        traduit_a_expr expr ++

                        let label_fin = "_branch_fin_" ^ (string_of_int (compteur_branch ())) in
                        let rec test_branch = function
                                | [] -> 
                                        movq (imm 0) (ind ~ofs:addr rbp)
                                | branch :: lst ->
                                        traduit_a_pattern (expr_adr expr) addr branch.expr label_fin branch.a_pattern ++
                                        test_branch lst
                        in test_branch possibilites ++
                        label label_fin
        end
        | A_uident (info, atoms, typ, addr) -> 
                        movq (imm (8*(info.size+1))) (reg rdi) ++
                        call "malloc" ++
                        movq (reg rax) (ind ~ofs:addr rbp) ++
                        movq (imm info.hash) (ind rax) ++
                        fst (List.fold_left (fun (acc,id) atom -> (acc ++ (
                                traduit_a_atom atom ++
                                movq (ind ~ofs:addr rbp) (reg r9) ++
                                movq2idx (atom_adr atom) rbp (8*id) r9
                        )),(id+1)) (nop,1) atoms)
        | A_nop -> nop





and traduit_a_pattern adr_expr_test adr_result expr_if_ok label_fin = function
        | A_patarg (A_uident (num, addr_compare)) ->
                let label_suite = "_branch_" ^ (string_of_int (compteur_branch ())) in
                movq (ind ~ofs:adr_expr_test rbp) (reg r8) ++
                cmpq (imm num) (ind r8)++
                jne label_suite ++
                traduit_a_expr expr_if_ok ++
                movq2idx (expr_adr expr_if_ok) rbp adr_result rbp ++
                jmp label_fin ++
                label label_suite
        | A_patarg (A_constant (const, addr_compare)) ->
                let label_suite = "_branch_" ^ (string_of_int (compteur_branch ())) in
                begin match const with
                | A_bool _ | A_int _ ->
                        traduit_a_const const ++
                        movq (ind ~ofs:adr_expr_test rbp) (reg r8) ++
                        movq (ind ~ofs:(const_adr const) rbp) (reg r9) ++
                        cmpq (reg r9) (reg r8)++
                        jne label_suite ++
                        traduit_a_expr expr_if_ok ++
                        movq2idx (expr_adr expr_if_ok) rbp adr_result rbp ++
                        jmp label_fin ++
                        label label_suite
                | A_string _ ->
                        traduit_a_const const ++
                        movq (ind ~ofs:adr_expr_test rbp) (reg rdi) ++
                        movq (ind ~ofs:(const_adr const) rbp) (reg rsi) ++
                        call "strcmp" ++
                        testq (reg rax) (reg rax) ++
                        jnz label_suite ++
                        traduit_a_expr expr_if_ok ++
                        movq2idx (expr_adr expr_if_ok) rbp adr_result rbp ++
                        jmp label_fin ++
                        label label_suite
                end



        | A_patarg (A_lident (_,adr)) -> 
                movq2idx (adr_expr_test) rbp adr rbp ++
                traduit_a_expr expr_if_ok ++
                movq2idx (expr_adr expr_if_ok) rbp adr_result rbp ++
                jmp label_fin
        | A_patarg (A_pattern (pattern,adr)) ->
                traduit_a_pattern adr_expr_test adr_result expr_if_ok label_fin pattern

        | A_mulpatarg (hash, patargs) ->
                let label_suite = "_branch_" ^ (string_of_int (compteur_branch ())) in
                movq (ind ~ofs:adr_expr_test rbp) (reg r8) ++
                cmpq (imm hash) (ind r8)++
                jne label_suite ++
                
                (
                let e = ref nop in
                List.iteri (fun (id:int) (patarg:a_patarg) : unit -> match patarg with
                        | A_lident (_,adr) -> e:= !e ++
                                        movq (ind ~ofs:adr_expr_test rbp) (reg r9) ++
                                        movq2idx (8*id+8) r9 adr rbp
                        | A_constant (const, adr) -> e := !e ++
                                traduit_a_const const ++
                                movq (ind ~ofs:adr_expr_test rbp) (reg r9) ++
                                movq (ind ~ofs:(const_adr const) rbp) (reg r8) ++
                                cmpq (reg r8) (ind ~ofs:(8*id+8) r9) ++
                                jne label_suite 
                        | A_pattern (pattern, adr) -> e := !e ++
                                let label_match = "_match_" ^ (string_of_int (compteur_match ())) in
                                movq (ind ~ofs:adr_expr_test rbp) (reg r9) ++
                                movq2idx (8*id+8) r9 adr rbp ++
                                traduit_a_pattern adr adr_result A_nop label_match pattern ++
                                jmp label_suite ++
                                label label_match
                        | A_uident (hash, adr) -> e := !e ++
                                movq (ind ~ofs:adr_expr_test rbp) (reg r9) ++
                                movq (ind ~ofs:(8*id+8) r9) (reg r8) ++
                                cmpq (imm hash) (ind r8)++
                                jne label_suite 

                ) patargs; !e) ++

                traduit_a_expr expr_if_ok ++
                movq2idx (expr_adr expr_if_ok) rbp adr_result rbp ++
                jmp label_fin ++
                label label_suite

and traduit_a_const = function
        | A_int (i,adr) -> movq (imm i) (ind ~ofs:adr rbp)
        | A_string (s,adr) -> let label_name = "_str_const_"^(string_of_int (compteur_str_const ())) in
                        add_data ((label label_name) ++ (string s)) ;
                        movq (ilab label_name) (ind ~ofs:adr rbp)
        | A_bool (false,adr) -> movq (imm 0) (ind ~ofs:adr rbp)
        | A_bool (true,adr) -> movq (imm 1) (ind ~ofs:adr rbp)

                        
and traduit_a_atom = function
        | A_expr (expr, typ, addr) ->
                let addr_result = expr_adr expr in
                traduit_a_expr expr ++
                movq2idx addr_result rbp addr rbp
        | A_constant (const, typ, addr) ->
                traduit_a_const const ++
                movq2idx (const_adr const) rbp addr rbp
        | A_lident (typ, addr) -> nop
        | A_uident (id, typ, addr) ->
                        movq (imm 8) (reg rdi) ++
                        call "malloc" ++
                        movq (imm id) (ind rax) ++
                        movq (reg rax) (ind ~ofs:addr rbp)



let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let text = traduit_a_file arbre_alloc in
                   
    {text = text ; data = !data}
