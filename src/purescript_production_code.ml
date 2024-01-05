open X86_64
open Purescript_allocation

let next_mult_16 a = a + (a mod 16)
let creer_compteur () =
        let i = ref (-1) in
        (fun () -> (incr i; !i))
let compteur_str_const = creer_compteur ()



let code_initial =
        globl "main" ++

        label "log" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        movq (ilab "_printf_log") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        leave ++
        ret ++
        
        label "_show_int" ++
        enter (imm 0) ++
        movq (imm 100) (reg rdi) ++
        call "malloc" ++
        movq (reg rax) (reg rdi) ++
        movq (ilab "_show_string_int") (reg rsi) ++
        xorq (reg rax) (reg rax) ++
        movq (ind ~ofs:16 rbp) (reg rdx) ++
        call "sprintf" ++
        movq (reg rax) (reg r8) ++
        movq (reg rdi) (reg rax) ++
        subq (reg r8) (reg rax) ++
        addq (reg rax) (reg r8) ++  (* to remove the last character *)
        movb (imm 0) (ind ~ofs:(-1) r8) ++
        leave ++
        ret ++

        label "_show_bool" ++
        enter (imm 0) ++
        cmpq (imm 0) (ind ~ofs:16 rbp) ++
        je "_show_bool_false" ++
        movq (ilab "_true") (reg rax) ++
        leave ++
        ret ++
        label "_show_bool_false" ++
        movq (ilab "_false") (reg rax) ++
        leave ++
        ret ++
        
        label "_divide" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        testq (reg rax) (reg rax) ++
        js "_divide_neg" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm 0) (reg rdx) ++
        idivq (reg rcx) ++
        leave ++
        ret ++
        label "_divide_neg" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm (-1)) (reg rdx) ++
        idivq (reg rcx) ++
        testq (reg rdx) (reg rdx) ++
        jnz "_divide_mod_1" ++
        leave ++
        ret ++
        label "_divide_mod_1" ++
        cmpq (imm 0) (ind ~ofs:24 rbp) ++
        jg "_divide_neg_decr" ++
        incq (reg rax) ++
        leave ++
        ret ++
        label "_divide_neg_decr" ++
        decq (reg rax) ++
        leave ++
        ret ++




        label "mod" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        testq (reg rax) (reg rax) ++
        js "_mod_neg" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm 0) (reg rdx) ++
        idivq (reg rcx) ++
        movq (reg rdx) (reg rax) ++
        leave ++
        ret ++
        label "_mod_neg" ++
        movq (ind ~ofs:24 rbp) (reg rcx) ++
        movq (imm (-1)) (reg rdx) ++
        idivq (reg rcx) ++
        testq (reg rdx) (reg rdx) ++
        movq (reg rdx) (reg rax) ++
        testq (reg rax) (reg rax) ++
        jz "_mod_ret_0" ++
        cmpq (imm 0) (ind ~ofs:24 rbp) ++
        js "_mod_neg_neg" ++
        addq (ind ~ofs:24 rbp) (reg rax) ++
        leave ++
        ret ++
        label "_mod_neg_neg" ++
        subq (ind ~ofs:24 rbp) (reg rax) ++
        leave ++
        ret ++
        label "_mod_ret_0" ++
        movq (imm 0) (reg rax) ++
        leave ++
        ret





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
        List.fold_left (fun acc tdecl -> acc ++ traduit_a_tdecl tdecl) code_initial file

and traduit_a_tdecl = function
        | A_defn x -> traduit_a_defn x

and traduit_a_defn defn =
        label defn.a_ident ++
        enter (imm (next_mult_16 (abs defn.tableau_activation))) ++

        traduit_a_expr defn.a_expr ++

        movq (imm 0) (reg rax) ++

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
                                 
                        | _ -> failwith "operation binaire pas encore suportee"
        end
        | A_let (bindings, expr, typ, addr) ->
                (* TODO : calc bindings *)
                traduit_a_expr expr ++
                movq2idx (expr_adr expr) rbp addr rbp




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


let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let text = traduit_a_file arbre_alloc in
                   
    {text = text ; data = !data}
