open X86_64
open Purescript_allocation

let next_mult_16 a = a + (a mod 16)

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
        
        label "show" ++
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



(*

        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg r8) ++
        cmpq (imm 0) (reg r8) ++
        je "_show_0" ++
        cmpq (imm 1) (reg r8) ++
        je "_show_1" ++
        cmpq (imm 2) (reg r8) ++
        je "_show_2" ++
        cmpq (imm 3) (reg r8) ++
        je "_show_3" ++
        cmpq (imm 4) (reg r8) ++
        je "_show_4" ++
        movq (ilab "_string_show_5") (reg rax) ++
        leave ++
        ret ++ *)

        label "_show_0" ++ movq (ilab "_string_show_0") (reg rax) ++ leave ++ ret ++
        label "_show_1" ++ movq (ilab "_string_show_1") (reg rax) ++ leave ++ ret ++
        label "_show_2" ++ movq (ilab "_string_show_2") (reg rax) ++ leave ++ ret ++
        label "_show_3" ++ movq (ilab "_string_show_3") (reg rax) ++ leave ++ ret ++
        label "_show_4" ++ movq (ilab "_string_show_4") (reg rax) ++ leave ++ ret


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
        | A_lident (fct, params, typ, addr) ->
                (* calcul des params *)
                List.fold_left (fun acc atom -> acc ++ (
                        traduit_a_atom atom
                )) nop params ++

                (* placer les params au bon endroit *)
                (* si nb impair de params *)
                (if ((List.length params) mod 2) = 1 then pushq (imm 0) else nop) ++

                List.fold_left (fun acc atom -> acc ++ (   (* reflechir au bon sens *)
                        pushq (ind ~ofs:(atom_adr atom) rbp)
                )) nop params ++

                (* appel recursif *)
                call fct ++ 

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


and traduit_a_atom = function
        | A_expr (expr, typ, addr) ->
                let addr_result = expr_adr expr in
                traduit_a_expr expr ++
                movq2idx addr_result rbp addr rbp
        | A_constant (const, typ, addr) ->
                let cstPtr = match const with
                        | A_int i -> imm i
                        | A_string s -> (ilab "_string_constante")
                        | A_bool false -> imm 0
                        | A_bool true -> imm 1
                in
                movq cstPtr (ind ~ofs:addr rbp)


let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let data =
        (label "_printf_log") ++ (string "%s\n") ++
        (label "_string_show_0") ++ (string "0") ++
        (label "_string_show_1") ++ (string "1") ++
        (label "_string_show_2") ++ (string "2") ++
        (label "_string_show_3") ++ (string "3") ++
        (label "_string_show_4") ++ (string "4") ++
        (label "_string_show_5") ++ (string "5") ++
        (label "_string_constante") ++ (string "blabla") ++
        (label "_show_string_int") ++ (string "%dA")
    in
    let text = traduit_a_file arbre_alloc in


                   
    {text = text ; data = data}
