open X86_64
open Purescript_allocation

let code_initial =
        globl "main" ++

        label "log" ++
        pushq (reg rbp) ++
        movq (reg rsp) (reg rbp) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        movq (ilab "printf_log") (reg rdi) ++
        call "printf" ++
        popq rbp ++
        ret ++
 
        
        label "show" ++
        movq (ilab "string_show") (reg rax) ++
 
        ret


let rec traduit_a_file file =
        List.fold_left (fun acc tdecl -> acc ++ traduit_a_tdecl tdecl) code_initial file

and traduit_a_tdecl = function
        | A_defn x -> traduit_a_defn x

and traduit_a_defn defn =
        label defn.a_ident ++
        pushq (reg rbp) ++
        movq (reg rsp) (reg rbp) ++
        subq (imm (abs defn.tableau_activation)) (reg rsp) ++

        traduit_a_expr defn.a_expr ++

        addq (imm (abs defn.tableau_activation)) (reg rsp) ++
        popq rbp ++
        
        movq (imm 0) (reg rax) ++
        ret

and traduit_a_expr = function
        | A_lident (fct, params, typ, addr) ->
                (* calcul des params *)
                List.fold_left (fun acc atom -> acc ++ (
                        traduit_a_atom atom
                )) nop params ++

                (* placer les params au bon endroit *)
                List.fold_left (fun acc atom -> acc ++ (   (* reflechir au bon sens *)
                        pushq (ind ~ofs:(atom_adr atom) rbp)
                )) nop params ++

                (* appel recursif *)
                call fct ++ 
                List.fold_left (fun acc atom -> acc ++ (   (* reflechir au bon sens *)
                        popq r8
                )) nop params ++


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
                movq (imm 1) (ind ~ofs:addr rbp)


let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let data = (label "printf_log") ++ (string "log -> %s\n") ++ (label "string_show") ++ (string "coucou") in
    let text = traduit_a_file arbre_alloc in


                   
    {text = text ; data = data}
