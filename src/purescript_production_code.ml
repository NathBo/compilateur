open X86_64
open Purescript_allocation

let code_initial =
        globl "main" ++

        label "log" ++
        (*movq (ilab "printf_log") (reg rdi) ++ *)
        call "printf" ++
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
        traduit_a_expr defn.a_expr ++
        movq (imm 0) (reg rax) ++
        ret
and traduit_a_expr = function
        | A_lident (fct, params, typ, addr) ->
                List.fold_left (fun acc atom -> acc ++ traduit_a_atom atom) nop params ++
                call fct ++ 
                movq (reg rax) (reg rdi)

and traduit_a_atom = function
        | A_expr (expr, typ) -> traduit_a_expr expr
        | A_constant (const, typ) ->
                movq (imm 1) (reg rax)


let genere_code arbre_typage =
    let arbre_alloc = typage_to_alloc arbre_typage in
    print_a_file Format.std_formatter arbre_alloc;

    let data = (label "printf_log") ++ (string "abdef\n") ++ (label "string_show") ++ (string "coucou\n") in
    let text = traduit_a_file arbre_alloc in


                   
    {text = text ; data = data}
