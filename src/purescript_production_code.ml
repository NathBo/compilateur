open X86_64
open Purescript_typage

let code_initial =
        globl "main" ++
        label "log" ++
        movq (ilab "printf_log") (reg rdi) ++
        call "printf" ++
        ret

let rec traduit_tddecl = function
        | TDtdecl _ -> nop
        | TDdefn tdefn -> traduit_tdefn tdefn
        | _ -> failwith "ne sait pas encore faire"
and traduit_tdefn tdefn =
        label tdefn.tident ++
        traduit_texpr tdefn.texpr ++
        movq (imm 0) (reg rax) ++
        ret
and traduit_texpr = function
        | TElident (fct, args, typ) ->
                call fct
        | _ -> failwith "ne sait pas encore faire"



let genere_code arbre_typage =
    let data = (label "printf_log") ++ (string "abdef\n") in
    let text = List.fold_left (fun acc tvdec -> acc ++ traduit_tddecl tvdec ) code_initial arbre_typage in
                   
    {text = text ; data = data}
