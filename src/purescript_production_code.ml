open X86_64
open Purescript_typage

let code_initial =
        globl "main" ++
        label "log" ++
        movq (ilab "printf_log") (reg rdi) ++
        call "printf" ++
        ret


let genere_code arbre_typage =
    let data = (label "printf_log") ++ (string "abdef\n") in
    let text = List.fold_left (fun acc tvdec -> acc ++ match tvdec with
        | TDtdecl _ -> nop
        | TDdefn tdefn ->
                label tdefn.tident ++
                begin match tdefn.texpr with
                | TElident (fct, args, typ) ->
                        call fct
                | _ -> 
                        label tdefn.tident ++
                        movq (imm 42) (reg rdi) ++
                        call "log" ++
                        movq (imm 0) (reg rax) ++
                        ret
                end ++
                movq (imm 0) (reg rax) ++
                ret
        | _ -> failwith "ne sait pas encore faire"
    ) code_initial arbre_typage in
                   
    {text = text ; data = data}
