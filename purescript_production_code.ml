open X86_64

let genere_code arbre_typage =
    let data = (label "message") ++ (string "le nombre es %d.\n") in
    let print_int =
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "message") (reg rdi) ++
        call "printf" ++
        ret
    in
    let main =
        label "main" ++
        movq (imm 42) (reg rdi) ++
        call "print_int" ++
        movq (imm 0) (reg rax) ++
        ret
    in
            
    let text = print_int ++ main in
    {text = (globl "main" ++ text) ; data = data}
