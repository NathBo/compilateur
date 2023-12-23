open X86_64

let genere_code arbre_typage =
    let data = (label "message") ++ (string "hello word") in
    let text = globl "main" ++ label "main" in
    {text = text ; data = data}
