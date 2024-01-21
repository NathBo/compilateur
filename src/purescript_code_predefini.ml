open X86_64

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
        
        label ".Show.show.1" ++
        enter (imm 0) ++
        movq (imm 24) (reg rdi) ++
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

        label ".Show.show.0" ++
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
        ret ++

        label "not" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rax) ++
        xorq (imm 1) (reg rax) ++
        leave ++
        ret ++

        label "_concat" ++
        enter (imm 0) ++
        movq (ind ~ofs:16 rbp) (reg rdi) ++
        call "strlen" ++
        movq (reg rax) (reg r9) ++
        movq (ind ~ofs:24 rbp) (reg rdi) ++
        call "strlen" ++
        addq (reg rax) (reg r9) ++
        incq (reg r9) ++
        movq (reg r9) (reg rdi) ++
        call "malloc" ++
        movq (reg rax) (reg r8) ++
        movq (reg rax) (reg rdi) ++
        movq (ind ~ofs:16 rbp) (reg rsi) ++
        call "strcpy" ++
        movq (ind ~ofs:24 rbp) (reg rsi) ++
        call "strcat" ++
        movq (reg r8) (reg rax) ++
        leave ++
        ret ++

        label "pure" ++
        enter (imm 0) ++
        movq (imm 0) (reg rax) ++
        leave ++
        ret
