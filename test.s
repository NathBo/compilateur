	.text
	.globl	main
log:
	enter $0, $0
	movq 16(%rbp), %rsi
	movq $_printf_log, %rdi
	xorq %rax, %rax
	call printf
	leave
	ret
_show_int:
	enter $0, $0
	movq $24, %rdi
	call malloc
	movq %rax, %rdi
	movq $_show_string_int, %rsi
	xorq %rax, %rax
	movq 16(%rbp), %rdx
	call sprintf
	movq %rax, %r8
	movq %rdi, %rax
	subq %r8, %rax
	addq %rax, %r8
	movb $0, -1(%r8)
	leave
	ret
_show_bool:
	enter $0, $0
	cmpq $0, 16(%rbp)
	je _show_bool_false
	movq $_true, %rax
	leave
	ret
_show_bool_false:
	movq $_false, %rax
	leave
	ret
_divide:
	enter $0, $0
	movq 16(%rbp), %rax
	testq %rax, %rax
	js _divide_neg
	movq 24(%rbp), %rcx
	movq $0, %rdx
	idivq %rcx
	leave
	ret
_divide_neg:
	movq 24(%rbp), %rcx
	movq $-1, %rdx
	idivq %rcx
	testq %rdx, %rdx
	jnz _divide_mod_1
	leave
	ret
_divide_mod_1:
	cmpq $0, 24(%rbp)
	jg _divide_neg_decr
	incq %rax
	leave
	ret
_divide_neg_decr:
	decq %rax
	leave
	ret
mod:
	enter $0, $0
	movq 16(%rbp), %rax
	testq %rax, %rax
	js _mod_neg
	movq 24(%rbp), %rcx
	movq $0, %rdx
	idivq %rcx
	movq %rdx, %rax
	leave
	ret
_mod_neg:
	movq 24(%rbp), %rcx
	movq $-1, %rdx
	idivq %rcx
	testq %rdx, %rdx
	movq %rdx, %rax
	testq %rax, %rax
	jz _mod_ret_0
	cmpq $0, 24(%rbp)
	js _mod_neg_neg
	addq 24(%rbp), %rax
	leave
	ret
_mod_neg_neg:
	subq 24(%rbp), %rax
	leave
	ret
_mod_ret_0:
	movq $0, %rax
	leave
	ret
not:
	enter $0, $0
	movq 16(%rbp), %rax
	xorq $1, %rax
	leave
	ret
_concat:
	enter $0, $0
	movq 16(%rbp), %rdi
	call strlen
	movq %rax, %r9
	movq 24(%rbp), %rdi
	call strlen
	addq %rax, %r9
	incq %r9
	movq %r9, %rdi
	call malloc
	movq %rax, %r8
	movq %rax, %rdi
	movq 16(%rbp), %rsi
	call strcpy
	movq 24(%rbp), %rsi
	call strcat
	movq %r8, %rax
	leave
	ret
pure:
	enter $0, $0
	movq $0, %rax
	leave
	ret
main:
	enter $304, $0
	movq $_str_const_0, -16(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %rax
	movq %rax, -32(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -40(%rbp)
	pushq $0
	pushq -40(%rbp)
	call log
	popq %r8
	popq %r8
	movq %rax, -48(%rbp)
	movq $_str_const_2, -72(%rbp)
	movq -72(%rbp), %rax
	movq %rax, -80(%rbp)
	movq -80(%rbp), %rax
	movq %rax, -56(%rbp)
	movq $_str_const_1, -88(%rbp)
	movq -88(%rbp), %rax
	movq %rax, -96(%rbp)
	movq -96(%rbp), %rax
	movq %rax, -64(%rbp)
	movq -56(%rbp), %rax
	movq %rax, -104(%rbp)
	movq -104(%rbp), %rax
	movq %rax, -112(%rbp)
	pushq $0
	pushq -112(%rbp)
	call log
	popq %r8
	popq %r8
	movq %rax, -120(%rbp)
	movq $_str_const_4, -144(%rbp)
	movq -144(%rbp), %rax
	movq %rax, -152(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -128(%rbp)
	movq $_str_const_3, -160(%rbp)
	movq -160(%rbp), %rax
	movq %rax, -168(%rbp)
	movq -168(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -136(%rbp), %rax
	movq %rax, -176(%rbp)
	movq -176(%rbp), %rax
	movq %rax, -184(%rbp)
	pushq $0
	pushq -184(%rbp)
	call log
	popq %r8
	popq %r8
	movq %rax, -192(%rbp)
	movq $_str_const_6, -216(%rbp)
	movq -216(%rbp), %rax
	movq %rax, -224(%rbp)
	movq -224(%rbp), %rax
	movq %rax, -200(%rbp)
	movq $_str_const_5, -232(%rbp)
	movq -232(%rbp), %rax
	movq %rax, -240(%rbp)
	movq -240(%rbp), %rax
	movq %rax, -208(%rbp)
	movq -200(%rbp), %rax
	movq %rax, -248(%rbp)
	movq -208(%rbp), %rax
	movq %rax, -256(%rbp)
	pushq -256(%rbp)
	pushq -248(%rbp)
	call _concat
	popq %r8
	popq %r8
	movq %rax, -264(%rbp)
	movq -264(%rbp), %rax
	movq %rax, -272(%rbp)
	pushq $0
	pushq -272(%rbp)
	call log
	popq %r8
	popq %r8
	movq %rax, -280(%rbp)
	movq $0, -288(%rbp)
	movq $0, %rax
	leave
	ret
	.data
_printf_log:
	.string "%s\n"
_string_constante:
	.string "blabla"
_show_string_int:
	.string "%dA"
_true:
	.string "true"
_false:
	.string "false"
_str_const_0:
	.string "a"
_str_const_1:
	.string "b"
_str_const_2:
	.string "a"
_str_const_3:
	.string "b"
_str_const_4:
	.string "a"
_str_const_5:
	.string "b"
_str_const_6:
	.string "a"
