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
printaux:
	enter $192, $0
	movq 24(%rbp), %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %r8
	cmpq $0, 0(%r8)
	jne _branch_3
	movq 16(%rbp), %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	movq %rax, -184(%rbp)
	jmp _branch_fin_0
_branch_3:
	movq -8(%rbp), %r8
	cmpq $1, 0(%r8)
	jne _branch_2
	movq -8(%rbp), %r9
	movq 8(%r9), %rax
	movq %rax, -48(%rbp)
	movq $0, -40(%rbp)
	movq -8(%rbp), %r9
	movq -40(%rbp), %r8
	cmpq %r8, 16(%r9)
	jne _branch_2
	movq 16(%rbp), %rax
	movq %rax, -64(%rbp)
	movq $_str_const_1, -80(%rbp)
	movq -80(%rbp), %rax
	movq %rax, -72(%rbp)
	movq -72(%rbp), %rax
	movq %rax, -88(%rbp)
	pushq -88(%rbp)
	pushq -64(%rbp)
	call _concat
	popq %r8
	popq %r8
	movq %rax, -96(%rbp)
	pushq -48(%rbp)
	pushq -96(%rbp)
	call printaux
	popq %r8
	popq %r8
	movq %rax, -104(%rbp)
	movq -104(%rbp), %rax
	movq %rax, -184(%rbp)
	jmp _branch_fin_0
_branch_2:
	movq -8(%rbp), %r8
	cmpq $1, 0(%r8)
	jne _branch_1
	movq -8(%rbp), %r9
	movq 8(%r9), %rax
	movq %rax, -120(%rbp)
	movq -8(%rbp), %r9
	movq 16(%r9), %rax
	movq %rax, -112(%rbp)
	movq 16(%rbp), %rax
	movq %rax, -136(%rbp)
	movq $_str_const_0, -152(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -144(%rbp)
	movq -144(%rbp), %rax
	movq %rax, -160(%rbp)
	pushq -160(%rbp)
	pushq -136(%rbp)
	call _concat
	popq %r8
	popq %r8
	movq %rax, -168(%rbp)
	pushq -120(%rbp)
	pushq -168(%rbp)
	call printaux
	popq %r8
	popq %r8
	movq %rax, -176(%rbp)
	movq -176(%rbp), %rax
	movq %rax, -184(%rbp)
	jmp _branch_fin_0
_branch_1:
	movq $0, -184(%rbp)
_branch_fin_0:
	movq -184(%rbp), %rax
	leave
	ret
.Show.show.2:
	enter $32, $0
	movq $_str_const_2, -16(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -8(%rbp)
	pushq 16(%rbp)
	pushq -8(%rbp)
	call printaux
	popq %r8
	popq %r8
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	leave
	ret
next:
	enter $160, $0
	movq 24(%rbp), %rax
	movq %rax, -8(%rbp)
	movq -8(%rbp), %r8
	cmpq $0, 0(%r8)
	jne _branch_6
	movq $24, %rdi
	call malloc
	movq %rax, -48(%rbp)
	movq $1, 0(%rax)
	movq $1, -32(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -24(%rbp)
	movq -48(%rbp), %r9
	movq -24(%rbp), %rax
	movq %rax, 8(%r9)
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	movq %rax, -40(%rbp)
	movq -48(%rbp), %r9
	movq -40(%rbp), %rax
	movq %rax, 16(%r9)
	movq -48(%rbp), %rax
	movq %rax, -144(%rbp)
	jmp _branch_fin_4
_branch_6:
	movq -8(%rbp), %r8
	cmpq $1, 0(%r8)
	jne _branch_5
	movq -8(%rbp), %r9
	movq 8(%r9), %rax
	movq %rax, -64(%rbp)
	movq -8(%rbp), %r9
	movq 16(%r9), %rax
	movq %rax, -56(%rbp)
	movq $24, %rdi
	call malloc
	movq %rax, -136(%rbp)
	movq $1, 0(%rax)
	movq 16(%rbp), %rax
	movq %rax, -80(%rbp)
	movq -56(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -80(%rbp), %r8
	addq -88(%rbp), %r8
	movq %r8, -96(%rbp)
	movq $7, -112(%rbp)
	movq -112(%rbp), %rax
	movq %rax, -104(%rbp)
	pushq -104(%rbp)
	pushq -96(%rbp)
	call mod
	popq %r8
	popq %r8
	movq %rax, -120(%rbp)
	movq -136(%rbp), %r9
	movq -120(%rbp), %rax
	movq %rax, 8(%r9)
	pushq -64(%rbp)
	pushq -56(%rbp)
	call next
	popq %r8
	popq %r8
	movq %rax, -128(%rbp)
	movq -136(%rbp), %r9
	movq -128(%rbp), %rax
	movq %rax, 16(%r9)
	movq -136(%rbp), %rax
	movq %rax, -144(%rbp)
	jmp _branch_fin_4
_branch_5:
	movq $0, -144(%rbp)
_branch_fin_4:
	movq -144(%rbp), %rax
	leave
	ret
pascal:
	enter $160, $0
	movq 24(%rbp), %rax
	movq %rax, -8(%rbp)
	movq 32(%rbp), %rax
	movq %rax, -16(%rbp)
	movq -8(%rbp), %r8
	movq -16(%rbp), %r9
	cmpq %r9, %r8
	jl _binop_inf_0
	movq $0, -24(%rbp)
	jmp _binop_inf_fin_0
_binop_inf_0:
	movq $1, -24(%rbp)
_binop_inf_fin_0:
	cmpq $0, -24(%rbp)
	je _if_lab_false_0
	pushq $0
	pushq 16(%rbp)
	call .Show.show.2
	popq %r8
	popq %r8
	movq %rax, -32(%rbp)
	pushq $0
	pushq -32(%rbp)
	call log
	popq %r8
	popq %r8
	movq %rax, -40(%rbp)
	movq $0, -56(%rbp)
	movq -56(%rbp), %rax
	movq %rax, -48(%rbp)
	pushq 16(%rbp)
	pushq -48(%rbp)
	call next
	popq %r8
	popq %r8
	movq %rax, -64(%rbp)
	movq 24(%rbp), %rax
	movq %rax, -72(%rbp)
	movq $1, -88(%rbp)
	movq -88(%rbp), %rax
	movq %rax, -80(%rbp)
	movq -80(%rbp), %rax
	movq %rax, -96(%rbp)
	movq -72(%rbp), %r8
	addq -96(%rbp), %r8
	movq %r8, -104(%rbp)
	pushq $0
	pushq 32(%rbp)
	pushq -104(%rbp)
	pushq -64(%rbp)
	call pascal
	popq %r8
	popq %r8
	popq %r8
	popq %r8
	movq %rax, -112(%rbp)
	movq $0, -120(%rbp)
	movq -120(%rbp), %rax
	movq %rax, -152(%rbp)
	jmp _if_lab_end_0
_if_lab_false_0:
	movq $1, -136(%rbp)
	movq -136(%rbp), %rax
	movq %rax, -128(%rbp)
	pushq $0
	pushq -128(%rbp)
	call pure
	popq %r8
	popq %r8
	movq %rax, -144(%rbp)
	movq -144(%rbp), %rax
	movq %rax, -152(%rbp)
_if_lab_end_0:
	movq $0, %rax
	leave
	ret
main:
	enter $64, $0
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	movq %rax, -8(%rbp)
	movq $0, -24(%rbp)
	movq -24(%rbp), %rax
	movq %rax, -16(%rbp)
	movq $42, -40(%rbp)
	movq -40(%rbp), %rax
	movq %rax, -32(%rbp)
	pushq $0
	pushq -32(%rbp)
	pushq -16(%rbp)
	pushq -8(%rbp)
	call pascal
	popq %r8
	popq %r8
	popq %r8
	popq %r8
	movq %rax, -48(%rbp)
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
	.string "*"
_str_const_1:
	.string "."
_str_const_2:
	.string ""
