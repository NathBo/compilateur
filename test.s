	.text
	.globl	main
log:
	movq $printf_log, %rdi
	call printf
	ret
fact:
fact:
	movq $42, %rdi
	call log
	movq $0, %rax
	ret
	movq $0, %rax
	ret
main:
	call log
	movq $0, %rax
	ret
	.data
printf_log:
	.string "abdef\n"
