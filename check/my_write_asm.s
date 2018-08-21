	.section	.rodata
.LC0:
	.string	"Hello, world!\n"
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movl	%edi, -36(%rbp)
	movq	%rsi, -48(%rbp)
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	leaq	.LC0(%rip), %rax
	movq	%rax, -16(%rbp)
#APP
# 10 "my_write.c" 1
	movq $1, %rax
	movq $1, %rdi
	movq -16(%rbp), %rsi
	movq $14, %rdx
	syscall
# 0 "" 2
#NO_APP
	movl	%eax, -20(%rbp)
	movl	$0, %eax
	movq	-8(%rbp), %rdx
	xorq	%fs:40, %rdx
	je	.L3
	call	__stack_chk_fail@PLT
.L3:
	leave
	ret
