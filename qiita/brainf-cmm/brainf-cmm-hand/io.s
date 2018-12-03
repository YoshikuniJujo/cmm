	.comm ch,1

	.global putchar_syscall
putchar_syscall:
	movb %bl, ch(%rip)
	movq $1, %rax
	movq $1, %rdi
	movq $ch, %rsi
	movq $1, %rdx
	syscall
	xorq %rbx, %rbx
	jmp *(%rbp)

	.global getchar_syscall
getchar_syscall:
	movq $0, %rax
	movq $0, %rdi
	movq $ch, %rsi
	movq $1, %rdx
	syscall
	xorq %rbx, %rbx
	movb ch(%rip), %bl
	jmp *(%rbp)
