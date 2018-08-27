	.global putstr_syscall
putstr_syscall:
	movq $1, %rax
	movq $1, %rdi
	movq %rbx, %rsi
	movq %r14, %rdx
	syscall
	xorq %rbx, %rbx
	jmp *(%rbp)
