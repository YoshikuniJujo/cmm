	.global getchar_syscall
getchar_syscall:
	movq $0, %rax
	movq $0, %rdi
	movq $msg, %rsi 
	movq $1, %rdx
	syscall
	xorq %rbx, %rbx
	movb msg(%rip), %bl
	jmp *(%rbp)

	.comm msg,1
