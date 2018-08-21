	.global putchar_syscall
putchar_syscall:
//	xorq %rbx, %rbx
//	movb $112, %bl
	movb %bl, msg(%rip)
	movq $1, %rax
	movq $1, %rdi
	movq $msg, %rsi 
	movq $1, %rdx
	syscall
	movq $12345, %rbx
	jmp *(%rbp)

	.comm msg,1
