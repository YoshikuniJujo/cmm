	.global fun
fun:
	movq $1, %rax
	movq $1, %rdi
	movq $msg, %rsi 
	movq $14, %rdx
	syscall
	movq $12345, %rbx
	jmp *(%rbp)

	.section	.rodata
msg:
	.string "Hello, world\n"
