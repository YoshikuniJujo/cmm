#include <stdio.h>

int
main(int argc, char *argv[])
{
	int ret;

	__asm__(
		"addq $-16, %%rsp\n\t"
		"movq %%rbp, 8(%%rsp)\n\t"
		"movq %%rsp, %%rbp\n\t"
		"movq $print_ret, (%%rbp)\n\t"
		"jmp foo\n"
	"print_ret:\n\t"
		"movq 8(%%rsp), %%rbp\n\t"
		"addq $16, %%rsp\n"
	:"=b"(ret)
	:
	: );

	printf("ret: %d\n", ret);
	return 0;
}
