#include <stdio.h>

int
main(int argc, char *argv[])
{
	unsigned char r;

	__asm__(
		"addq $-16, %%rsp\n\t"
		"movq %%rbp, 8(%%rsp)\n\t"
		"movq %%rsp, %%rbp\n\t"
		"movq $ret, (%%rbp)\n\t"
		"jmp cmm_main\n"
	"ret:\n\t"
		"movq 8(%%rsp), %%rbp\n\t"
		"addq $16, %%rsp\n"
	: "=b"(r)
	:
	: );

	printf("return value: %d\n", r);
	return 0;
}
