#include <stdio.h>

int
main(int argc, char *argv[])
{
	int r;

	__asm__(
		"addq $-8,%%rbp\n\t"
		"movq $print_ret,(%%rbp)\n\t"
		"jmp call_factorial\n"
	"print_ret:"
	:"=b"(r)
	:
	: );

	printf("10! = %d\n", r);
	return 0;
}
