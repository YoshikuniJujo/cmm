#include <stdio.h>

int foo(void);

int
main(int argc, char *argv[])
{
	int r;

	foo();
	__asm__("":"=b"(r)::);
	printf("ret: %d\n", r);
	return 0;
}
