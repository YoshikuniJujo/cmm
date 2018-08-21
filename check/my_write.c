#define STDOUT_FILENO	1
#define WRITE_SYS_NUM	1

int
main(int argc, char *argv[])
{
	int err;
	const char *text = "abc\n";

	__asm__(
		"movq %1, %%rax\n\t"
		"movq %2, %%rdi\n\t"
		"movq %3, %%rsi\n\t"
		"movq %4, %%rdx\n\t"
		"syscall"
	:	"=a"(err)
	:	"i"(WRITE_SYS_NUM), "i"(STDOUT_FILENO), "m"(text),
		"i"(sizeof(text) - 1)
	: );
}
