/* GNU backtraces
 * 	http://docs.linux.cz/programming/c/www.gnu.org/software/libc/manual/html_node/Backtraces.html#Backtraces
 *
 * Solaris walkcontext
 *	http://docs.sun.com/app/docs/doc/819-2243/printstack-3c?a=view
 *
 * Stack parameters not reported properly with either getcontext() or
 * when invoking signal handlers
 * 	http://sources.redhat.com/bugzilla/show_bug.cgi?id=759
 */

#define _GNU_SOURCE
#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <ucontext.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <dlfcn.h>

/* Obtain a backtrace and print it to stdout. */
void
print_trace (void)
{
void *array[10];
size_t size;
char **strings;
size_t i;

size = backtrace (array, 10);
strings = backtrace_symbols (array, size);

printf ("Obtained %zd stack frames.\n", size);

for (i = 0; i < size; i++)
printf ("%s\n", strings[i]);

free (strings);
}

/* A dummy function to make the backtrace more interesting. */
void
dummy_function ()
{
print_trace ();
}

#define OFFSET(PTR,DIST) (void*)((char*)(PTR)+(DIST))

/* Explore process VM layout parsing /proc/${PID}/maps.
 * If the segment enclosing sample location is found
 * function succeeds. Non-zero return status indicates 
 * success. If segment is found and is availible for
 * reading start and end variables recieves pointers
 * to the start and the end of segment. */
int gsegbounds(void *sample, void **start, void **end)
{
	int success = 0, fd = -1;
	void *localStart = NULL, *localEnd = NULL;
	char buf[256] = {0}, localPathname[256] = {0}, localPerms[16] = {0};
	int localOffset = 0, localDevMinor = 0, localDevMajor = 0;
	int localInode = 0;
	
	assert(start && end);
	*start = *end = NULL;

	sprintf(buf, "/proc/%d/maps", getpid());
	fd = open(buf, O_RDONLY);

	if (fd != -1)
	{
		char *bufGetCursor = buf, *bufPutCursor = buf;
		char *bufEolCursor = NULL;
		ssize_t readSz = 0, availSz = 0;
		int numParsed = 0;

		while (1)
		{
			if (bufGetCursor != buf)
			{
				readSz = bufPutCursor - bufGetCursor;
				memmove(buf, bufGetCursor, readSz);
				bufGetCursor = buf;
				bufPutCursor = buf + readSz;
			}
			availSz = sizeof buf - 1 - (bufPutCursor - buf);
			readSz = read(fd, bufPutCursor, availSz);
			if (readSz == -1)
			{
				if (errno == EINTR) continue;
				break;
			}
			0[bufPutCursor+=readSz]=0;
			if (readSz == 0 && bufGetCursor >= bufPutCursor) break;
			bufEolCursor = strchr(buf,'\n');
			if (!bufEolCursor) 
			{
				if (readSz == 0) bufEolCursor = bufPutCursor;
				else break;
			}
			*bufEolCursor = 0;
			strcpy(localPathname,"");
			numParsed = sscanf(bufGetCursor,
					"%p-%p %15s %x %x:%x %d %255s",
					&localStart, &localEnd, 
					localPerms, &localOffset, 
					&localDevMajor, &localDevMinor,
					&localInode, localPathname);
			if (numParsed != 7 && numParsed != 8) break;
			bufGetCursor = bufEolCursor+1;

			if (localStart<=sample && sample<localEnd)
			{
				success = 1;
				if (strchr(localPerms,'r')==NULL)
				{
					localStart = localEnd = NULL;
				}
				break;
			}
		}
	}
	close(fd); fd=-1;
	if (success) {*start = localStart; *end = localEnd;}
	return success;
}

void gsymstr(void *addr, char *buf)
{
	Dl_info dlinfo = {};
	if (!dladdr(addr, &dlinfo))
	{
		strcpy(buf,"error");
	}
	else
	{
		sprintf(buf,"%s %s", dlinfo.dli_fname, dlinfo.dli_sname);
	}
}

void ctxstracefd(const ucontext_t *ucontext, int fd)
{
	uintptr_t stack_top = 0, stack_bot = 0, stack = 0, code = 0, desert = 0;
	char buf[1024], buf2[512];
	size_t sz=0;

	assert(ucontext);
	stack = ucontext->uc_mcontext.gregs[REG_EBP];
	desert = stack - 1;
	code = ucontext->uc_mcontext.gregs[REG_EIP];

	if (gsegbounds((void*)stack,(void**)&stack_top,(void**)&stack_bot) &&
		stack_top<=stack && stack<stack_bot)
	{
		/* We have to check that pointers in the list of 
		 * stack frames do not point outside the stack segment. 
		 * We also assume that we will move towards the stack 
		 * bottom while walking the stack. This way we protect 
		 * from circular stack frames list in a corrupted stack.
		 * If (stack == 0), we are in the topmost frame. However
		 * we still have to extract the function address. */ 
		while (stack == 0 || stack > desert && stack<=stack_bot)
		{
			gsymstr((void*)code, buf2);
			sz = sprintf(buf,"%p %s\n",code,buf2);
			write(fd, buf, sz);
			if (stack == 0) break;
			desert = stack;
			code  = 1[(uintptr_t *)stack];
			stack = 0[(uintptr_t *)stack];
		}
	}
}


void sig_handler(int signo, siginfo_t *siginfo, void *ucontext)
{
dummy_function();
ctxstracefd(ucontext, 2);
abort();
}

void dummy_function2 (void)
{
strcpy(NULL,"DIE");
}

void dummy_function3 (void)
{
 dummy_function2();
}

int
main (void)
{
void *a = NULL, *b = NULL;
struct sigaction act;
act.sa_sigaction = sig_handler;
sigemptyset(&act.sa_mask);
act.sa_flags = SA_SIGINFO;

gsegbounds(NULL,&a,&b);
sigaction (SIGSEGV, &act, NULL);
dummy_function3();

return 0;
}

