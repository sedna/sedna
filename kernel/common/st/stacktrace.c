#ifdef _WIN32
#include <io.h>
#define write _write
#else
#include <unistd.h>
#endif

/* Generic implementation assuming we are unable to examine stack traces. */ 
#include "stacktrace.h"

int StackTraceInit()
{
	return 0;
}

void StackTraceDeinit()
{
	;
}

int StackTraceWalk(void *context, StackTraceWalkProc walkerProc, void *userData,
				   int limit, int offset)
{
	return 0;
}

int StackTraceWriteFd(void *context, int fd, int limit, int offset)
{
	static const char errorMsg[] = 
		"<unable to produce stack trace - feature disabled>\n";

	write(fd, errorMsg, (sizeof errorMsg) - 1);

	return 0;
}
