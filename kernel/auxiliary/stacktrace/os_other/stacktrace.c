#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

/* Generic implementation assuming we are unable to examine stack traces. */ 
#include "../stacktrace.h"

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

int StackTraceWriteFd(void *context, intptr_t fd, int limit, int offset)
{
	static const char errorMsg[] = 
		"<unable to produce stack trace - feature disabled>\n";

#ifdef _WIN32
	DWORD dummy;
	WriteFile((HANDLE)fd, errorMsg, sizeof(errorMsg) - 1, &dummy, NULL);
#else
	write(fd, errorMsg, (sizeof errorMsg) - 1);
#endif

	return 0;
}
