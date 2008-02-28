#include <string.h>
#include <assert.h>
#include "stacktrace.h"

static const char *GetFileNameFromFilepath(const char *filepath);

int PrintStackFrameInfo(SnprintfLikeProc proc, 
						char *buf,
						size_t count,
						int frame, 
						const StackFrameInfo *frameInfo,
						int reserved)
{
	static const char
		*fullFmt =	"#%-3d %s -- %s -- %s:%d\n",/* #0    function -- module.exe -- file.cpp:10 */ 
		*nomodFmt = "#%-3d %s -- %s:%d\n",		/* #1    procedur -- file.cpp:10 */ 
		*nolnFmt =	"#%-3d %s -- %s\n",			/* #2    foofunct -- module.exe */ 
		*nosrcFmt =	"#%-3d %08p -- %s\n",		/* #3    4000E7BC -- module.exe */ 
		*minFmt =	"#%-3d %08p\n";				/* #4    4000E7A0 */ 

	int result = -1;
	const char *module = NULL, *file = NULL, *function=NULL;

	assert(frameInfo && proc);
	function = frameInfo->function;
	module = GetFileNameFromFilepath(frameInfo->module);
	file = GetFileNameFromFilepath(frameInfo->file);

	if (module && file && function)
	{
		result = proc(buf, count, fullFmt, frame, function, module, file, frameInfo->line);
	}
	else if (file && function)
	{
		result = proc(buf, count, nomodFmt, frame, function, file, frameInfo->line);
	}
	else if (module && function)
	{
		result = proc(buf, count, nolnFmt, frame, function, module);
	}
	else if (module)
	{
		result = proc(buf, count, nosrcFmt, frame, frameInfo->addr, module);
	}
	else
	{
		result = proc(buf, count, minFmt, frame, frameInfo->addr);
	}

	return result;
}

static const char *GetFileNameFromFilepath(const char *filepath)
{
	const char *result = NULL, *temp = filepath;

	while (filepath && temp)
	{
		result = temp;
		temp = strpbrk(temp,"/\\");
		if (temp) ++temp;
	}
	return result;
}
