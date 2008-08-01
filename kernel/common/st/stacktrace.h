/* Writing stack traces, cross platform. */ 

#if (_MDC_VER > 1000)
#pragma once
#endif

#ifndef STACKTRACE_H_INCLUDED
#define STACKTRACE_H_INCLUDED

#ifndef EXTERNC
#ifndef __cplusplus
#define EXTERNC
#else
#define EXTERNC extern "C"
#endif
#endif

#include "stdint.h"

/*	Init the library. Non-zero indicates success. */ 
EXTERNC int StackTraceInit();

/*	Deinit library - safe (though redundant) to call even if init failed. */ 
EXTERNC void StackTraceDeinit();

/*	Write stack trace to file identified by descriptor fd (handle on WIN). 
	If context is NULL the calling thread stack trace 
	is writen (except the StackTraceWriteFd frames). If context
	non-NULL it must point to platform specific structure capturing
	the thread context (CONTEXT on NT, ucontext_t on UNIX). 
	Non-zero indicates success. 
	If the library is not initialised the call will fail (but won't crash). */ 
EXTERNC int StackTraceWriteFd(void *context, intptr_t fd, int limit, int offset);

typedef struct StackFrameInfo_tag_
{
	const char *function;
	const char *module;
	const char *file;
	int line;
	void *addr;
}
StackFrameInfo;

typedef int (*StackTraceWalkProc)(int frame, const StackFrameInfo *frameInfo, int *continueWalk, void *userData);

/*	Walk the stack invoking walkProc for each frame on the stack.
	If context is NULL the calling thread stack is examined
	(except StackTraceWalk frames). If context
	non-NULL it must point to platform specific structure capturing
	the thread context (CONTEXT on NT, ucontext_t on UNIX). 
	Walk commences if either all frames were visited or the walkProc
	resets continueWalk variable or the walkProc fails 
	(the walkProc indicates sucess with non-zero return value).
	Non-zero indicates success (no error condition encountered - either 
	all frames visited or the walkProc intentionaly stops the walk). 
	If the library is not initialised the call will fail (but won't crash).*/ 
EXTERNC int StackTraceWalk(void *context, 
						   StackTraceWalkProc walkProc, void *userData,
						   int limit, int offset);

typedef
int (*SnprintfLikeProc) (char *buf, size_t count, const char *fmt, ...);

EXTERNC 
int PrintStackFrameInfo(SnprintfLikeProc proc, 
						char *buf,
						size_t count,
						int frame, 
						const StackFrameInfo *frameInfo,
						int reserved);
#endif
