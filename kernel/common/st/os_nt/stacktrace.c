
/*	Win NT implementation of the stack trace library.
	Note - Dbghelp library we are using internaly is single threaded.
	We synchronise using the global mutex (hGlobalMutex). If waiting
	on the mutex times out we assume deadlock and report failure. */ 

#include <windows.h>
#include <Dbghelp.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include "../stacktrace.h"

#define GLOBAL_MUTEX_TIMEOUT	1000

/*	Conditionally defining STACK_GROWS_UPWARDS and STACK_GROWS_DOWNWARDS
	boolean constants. "Upwards" means stack grows towards higher addresses 
	and "downwards" means stack grows towards lower addresses. */ 
#if defined(_X86_) || defined(_AMD64_)
#define STACK_GROWS_UPWARDS		0
#define STACK_GROWS_DOWNWARDS	1
#else
#error Stacktrace library doesn't support current processor architecture!
#endif

static int isInitialized = 0;
static HANDLE hGlobalMutex = NULL;
static const DWORD exceptCode = 'ECTX';

//
static DWORD InvokeProcWithContextExceptionFilter(LPEXCEPTION_POINTERS exceptionPointers,
												  int (*proc) (PCONTEXT pContext, void *params),
												  void *params,
												  int *status)
{
	DWORD ret = EXCEPTION_CONTINUE_SEARCH;

	if (exceptionPointers->ExceptionRecord->ExceptionCode == exceptCode)
	{
		ret = EXCEPTION_EXECUTE_HANDLER;
		*status = proc(exceptionPointers->ContextRecord, params);		
	}

	return ret;
}
//
static
int InvokeProcWithContext(int (*proc) (PCONTEXT pContext, void *params), 
						  PCONTEXT pContext, 
						  void *params)
{
	int success = 0;

	assert(proc);
	if (pContext)
	{
		success = proc(pContext, params);
	}
	else
	{
		/*	We raise an exception to capture current thread context
			(using GetThreadContext for this purpose requires SuspendThread
			and hence we must start an additional thread if we use GetThreadCtx solution).
			Stack is analysed in the exception filter to ensure that
			stack's top isn't overwritten so we can walk the stack
			starting from the position encoded in the captured context. 
			
			TODO: what happens if an exception occurs during filter function
			execution? */ 
		__try { RaiseException(exceptCode, 0, 0, NULL); }
		__except (InvokeProcWithContextExceptionFilter(
			GetExceptionInformation(), proc, params, &success)) {}
	}
	return success;
}
//
typedef struct StackTraceWalkInternal2Params_tag_
{
	StackTraceWalkProc walkProc;
	void *userData;
	int limit;
	int offset;
	void *marker;
}
StackTraceWalkInternal2Params;
//
static
int StackTraceWalkInternal2(PCONTEXT, void *);
//
static 
int StackTraceWalkInternal(StackTraceWalkProc walkProc, 
						   PCONTEXT pContext, 
						   void *userData,
						   int limit,
						   int offset,
						   void *marker)
{
	int success = 0;
	StackTraceWalkInternal2Params params = { 0 };

	assert(walkProc);
	params.walkProc = walkProc;
	params.userData = userData;
	params.limit = limit;
	params.offset = offset;
	params.marker = marker;

	if (!isInitialized)
	{
		/* library not initialized */ 
	}
	else if (WAIT_OBJECT_0 != WaitForSingleObject(hGlobalMutex, GLOBAL_MUTEX_TIMEOUT))
	{
		/* waiting for mutex fails for some reasons */ 
	}
	else
	{
		success = InvokeProcWithContext(StackTraceWalkInternal2, pContext, &params);
		ReleaseMutex(hGlobalMutex);
	}

	return success;
}
//
static DWORD2ADDRESS64(LPADDRESS64 a64, DWORD_PTR address)
{
	assert(a64);
	a64->Offset = address;
	a64->Segment = 0;
	a64->Mode = AddrModeFlat;
}
//
static DWORD_PTR ADDRESS64_2DWORD(LPADDRESS64 a64)
{
	assert(a64);
	return (DWORD_PTR)a64->Offset;
}
//
static
int StackTraceWalkInternal2(PCONTEXT pContext, void *paramsPtr)
{
	int success = 0, bContinue = 1, frame = 0;
	StackTraceWalkInternal2Params *params = (StackTraceWalkInternal2Params *)paramsPtr;
	HANDLE hThread = NULL, hProcess = NULL;
	STACKFRAME64 stackFrame = {0};
	CONTEXT context = {0};
	DWORD machineType = -1;
	void *framep = NULL;

	assert(pContext && params && params->walkProc);
	context = *pContext;
	hProcess = GetCurrentProcess();
	hThread = GetCurrentThread();

#ifdef _X86_
	machineType = IMAGE_FILE_MACHINE_I386;
	DWORD2ADDRESS64(&stackFrame.AddrPC, context.Eip);
	DWORD2ADDRESS64(&stackFrame.AddrFrame, context.Ebp);
	DWORD2ADDRESS64(&stackFrame.AddrStack, context.Esp);
#elif defined(_AMD64_)
    machineType = IMAGE_FILE_MACHINE_AMD64;
	DWORD2ADDRESS64(&stackFrame.AddrPC, context.Rip);
	DWORD2ADDRESS64(&stackFrame.AddrFrame, context.Rbp);
	DWORD2ADDRESS64(&stackFrame.AddrStack, context.Rsp);
#else
#error "Unknown architecture format!"
#endif

	while (bContinue)
	{
		if (ADDRESS64_2DWORD(&stackFrame.AddrPC) == 0 ||
			frame - params->offset >= params->limit)
		{
			/* the stack is over */ 
			bContinue = 0;
			success = 1;
		}
		else if (!StackWalk64(
				machineType, hProcess, hThread, &stackFrame, &context, 
				NULL, SymFunctionTableAccess64, SymGetModuleBase64, NULL))
		{
			/* stack walk failed (why?) */ 
			bContinue = 0;
			success = 1; /* TODO: investigate how StackWalk reports of the stack end */ 
		}
		else if (framep = (void *) ADDRESS64_2DWORD(&stackFrame.AddrFrame),
				 params->marker != NULL && 
				 (STACK_GROWS_UPWARDS && params->marker < framep ||
				  STACK_GROWS_DOWNWARDS && params->marker > framep))
		{
			/* ignore frame */ 
		}
		else if (frame<params->offset)
		{
			/* skip frame */ 
			++frame;
		}
		else
		{
			IMAGEHLP_MODULE64 moduleInfo = {0};
			struct
			{
				IMAGEHLP_SYMBOL64 info;
				char nameBuf[512];
			}
			symbolInfoEx = {0};
			IMAGEHLP_LINE64 lineInfo = {0};
			DWORD64 displ = 0;
			StackFrameInfo frameInfo = {0};

			moduleInfo.SizeOfStruct = sizeof moduleInfo;
			if (SymGetModuleInfo64(hProcess, stackFrame.AddrPC.Offset, &moduleInfo))
			{
				frameInfo.module = moduleInfo.ImageName;
			}
			symbolInfoEx.info.SizeOfStruct = sizeof symbolInfoEx.info;
			symbolInfoEx.info.MaxNameLength = sizeof symbolInfoEx.nameBuf;
			if (SymGetSymFromAddr64(hProcess, stackFrame.AddrPC.Offset, &displ, &symbolInfoEx.info))
			{
				frameInfo.function = symbolInfoEx.info.Name;
			}
			lineInfo.SizeOfStruct = sizeof lineInfo;
			if (SymGetLineFromAddr64(hProcess, stackFrame.AddrPC.Offset, (PDWORD)&displ, &lineInfo))
			{
				frameInfo.line = lineInfo.LineNumber;
				frameInfo.file = lineInfo.FileName;
			}
			frameInfo.addr = (void*) ADDRESS64_2DWORD(&stackFrame.AddrPC);

			if (!params->walkProc(frame-params->offset, &frameInfo, &bContinue, params->userData))
			{
				/* walk proc failed */ 
				bContinue = 0;
			}
			else if (bContinue == 0)
			{
				/* walk proc requested end of walk */ 
				success = 1;
			}
			++frame;
		}
	}

	return success;
}
//
typedef struct StackTraceWriteFdWalkProcParams_tag_
{
	HANDLE handle;
}
StackTraceWriteFdWalkProcParams;
//
static int StackTraceWriteFdWalkProc(int frame, const StackFrameInfo *frameInfo, int *continueWalk, void *userData)
{
	DWORD dummy;
	StackTraceWriteFdWalkProcParams *params = NULL;
	char buf[0x1000];
	int charsCnt = 0;
	int success = 0;

	assert(frameInfo && continueWalk && userData);
	params = (StackTraceWriteFdWalkProcParams *)userData;

	charsCnt = PrintStackFrameInfo(_snprintf, buf, sizeof buf, frame, frameInfo, 0);
	if (charsCnt < 0)
	{
		/* output truncated */ 
		static const char truncated[] = {'.','.','.','<','t','r','u','n','c','a','t','e','d','>','\n'};
		const ptrdiff_t offset = (sizeof buf) - (sizeof truncated);
		assert (offset>0);
		memcpy(buf + offset, truncated, sizeof truncated);
		charsCnt = (int)sizeof buf;
	}

	success = (WriteFile(params->handle, buf, charsCnt, &dummy, NULL));

	return success;
}
//
__declspec(noinline)
int StackTraceWalk(void *context, 
				   StackTraceWalkProc walkProc, void *userData,
				   int limit, int offset)
{
	void *marker = NULL;

	if (!context) 
	{
		offset += 1;  /* have to hide StackTraceWalk frame */ 
		marker = &marker;
	}
	return StackTraceWalkInternal(walkProc, (PCONTEXT)context, userData, limit, offset, marker);
}
//
__declspec(noinline)
int StackTraceWriteFd(void *context, intptr_t handle, int limit, int offset)
{
	void *marker = NULL;
	StackTraceWriteFdWalkProcParams params = { 0 };

	params.handle = (HANDLE)handle;
	if (!context) 
	{
		offset += 1; /* have to hide StackTraceWriteFd frame */ 
		marker = &marker;
	}
	return StackTraceWalkInternal(StackTraceWriteFdWalkProc, 
								  (PCONTEXT)context, 
								  &params, 
								  limit, 
								  offset, 
								  marker);
}
//
int StackTraceInit()
{
	int success = 0;
	if (!isInitialized && 
		(hGlobalMutex = CreateMutex(NULL,FALSE,NULL)) && 
		SymInitialize(GetCurrentProcess(), NULL, TRUE))
	{
		success = 1;
	}
	if (!success)
	{
		CloseHandle(hGlobalMutex);
		hGlobalMutex = NULL;
	}
	if (!isInitialized) isInitialized = success;
	return success;
}
//
void StackTraceDeinit()
{
	if (isInitialized)
	{
		SymCleanup(GetCurrentProcess());
		CloseHandle(hGlobalMutex);
		isInitialized = 0;
	}
	hGlobalMutex = NULL;
}
