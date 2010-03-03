/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"
#include "common/commutil.h"
#include "common/sedna.h"
#include "tr/vmm/os_exceptions.h"
#include "tr/tr_globals.h"


int IsAccessViolationNonFatal(void *addr, void *context)
{
	return 0;
}

enum WriteFaultFileKind
{
	WFF_UNKNOWN_ERROR,
	WFF_ACCESS_VIOLATION,
	WFF_STACK_OVERFLOW,
	WFF_SEDNA_SOFT_FAULT
};
	

//TODO: make this less ugly & remove old print stack trace stuff
#ifdef _WIN32
static void WriteFaultFile(WriteFaultFileKind kind, LPEXCEPTION_POINTERS exceptPtrs)
{
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
	tr_globals::ppc->WriteStackTraceFile(exceptPtrs);
#endif
}
#else

static void WriteFaultFile(WriteFaultFileKind kind, ...)
{
}
#endif

typedef struct AbortSourceInfo_tag_
{
	const char *file;
	const char *function;
	int line;
}
AbortSourceInfo;

/* abort due to unrecoverable exception - call from exception filter/signal handler ONLY! */ 
void AbortUnrecoverableException()
{
}

/* just abort */ 
void AbortCriticalError(const AbortSourceInfo *sourceInfo, const char *message)
{
}

/* abort due to failed assertion */ 
void AbortAssertFailed(const AbortSourceInfo *sourceInfo, const char *message)
{
}

#ifdef _WIN32
#include <windows.h>

static DWORD WinExceptFilter(DWORD exceptCode,
							 LPEXCEPTION_POINTERS exceptPtrs);

int main(int argc, char **argv)
{
	int exitCode=-1;
	__try
	{
		exitCode = TRmain(argc, argv);
	}
	__except(WinExceptFilter(GetExceptionCode(), GetExceptionInformation()))
	{
		/* will never get here normally */ 
		TerminateProcess(GetCurrentProcess(), exitCode);
	}
	return exitCode;
}

DWORD WinExceptFilter(DWORD exceptCode,
					  LPEXCEPTION_POINTERS exceptPtrs)
{
	void *hitAddr = NULL;
	DWORD resolution = 0;
	switch(exceptCode)
	{
		case EXCEPTION_ACCESS_VIOLATION:
			/* check if we can survive from A/V, if not die horribly */ 
			hitAddr = (void *)(exceptPtrs->ExceptionRecord->ExceptionInformation[1]);
			if (IsAccessViolationNonFatal(hitAddr, exceptPtrs->ContextRecord))
			{
				resolution = EXCEPTION_CONTINUE_EXECUTION;
			}
			else 
			{
				/*	A/V is fatal:
					write fault file(if enabled), and act according to abnormal termination policy 
					temination routine can return control if termination policy is 'Native',
					in the later case exception filter should return EXCEPTION_CONTINUE_SEARCH
					to let the remaining exception handlers to be called and the standard
					'Program is Fucked Up' dialog to pop up */ 
				WriteFaultFile(WFF_ACCESS_VIOLATION, exceptPtrs);
				AbortUnrecoverableException();
				resolution = EXCEPTION_CONTINUE_SEARCH;
			}
			break;

		case EXCEPTION_STACK_OVERFLOW:
		case EXCEPTION_INT_DIVIDE_BY_ZERO:
			WriteFaultFile(WFF_STACK_OVERFLOW, exceptPtrs);
			AbortUnrecoverableException();
			resolution = EXCEPTION_CONTINUE_SEARCH;
			break;
		default:
			resolution = EXCEPTION_CONTINUE_SEARCH;
			break;
	}
	return resolution;
}

#else
#include <unistd.h>
#include <signal.h>
#include <sys/mman.h>
//#include <alloca.h>
#include <stdlib.h>
#include <string.h>

/* enable sigaltstack() - 
	note: stack overflow is not reported unless alt stack is enabled 
	note2: alt stack is thread-local setting hence current implementation
	provides alt stack for the main thread only */ 
//#define ENABLE_ALTSTACK

/* secure alt stack with guard pages to detect alt stack overflow */ 
#define ENABLE_ALTSTACK_GUARD_PAGES

/*	allocate alternative stack inside the primary thread stack 
	this is required for old LinuxThreads which stored thread descriptor
	at the bottom of the stack and applyed some math to the stack pointer
	to access the descriptor */ 
#define ENABLE_ALTSTACK_INSIDE_PRIMARY_STACK

/* the alternative stack size for SIGSEGV processing - should be SMALL! */ 
#define ALTSTACK_SZ		0xD000

void unix_sigsegv_signal_handler(int signo, siginfo_t *info, void *cxt);

static void UnixSegvSignalHandler(int sigNum, siginfo_t *sigInfo, void *sigCtx)
{
	unix_sigsegv_signal_handler(sigNum, sigInfo, sigCtx);
}

int main(int argc, char **argv)
{
	int exitCode = -1;
	void *altStackMem = NULL;
	stack_t altStackNew, altStackOld;
	int pageSize = 0;
	int altStackPagesNum = 0;
	int altStackPagesTotal = 0;
	int altStackPagesOffset = 0;
	struct sigaction segvSigActionNew, segvSigActionOld;

	pageSize = getpagesize();
#ifdef ENABLE_ALTSTACK
	altStackPagesNum = ROUND_SIZE_UP(ALTSTACK_SZ, pageSize) / pageSize;
#ifdef ENABLE_ALTSTACK_GUARD_PAGES
	altStackPagesTotal = altStackPagesNum+2;
	altStackPagesOffset = 1;
#else
	altStackPagesTotal = altStackPagesNum;
	altStackPagesOffset = 0;
#endif
#ifdef ENABLE_ALTSTACK_INSIDE_PRIMARY_STACK
	/*	allocate alt stack mem with alloca and align pointer on page boundary
		the extra page is for alignment 
		memset is to ensure no stack overflow occured due to alloca() */ 
	altStackMem = alloca(pageSize * (1+altStackPagesTotal));
	altStackMem = ALIGN_PTR(altStackMem, pageSize);
	memset(altStackMem, 0, pageSize * altStackPagesTotal);
#else
	/* allocate alt stack mem with mmap, the resulting address is already aligned */ 
	altStackMem = mmap(NULL, pageSize * altStackPagesTotal, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (altStackMem == MAP_FAILED)
	{
		perror("mmap");
		abort();
	}
#endif
#ifdef ENABLE_ALTSTACK_GUARD_PAGES
	/* create guard pages on top and on the bottom of alt stack */ 
	if (0 != mprotect(altStackMem, pageSize, PROT_NONE) ||
		0 != mprotect(OFFSET_PTR(altStackMem, pageSize*(1+altStackPagesNum)), pageSize, PROT_NONE))
	{
		perror("mprotect");
		abort();
	}
#endif 
	/* install alt stack */ 
	altStackNew.ss_sp = OFFSET_PTR(altStackMem, pageSize * altStackPagesOffset);
	altStackNew.ss_size = pageSize * altStackPagesNum;
	altStackNew.ss_flags = 0;
	if (0 != sigaltstack(&altStackNew, &altStackOld))
	{
		perror("sigaltstack");
		abort();
	}
#endif /* ENABLE_ALTSTACK */ 

	/* install SEGV signal handler */ 
	segvSigActionNew.sa_sigaction = UnixSegvSignalHandler;
	segvSigActionNew.sa_flags = SA_ONSTACK|SA_SIGINFO;
	sigemptyset(&segvSigActionNew.sa_mask);
	if (0 != sigaction(SIGSEGV, &segvSigActionNew, &segvSigActionOld))
	{
		perror("sigaction");
		abort();
	}

	/*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
          so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
	sigignore(SIGPIPE);
#endif

	/* THE REAL MAIN CALLED */ 
	exitCode = TRmain(argc, argv);

	/* restore SEGV signal handler */ 
	if (0 != sigaction(SIGSEGV, &segvSigActionOld, NULL))
	{
		perror("sigaction");
		abort(); 
	}

#ifdef ENABLE_ALTSTACK
	/* remove alt stack */ 
	if (0 != sigaltstack(&altStackOld, NULL))
	{
		perror("sigaltstack");
		abort();
	}
#ifdef ENABLE_ALTSTACK_GUARD_PAGES
	/* destroy guard pages */ 
	if (0 != mprotect(altStackMem, pageSize, PROT_READ|PROT_WRITE) ||
		0 != mprotect(OFFSET_PTR(altStackMem, pageSize*(1+altStackPagesNum)), pageSize, PROT_READ|PROT_WRITE))
	{
		perror("mprotect");
		abort();
	}
#endif
	/* reclaim memory */ 
#ifndef ENABLE_ALTSTACK_INSIDE_PRIMARY_STACK
	if (0 != munmap(altStackMem,  pageSize * altStackPagesTotal))
	{
		perror("munmap");
		abort();
	}
#endif
	altStackMem = NULL;
#endif /* ENABLE_SIGALTSTACK */ 

	return exitCode;
}

#endif

#include "common/sedna.h"
#include "tr/vmm/os_exceptions.h"
#include "tr/vmm/vmm.h"
#include "common/sm_vmm_data.h"


#define PRINT_STACK_TRACE

#ifdef PRINT_STACK_TRACE
#undef PRINT_STACK_TRACE
#endif

bool OS_exceptions_handler::critical_section = false;

#ifdef _WIN32

#ifdef PRINT_STACK_TRACE
#include "tr/vmm/sym_engine.h"
#endif

#include <iostream>
#include <assert.h>

///////////////////////////////////////////////////////////////////////////////
/// WIN32 SECTION
///////////////////////////////////////////////////////////////////////////////

/*******************************************************************************
 * Win32 Exceptions
 * These exceptions are used for dealing with structured (system) exceptions
 * in programs written in C++. The idea is the following. We write a handler
 * that replaces the default handler and in the case of structured exception
 * it rases an ordinary C++ exception that can be handled by try/catch block.
 * We use this approach for handling 'access violation' exceptions and as the
 * result, helps us successfully implement CHECKP macros.
 ******************************************************************************/


static void win32_exception_translate(unsigned code, EXCEPTION_POINTERS* info)
{
    switch (code)
    {
        case EXCEPTION_ACCESS_VIOLATION	: 
            {
                bool var1 = LAYER_ADDRESS_SPACE_START_ADDR_INT <= (int)(info->ExceptionRecord->ExceptionInformation[1]);
                bool var2 = (int)(info->ExceptionRecord->ExceptionInformation[1]) < LAYER_ADDRESS_SPACE_BOUNDARY_INT;
                bool var3 = (int)(info->ExceptionRecord->ExceptionInformation[0]) == 1;
                if (var1 && var2 && var3)
                {
#ifdef PRINT_STACK_TRACE
           			sym_engine::stack_trace(std::cout, info->ContextRecord);
#endif
                    throw SYSTEM_EXCEPTION("Memory access violation error.");
//                    throw win32_access_violation();
                }
                else
				{
#ifdef PRINT_STACK_TRACE
                			sym_engine::stack_trace(std::cout, info->ContextRecord);
#endif
					throw SYSTEM_EXCEPTION("Access violation");
				}
            }
        case STATUS_STACK_OVERFLOW		: 
            {
#ifdef PRINT_STACK_TRACE
            	sym_engine::stack_trace(std::cout, info->ContextRecord);
#endif
                throw SYSTEM_EXCEPTION("Stack overflow");
            }
        default							:
            {
#ifdef PRINT_STACK_TRACE
             	sym_engine::stack_trace(std::cout, info->ContextRecord);
#endif
                throw SYSTEM_EXCEPTION("Unknown system error");
            }
    }
}

void OS_exceptions_handler::install_handler()
{
//    _set_se_translator(win32_exception_translate);
//	InstallKiUserExceptionDispatcherHook(RootExceptionDispatcher);
//	exceptionDispatcherProc = WorkerThreadExceptionDispatcher;
//	logfile = fopen("VMM_SIGNAL_MODIFICATION.log","at");
}

void OS_exceptions_handler::enter_stack_overflow_critical_section()
{
    critical_section = true;
}

void OS_exceptions_handler::leave_stack_overflow_critical_section()
{
    critical_section = false;
}

#else

///////////////////////////////////////////////////////////////////////////////
/// UNIX SECTION
///////////////////////////////////////////////////////////////////////////////

#ifdef PRINT_STACK_TRACE
#include <ucontext.h>
#include <dlfcn.h>
#include "common/errdbg/d_printf.h"
#endif

//jmp_buf stack_overflow_env;
jmp_buf access_violation_env;
jmp_buf vmm_is_busy_env;

void unix_sigsegv_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    //d_printf1("SEGV\n");fflush(stdout);

    U_ASSERT(false);

//    U_ASSERT(false);

    if (LAYER_ADDRESS_SPACE_START_ADDR_INT <= (int)(info->si_addr) && 
        (int)(info->si_addr) < LAYER_ADDRESS_SPACE_BOUNDARY_INT)
    { 
        // VMM page fault
//        if (vmm_is_busy_called) longjmp(vmm_is_busy_env, 0);
        vmm_unswap_block(vmm_cur_xptr);
    }
    else
    {
        // stack overflow or access violation
#ifdef PRINT_STACK_TRACE
        int
            f = 0;
        ucontext_t *
            ucontext = (ucontext_t *) cxt;
        Dl_info dlinfo;
        void **
            bp = 0;
        void *
            ip = 0;
        d_printf1("Segmentation fault!\n");
        d_printf2("   signo = %d\n", signo);
        d_printf2("   errno = %d\n", info->si_errno);
        d_printf2("   code  = %d\n", info->si_code);
        d_printf2("   addr  = %p\n", info->si_addr);

        ip = (void *) ucontext->uc_mcontext.gregs[REG_EIP];
        bp = (void **) ucontext->uc_mcontext.gregs[REG_EBP];

        d_printf1(" Stack trace:\n");
        while (bp && ip)
        {
            if (!dladdr(ip, &dlinfo))
                break;
            d_printf3("  % 2d: %p", ++f, ip);
            if (dlinfo.dli_sname)
                d_printf3("<%s+%u>", dlinfo.dli_sname, (unsigned) ((char *) ip - (char *) dlinfo.dli_saddr));
            d_printf2("(%s)", dlinfo.dli_fname);

            d_printf1("\n");
            if (dlinfo.dli_sname && !strcmp(dlinfo.dli_sname, "main"))
                break;
            ip = bp[1];
            bp = (void **) bp[0];
        }
        d_printf1(" End of stack trace.\n");
#endif
/*      
        if (OS_exceptions_handler::is_in_stack_overflow_critical_section())
            longjmp(stack_overflow_env, 0);
        else 
*/
            longjmp(access_violation_env, 0);
    }
}

void OS_exceptions_handler::install_handler()
{
/// This part of the code has been commented because it had been found that
/// alternative stack doesn't work properly with linux threads !

/*
    stack_t ss;
    ss.ss_sp = se_new char[1024 * 1024 * 2]; // SIGSTKSZ
    ss.ss_size = 1024 * 1024 * 2; // SIGSTKSZ
    ss.ss_flags = 0;
    if (sigaltstack(&ss, NULL) == -1)
        throw USER_EXCEPTION(SE1032);
*/

/// Use OS_EXCEPTIONS_INSTALL_HANDLER macro instead !

/*  
    struct sigaction sigsegv_act;
    memset(&sigsegv_act, '\0', sizeof(struct sigaction));
    sigsegv_act.sa_sigaction = unix_sigsegv_signal_handler;
    sigsegv_act.sa_flags = SA_SIGINFO  // | SA_ONSTACK;
    if (sigaction(SIGSEGV, &sigsegv_act, NULL) == -1) throw USER_EXCEPTION(SE1033);
    if (setjmp(access_violation_env) != 0) throw SYSTEM_EXCEPTION("Access violation or stack overflow");
*/

/// Nothing doing here
}

void OS_exceptions_handler::enter_stack_overflow_critical_section()
{
    critical_section = true;

//  if (setjmp(stack_overflow_env) != 0)
//      throw USER_EXCEPTION(SE1001);
}

void OS_exceptions_handler::leave_stack_overflow_critical_section()
{
    critical_section = false;
}

#endif
