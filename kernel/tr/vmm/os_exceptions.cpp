/*
 * File:  exceptions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/os_exceptions.h"
#include "tr/vmm/vmm.h"
#include "common/sm_vmm_data.h"
#include "exndisphook.h"

#define PRINT_STACK_TRACE


bool OS_exceptions_handler::critical_section = false;

#ifdef _WIN32
#ifdef PRINT_STACK_TRACE
#include "tr/vmm/sym_engine.h"
#include <iostream>
#include <assert.h>

//#include "../libs/sym_engine/sym_engine.h"
#endif

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

const char base[] = __FILE__;
int strip = 3;

void WriteStackTrace(PCONTEXT context, FILE *file)
{
	int success = 0, i = 0;
	char fileName[MAX_PATH] = "<unknown file>", procName[256] = "<unknown proc>", *fileNamePtr=fileName;
	const char *j = NULL;
	unsigned lineNo = 0, displ = 0;
	size_t len = 0;
	CONTEXT context2 = {};
	sym_engine sym(0);

	assert(context);
	context2=*context;
	if (!sym.stack_first(&context2)) {}
	else
	{
		success = 1;
		sym.fileline(fileName, sizeof(fileName), &lineNo, &displ);
		sym.symbol(procName, sizeof(procName), &displ);
		len=strlen(procName);
		if (len>=2 && 0==strcmp(procName+len-2,"()")) procName[len-2]='\0';
		i=strip+1;
		j=base+strlen(base);
		while(j>base)
		{			
			if (j[-1]=='\\') --i;
			if (i<=0) break;
			--j;
		}
		if (strncmp(base,fileName,j-base)==0)
		{
			fileNamePtr=fileName+(j-base);
		}
	}

	if (!success)
	{
		fprintf(file,"No file-line-function info availible, breaking into debugger.\n");
		DebugBreak();
	}
	else
	{
		fprintf(file,"%-30s %4d %s\n",procName,lineNo,fileNamePtr);
	}
	fflush(file);
}

static FILE *logfile = NULL;

static LONG NTAPI NullExceptionDispatcher(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context)
{
	return EXCEPTION_CONTINUE_SEARCH;
}

static __declspec(thread) UserExceptionDispatcherProc exceptionDispatcherProc = NullExceptionDispatcher;

static LONG NTAPI WorkerThreadExceptionDispatcher(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context)
{
	PVOID hit = NULL;
	int isW = 0;
	LONG resolution = EXCEPTION_CONTINUE_SEARCH;
	/* if we are invoked recursively we just give up*/ 
	exceptionDispatcherProc = NullExceptionDispatcher;

	if (ExceptionRecord->ExceptionCode != EXCEPTION_ACCESS_VIOLATION) 
	{
		/* not the type of exception we are interested in */ 
	}
	else
	{
		hit = (PVOID) ExceptionRecord->ExceptionInformation[1];
		isW = (int) ExceptionRecord->ExceptionInformation[0];
		if (isW && LAYER_ADDRESS_SPACE_START_ADDR <= hit && hit < LAYER_ADDRESS_SPACE_BOUNDARY)
		{
			WriteStackTrace(Context, logfile ? logfile : stderr);
			VMM_SIGNAL_MODIFICATION(ADDR2XPTR(hit));
			resolution = EXCEPTION_CONTINUE_EXECUTION;
		}
		else
		{
			/* TODO: Just a kludge for now, remove as soon as get VC2005 */ 
			U_ASSERT(0);
		}
	}

	exceptionDispatcherProc = WorkerThreadExceptionDispatcher;
	return resolution;
}

static LONG NTAPI RootExceptionDispatcher(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context)
{
	return exceptionDispatcherProc(ExceptionRecord,Context);
}

void OS_exceptions_handler::install_handler()
{
    //_set_se_translator(win32_exception_translate);
	InstallKiUserExceptionDispatcherHook(RootExceptionDispatcher);
	exceptionDispatcherProc = WorkerThreadExceptionDispatcher;
	logfile = fopen("VMM_SIGNAL_MODIFICATION.log","at");
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

    if (LAYER_ADDRESS_SPACE_START_ADDR_INT <= (int)(info->si_addr) && 
        (int)(info->si_addr) < LAYER_ADDRESS_SPACE_BOUNDARY_INT)
    { 
        // VMM page fault
        if (vmm_is_busy_called) longjmp(vmm_is_busy_env, 0);
#ifdef VMM_UNIX_LIGHT_CHECKP
        else vmm_unswap_block(xptr(0, info->si_addr));
#else
        else vmm_unswap_block(vmm_cur_xptr);
#endif
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

