/*
 * File:  os_exceptions.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */


#include "tr/vmm/os_exceptions.h"
#include "common/errdbg/d_printf.h"
#include "common/xptr/sm_vmm_data.h"
#include "tr/vmm/vmm.h"
#include "tr/tr_globals.h"


///////////////////////////////////////////////////////////////////////////////
/// COMMON SECTION
///////////////////////////////////////////////////////////////////////////////
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


///////////////////////////////////////////////////////////////////////////////
/// WINDOWS SECTION
///////////////////////////////////////////////////////////////////////////////
#ifdef _WIN32

static void WriteFaultFile(WriteFaultFileKind kind,
                           LPEXCEPTION_POINTERS exceptPtrs)
{
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    tr_globals::ppc->WriteStackTraceFile(exceptPtrs);
#endif
}

static DWORD WinExceptFilter(DWORD exceptCode,
                             LPEXCEPTION_POINTERS exceptPtrs);

/* Abort due to unrecoverable exception
 * call from exception filter/signal handler! */ 
static void AbortUnrecoverableException()
{
}

int main(int argc, char **argv)
{
    int exitCode=-1;
    __try
    {
        exitCode = TRmain(argc, argv);
    }
    __except(WinExceptFilter(GetExceptionCode(), GetExceptionInformation()))
    {
        /* Will never get here normally */ 
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
        /* Check if we can survive from A/V, if not die horribly */ 
        hitAddr = (void *)(exceptPtrs->ExceptionRecord->ExceptionInformation[1]);
        if (IsAccessViolationNonFatal(hitAddr, exceptPtrs->ContextRecord))
        {
            resolution = EXCEPTION_CONTINUE_EXECUTION;
        }
        else 
        {
            /*	
             * A/V is fatal: write fault file(if enabled), and act according
             * to abnormal termination policy temination routine can return
             * control if termination policy is 'Native', in the later case
             * exception filter should return EXCEPTION_CONTINUE_SEARCH to let
             * the remaining exception handlers to be called and the standard
             * 'Program is Fucked Up' dialog to pop up 
             */ 
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


///////////////////////////////////////////////////////////////////////////////
/// UNIX SECTION
///////////////////////////////////////////////////////////////////////////////
#else /* !_WIN32 */
jmp_buf access_violation_env;

void unix_sigsegv_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    U_ASSERT(false);

    if (LAYER_ADDRESS_SPACE_START_ADDR_INT <= (uintptr_t)(info->si_addr) &&
        (uintptr_t)(info->si_addr) < LAYER_ADDRESS_SPACE_BOUNDARY_INT)
    { 
        /* VMM page accessed */
        vmm_unswap_block(vmm_cur_xptr);
    }
    else
    {
        /* Stack overflow or access violation */
        longjmp(access_violation_env, 0);
    }
}

int main(int argc, char **argv)
{
    int exitCode = -1;
    
    /* 
     * Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    /* The real main call */ 
    exitCode = TRmain(argc, argv);

    return exitCode;
}

#endif /* _WIN32 */
