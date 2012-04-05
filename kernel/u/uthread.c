/*
 * File:  uthread.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/errdbg/d_printf.h"
#include "u/uthread.h"


uResVal uCreateThread(
  uThreadProc proc,
  uArg        arg,
  UTHANDLE    *id,
  uStackSize  size,
  USECURITY_ATTRIBUTES* sa,
  sys_call_error_fun fun
)
{
#ifdef _WIN32
    DWORD threadId = 0;
    HANDLE hndl = CreateThread(
                      sa,
                      size,
                      proc,
                      arg,
                      0,
                      &threadId);
    *id = hndl;
    if (hndl != 0) return 0;
    else
    {
       sys_call_error("CreateThread");
       return (void*)-1; 
    }
#else
    pthread_attr_t attr;
    int set_res = 0;
    set_res = pthread_attr_init(&attr);
    if (set_res != 0)
    {  
       sys_call_error("pthread_attr_init");
       return set_res;
    }
    set_res = pthread_attr_setstacksize(&attr, size);
    if (set_res != 0) 
    {
       sys_call_error("pthread_attr_setstacksize"); 
       return set_res;
    }
    pthread_t threadId = 0;
    int     res = pthread_create(
                      &threadId,
                      &attr,
                      proc,
                      arg);

    if (res != 0) sys_call_error("pthread_create");

    *id = threadId;
    return res;
#endif
}

#ifdef _WIN32
#else
static void _suspend_thread_signal_handler(int signo, siginfo_t *info, void *cxt)
{
	sys_call_error_fun fun=NULL;
    d_printf1("suspend\n");
    int sig = 0;
    sigset_t signalSet;
    if (sigfillset(&signalSet) == -1) sys_call_error("sigfillset");
    if (sigwait(&signalSet, &sig) != 0) sys_call_error("sigwait");
}
static void _resume_thread_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    d_printf1("resume\n");
}
#endif

int uEnableSuspend(sys_call_error_fun fun)
{
#ifdef _WIN32
    return 0;
#else
    struct sigaction sigsegv_act;
                                                                                                                             
    memset(&sigsegv_act, '\0', sizeof(struct sigaction));
    sigsegv_act.sa_sigaction = _suspend_thread_signal_handler;
    sigsegv_act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGUSR1, &sigsegv_act, NULL) == -1)
    {
        sys_call_error("sigaction");
        return 1;
    }
                                                                                                                             
    memset(&sigsegv_act, '\0', sizeof(struct sigaction));
    sigsegv_act.sa_sigaction = _resume_thread_signal_handler;
    sigsegv_act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGUSR2, &sigsegv_act, NULL) == -1)
    {
        sys_call_error("sigaction");
        return 1;
    }

    return 0;
#endif
}

int uSuspendThread(UTHANDLE id, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (SuspendThread(id) == -1)
    {
       sys_call_error("SuspendThread");
       return 1;
    }
    else return 0;
#else
    int res = pthread_kill(id, SIGUSR1);
    if (res != 0) sys_call_error("pthread_kill");
    return res;
#endif
}

int uResumeThread(UTHANDLE id, sys_call_error_fun fun) 
{
#ifdef _WIN32
    if (ResumeThread(id) == -1)
    {
       sys_call_error("ResumeThread");
       return 1;
    }
    else return 0;
#else
    int res = pthread_kill(id, SIGUSR2); 
    if (res != 0) sys_call_error("pthread_kill");
    return res;
#endif
}

int uTerminateThread(UTHANDLE id, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = 0;
    res = TerminateThread(id, 0);
    if (res == 0)
    {
        sys_call_error("TerminateThread");
        return 1;
    }

    return 0;
#else
    int res = pthread_cancel(id);
    if (res != 0) sys_call_error("pthread_cancel");
    return res;
#endif
}

int uCloseThreadHandle(UTHANDLE id, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = 0;
    res = CloseHandle(id);
    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }

    return 0;
#else
    return 0;
#endif
}

int uThreadJoin(UTHANDLE id, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD res = 0;
    do {
        res = WaitForSingleObjectEx(id, INFINITE, TRUE);
    } while (res == WAIT_IO_COMPLETION);
    if (res == WAIT_FAILED) sys_call_error("WaitForSingleObject");

    if (res != WAIT_OBJECT_0) return 1;
    else return 0;
#else
    int res = pthread_join(id, NULL);
    if (res != 0) sys_call_error("pthread_join");
    return res;
#endif
}

void uExitThread(unsigned rc, sys_call_error_fun fun)
{
#ifdef _WIN32
	ExitThread(rc);
#else
	// safe to do double-cast here since we always interpret rc as return-code
    pthread_exit((void*)(uintptr_t)rc);
#endif
}

UTHANDLE uGetCurrentThread(sys_call_error_fun fun)
{
#ifdef _WIN32
	UTHANDLE result;

	if (DuplicateHandle(GetCurrentProcess(),
                        GetCurrentThread(),
                        GetCurrentProcess(),
                        &result,
                        0,
                        FALSE,
                        DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE) == 0)
       sys_call_error("DuplicateHandle");

	return result;
#else
    return pthread_self();
#endif
}


int uThreadBlockAllSignals(sys_call_error_fun fun)
{
#ifdef _WIN32
    return 0;
#else
    sigset_t new_sigset, old_sigset;
    if (sigfillset(&new_sigset) == -1)
    {
      sys_call_error("sigfillset");
      return -1;
    }
    if (pthread_sigmask(SIG_SETMASK, &new_sigset, &old_sigset) == -1)
    {
       sys_call_error("pthread_sigmask");
       return -1;
    }
    return 0;
#endif
}

/*
int uTime()
{
#ifdef _WIN32
    time_t t;
    time(&t);
    return t;
#else
    time_t t;
    return time(&t);
#endif
}
*/
