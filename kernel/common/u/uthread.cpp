/*
 * File:  uthread.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "d_printf.h"
#include "uthread.h"

#ifdef _WIN32
#include <time.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <string.h>
#endif

uResVal uCreateThread(
  uThreadProc proc,
  uArg        arg,
  UTHANDLE    *id,
  uStackSize  size
)
{
#ifdef _WIN32
    DWORD threadId = 0;
    HANDLE hndl = CreateThread(
                      NULL,
                      size,
                      proc,
                      arg,
                      0,
                      &threadId);
    *id = hndl;
    if (hndl != 0) return 0;
    else return (void*)-1;
#else
    //printf("01\n");
    pthread_attr_t attr;
    int set_res = 0;
    set_res = pthread_attr_init(&attr);
    //printf("02\n");
    if (set_res != 0) return set_res;
    //printf("03\n");
    set_res = pthread_attr_setstacksize(&attr, size);
    //printf("04\n");
    if (set_res != 0) return set_res;
    //printf("05\n");
    pthread_t threadId = 0;
    int     res = pthread_create(
                      &threadId,
                      &attr,
                      proc,
                      arg);
    //printf("06\n");
    *id = threadId;
    return res;
#endif
}

#ifdef _WIN32
#else
static void _suspend_thread_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    d_printf1("suspend\n");
    int sig = 0;
    sigset_t signalSet;
    sigfillset(&signalSet);
    sigwait(&signalSet, &sig);
}
static void _resume_thread_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    d_printf1("resume\n");
}
#endif

int uEnableSuspend()
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
        d_perror("sigaction");
        return 1;
    }
                                                                                                                             
    memset(&sigsegv_act, '\0', sizeof(struct sigaction));
    sigsegv_act.sa_sigaction = _resume_thread_signal_handler;
    sigsegv_act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGUSR2, &sigsegv_act, NULL) == -1)
    {
        d_perror("sigaction");
        return 1;
    }

    return 0;
#endif
}

int uSuspendThread(UTHANDLE id)
{
#ifdef _WIN32
    return SuspendThread(id) == -1 ? 1 : 0;
#else
    return pthread_kill(id, SIGUSR1);
#endif
}

int uResumeThread(UTHANDLE id) 
{
#ifdef _WIN32
    return ResumeThread(id) == -1 ? 1 : 0;
#else
    uSleep(2);
    return pthread_kill(id, SIGUSR2);
#endif
}

int uTerminateThread(UTHANDLE id)
{
#ifdef _WIN32
    BOOL res = 0;
    res = TerminateThread(id, 0);
    if (res == 0)
    {
        d_printf1("TerminateThread failed\n");
        return 1;
    }

    return 0;
#else
    return pthread_cancel(id);
#endif
}

int uCloseThreadHandle(UTHANDLE id)
{
#ifdef _WIN32
    BOOL res = 0;
    res = CloseHandle(id);
    if (res == 0)
    {
        d_printf1("CloseHandle failed\n");
        return 1;
    }

    return 0;
#else
    return 0;
#endif
}

int uThreadJoin(UTHANDLE id)
{
#ifdef _WIN32
    if (WaitForSingleObject(id, INFINITE) != WAIT_OBJECT_0) return 1;
    else return 0;
#else
    return pthread_join(id, NULL);
#endif
}

void uExitThread(int rc) 
{
#ifdef _WIN32
	ExitThread(rc);
#else
    pthread_exit((void*)rc); 
#endif
}

void uSleep(unsigned int secs)
{
#ifdef _WIN32
    Sleep(secs * 1000);
#else
    sleep(secs);
#endif
}

UTHANDLE uGetCurrentThread()
{
#ifdef _WIN32
	UTHANDLE result;
	DuplicateHandle(GetCurrentProcess(), 
                    GetCurrentThread(), 
                    GetCurrentProcess(), 
                    &result, 
                    NULL, 
                    FALSE, 
                    DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE);
	return result;
#else
    return pthread_self();
#endif
}


int uThreadBlockAllSignals()
{
#ifdef _WIN32
    return 0;
#else
    sigset_t new_sigset, old_sigset;
    if (sigfillset(&new_sigset) == -1) return -1;
    if (pthread_sigmask(SIG_SETMASK, &new_sigset, &old_sigset) == -1) return -1;
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

