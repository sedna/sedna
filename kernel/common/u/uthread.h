/*
 * File:  uthread.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef UTHREAD_H
#define UTHREAD_H

#include "u.h"
#include "usecurity.h"


#ifdef _WIN32
#else
#include <pthread.h>
#endif
  

#ifdef _WIN32
#define U_THREAD_PROC(ProcName, arg) DWORD WINAPI ProcName(LPVOID arg)
#else 
#define U_THREAD_PROC(ProcName, arg) void *       ProcName(void*  arg)
#endif

#define UEXITTHREAD_OK			0
#define UEXITTHREAD_FAIL		(-1)


#ifdef _WIN32
typedef HANDLE                 uResVal;
typedef LPVOID                 uArg;
typedef HANDLE                 UTHANDLE;
typedef SIZE_T                 uStackSize;
typedef LPTHREAD_START_ROUTINE uThreadProc;
typedef DWORD                  UTID;

typedef volatile LONG          uspinlock;
#else
typedef int                    uResVal;
typedef void*                  uArg;
typedef pthread_t              UTHANDLE;
typedef size_t                 uStackSize;
typedef void*                (*uThreadProc)(void*);
typedef int                    UTID;

#ifdef HAVE_SPINLOCKS
typedef pthread_spinlock_t     uspinlock;
#endif
#endif

uResVal uCreateThread(
    uThreadProc proc,
    uArg        arg,
    UTHANDLE    *id,
    uStackSize  size,
    USECURITY_ATTRIBUTES* sa
);

int uEnableSuspend();
int uSuspendThread(UTHANDLE id);
int uResumeThread(UTHANDLE id);
int uTerminateThread(UTHANDLE id);
int uCloseThreadHandle(UTHANDLE id);
int uThreadJoin(UTHANDLE id);
// use UEXITTHREAD_OK or UEXITTHREAD_FAIL as arguments to uExitThread
void uExitThread(int rc);
void uSleep(unsigned int secs);
UTHANDLE uGetCurrentThread();

#ifdef _WIN32
#define uSpinInit(sl)		((*(sl) = FALSE), 0)
#define uSpinDestroy(sl)	0
#define uSpinLock(sl)		while (InterlockedExchange(sl, TRUE) == TRUE) Sleep(0)
#define uSpinUnlock(sl)		InterlockedExchange(sl, FALSE)
#else
#define uSpinInit(sl)		pthread_spin_init(sl, 0)
#define uSpinDestroy(sl)	pthread_spin_destroy(sl)
#define uSpinLock(sl)		pthread_spin_lock(sl)
#define uSpinUnlock(sl)		pthread_spin_unlock(sl)
#endif

int uThreadBlockAllSignals();

#endif

