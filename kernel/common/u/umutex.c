/*
 * File:  umutex.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _WIN32
/* need this to enable UNIX98 features like pthread_mutexattr_settype from pthread.h */ 
#define _GNU_SOURCE
#endif
#include "common/u/umutex.h"

int uMutexInit(uMutexType *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    InitializeCriticalSection(mutex);
    return 0;
#else
    pthread_mutexattr_t attr;
    int res = pthread_mutexattr_init(&attr);
    if (res != 0) 
    {
      sys_call_error("pthread_mutexattr_init");  
      return res;
    }

    if (res = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE), res!=0)
    {
            sys_call_error("pthread_mutexattr_settype");
            return res;
    }

    if ((res = pthread_mutex_init(mutex, &attr)) != 0)
       sys_call_error("pthread_mutex_init");
    return res;
#endif
}

int uMutexLock(uMutexType *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    EnterCriticalSection(mutex);
    return 0;
#else
    int res;
    if ((res = pthread_mutex_lock(mutex)) != 0)
       sys_call_error("pthread_mutex_lock");
    return res;
#endif
}

int uMutexUnlock(uMutexType *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    LeaveCriticalSection(mutex);
    return 0;
#else
    int res;
    if ((res = pthread_mutex_unlock(mutex)) != 0)
       sys_call_error("pthread_mutex_unlock");

    return res;
#endif
}

int uMutexDestroy(uMutexType *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    DeleteCriticalSection(mutex);
    return 0;
#else
    int res;
    if ((res = pthread_mutex_destroy(mutex)) != 0)
       sys_call_error("pthread_mutex_destroy");

    return res;
#endif
}


int uCondInit(uCondType *event, sys_call_error_fun fun)
{
#ifdef _WIN32
#else
    int res;

    if ((res = pthread_cond_init(event, NULL)) != 0)
       sys_call_error("pthread_cond_init");

    return res;
#endif
};

int uCondSignal(uCondType *event, sys_call_error_fun fun)
{
#ifdef _WIN32
  U_ASSERT(false);
#else
    int res;

    if ((res = pthread_cond_signal(event)) != 0)
       sys_call_error("pthread_cond_signal");

    return res;
#endif
};

int uCondWait(uCondType *event, uMutexType * mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
  U_ASSERT(false);
#else
    int res;

    if ((res = pthread_cond_wait(event, mutex)) != 0)
       sys_call_error("pthread_cond_wait");

    return res;
#endif
};

int uCondTimedWait(uCondType* event, uMutexType * mutex, unsigned int time, sys_call_error_fun fun)
{
#ifdef _WIN32
  U_ASSERT(false);
#else
    int res;
    struct timeval tv;
    struct timespec timeout;

    if (gettimeofday(&tv, NULL) == -1)
    {
        sys_call_error("gettimeofday");
        return 1;
    }

    timeout.tv_sec = tv.tv_sec + time;
    timeout.tv_nsec = tv.tv_usec * 1000;

    if ((res = pthread_cond_timedwait(event, mutex, &timeout)) != 0) {
       if (res != ETIMEDOUT) {
          sys_call_error("pthread_cond_timedwait");
       }
    }

    return res;
#endif
};

int uCondDestroy(uCondType *event, sys_call_error_fun fun)
{
#ifdef _WIN32
#else
    int res;

    if ((res = pthread_cond_destroy(event)) != 0)
       sys_call_error("pthread_cond_destroy");

    return res;
#endif
};


/*
int uMutex2Create(uMutex2Type *mutex, int inheritable, sys_call_error_fun fun)
{
#ifdef _WIN32
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = inheritable ? TRUE : FALSE;
    *mutex = CreateMutex(
                   &sa,
                   TRUE,	// initial owner
                   NULL		// object name
             );
    if (*mutex == NULL) return 1;
    return 0;
#else
#error The platform is unsupported now
#endif
}

int uMutex2Lock(uMutex2Type *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD res = WaitForSingleObject(
                    *mutex,        // handle to object
                    INFINITE   // time-out interval
                );

    if (res == WAIT_FAILED) return 1;
    return 0;
#else
#error The platform is unsupported now
#endif
}

int uMutex2Unlock(uMutex2Type *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = ReleaseMutex(
                      *mutex   // handle to mutex
               );

    if (res == 0) return 1;
    return 0;
#else
#error The platform is unsupported now
#endif
}

int uMutex2Destroy(uMutex2Type *mutex, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = CloseHandle(
                    *mutex   // handle to mutex
               );

    if (res == 0) return 1;
    return 0;
#else
#error The platform is unsupported now
#endif
}
*/
