/*
 * File:  umutex.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "umutex.h"


int uMutexInit(uMutexType *mutex)
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

    if ((res = pthread_mutex_init(mutex, &attr)) != 0)
       sys_call_error("pthread_mutex_init");
    return res;
#endif
}

int uMutexLock(uMutexType *mutex)
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

int uMutexUnlock(uMutexType *mutex)
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

int uMutexDestroy(uMutexType *mutex)
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





/*
int uMutex2Create(uMutex2Type *mutex, int inheritable)
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

int uMutex2Lock(uMutex2Type *mutex)
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

int uMutex2Unlock(uMutex2Type *mutex)
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

int uMutex2Destroy(uMutex2Type *mutex)
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
