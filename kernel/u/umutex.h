/*
 * File:  umutex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef UMUTEX_H
#define UMUTEX_H

#include "u/u.h"

#ifdef __cplusplus
extern "C" {
#endif

//#if defined _WIN32
// WIN32 defined
//#elif defined POSIX
// POSIX defined
//#else
//#error Unknown platform, unknown threads...
//#endif

  
#ifdef _WIN32
typedef CRITICAL_SECTION	uMutexType;
#else
#include <pthread.h>

typedef pthread_mutex_t		uMutexType;
#endif

/*
#ifdef _WIN32
typedef HANDLE			uMutex2Type;
#else
typedef pthread_mutex_t		uMutex2Type;
#endif
*/

int uMutexInit(uMutexType *mutex, sys_call_error_fun fun);

int uMutexLock(uMutexType *mutex, sys_call_error_fun fun);

int uMutexUnlock(uMutexType *mutex, sys_call_error_fun fun);

int uMutexDestroy(uMutexType *mutex, sys_call_error_fun fun);


/*
int uMutex2Create(uMutex2Type *mutex, int inheritable, sys_call_error_fun fun);

int uMutex2Lock(uMutex2Type *mutex, sys_call_error_fun fun);

int uMutex2Unlock(uMutex2Type *mutex, sys_call_error_fun fun);

int uMutex2Destroy(uMutex2Type *mutex, sys_call_error_fun fun);
*/
#ifdef __cplusplus
}
#endif


#endif

