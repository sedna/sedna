/*
 * File:  usem.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _USEM_H
#define _USEM_H

#include "u/u.h"
#include "u/usecurity.h"

#ifdef _WIN32

typedef HANDLE USemaphore;        // named semaphore (interprocess semaphore)
typedef HANDLE *USemaphoreArr;    // named array of semaphores (interprocess semaphore)
typedef HANDLE UUnnamedSemaphore; // unnamed semaphore (intraprocess semaphore)

#else

#include <sys/sem.h>
#include <pthread.h>

typedef int USemaphore;           // named semaphore (interprocess semaphore)
typedef int USemaphoreArr;        // named array of semaphores (interprocess semaphore)
typedef struct {
    pthread_mutex_t  mutex;
    pthread_cond_t   condition;
    int              count;
} UUnnamedSemaphore;              // unnamed semaphore (intraprocess semaphore)


#endif


#ifdef __cplusplus
extern "C" {
#endif

// functions return 0 on success
// sa - Security Attributes for the new semaphore
int USemaphoreCreate(USemaphore* sem, int init_value, int max_value, global_name gname, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);
int USemaphoreOpen(USemaphore* sem, global_name gname, sys_call_error_fun fun);
int USemaphoreRelease(USemaphore sem, sys_call_error_fun fun);
int USemaphoreClose(USemaphore sem, sys_call_error_fun fun);
int USemaphoreDown(USemaphore sem, sys_call_error_fun fun);
// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int USemaphoreDownTimeout(USemaphore sem, unsigned int millisec, sys_call_error_fun fun);
int USemaphoreUp(USemaphore sem, sys_call_error_fun fun);


// sa - Security Attributes for the new semaphores
int USemaphoreArrCreate(USemaphoreArr* sem, unsigned int size, const int* init_values, global_name gname, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);
int USemaphoreArrOpen(USemaphoreArr* sem, unsigned int size, global_name gname, sys_call_error_fun fun);
int USemaphoreArrRelease(USemaphoreArr sem, unsigned size, sys_call_error_fun fun);
int USemaphoreArrClose(USemaphoreArr sem, unsigned size, sys_call_error_fun fun);
int USemaphoreArrDown(USemaphoreArr sem, unsigned i, sys_call_error_fun fun);
int USemaphoreArrDownTimeout(USemaphoreArr sem, unsigned i, unsigned int millisec, sys_call_error_fun fun);
int USemaphoreArrUp(USemaphoreArr sem, unsigned i, sys_call_error_fun fun);



// sa - Security Attributes for the new unnamed semaphore
int UUnnamedSemaphoreCreate(UUnnamedSemaphore *sem, int init_value, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);
int UUnnamedSemaphoreRelease(UUnnamedSemaphore *sem, sys_call_error_fun fun);
int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem, sys_call_error_fun fun);
// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int UUnnamedSemaphoreDownTimeout(UUnnamedSemaphore *sem, unsigned int millisec, sys_call_error_fun fun);
int UUnnamedSemaphoreUp(UUnnamedSemaphore *sem, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif


