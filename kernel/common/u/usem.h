/*
 * File:  usem.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _USEM_H
#define _USEM_H

#include "u.h"

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

// Amount of new created semaphores. Only for Unix. 
#define SEM_AMOUNT 1 
#define WAIT_FR 1

#endif

// functions return 0 on success

int USemaphoreCreate(USemaphore *sem, int init_value, int max_value, global_name name);
int USemaphoreOpen(USemaphore *sem, global_name name);
int USemaphoreRelease(USemaphore sem);
int USemaphoreClose(USemaphore sem);
int USemaphoreDown(USemaphore sem);
// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int USemaphoreDown(USemaphore sem, unsigned int millisec);
int USemaphoreUp(USemaphore sem);



int USemaphoreArrCreate(USemaphoreArr *sem, int size, const int *init_values, global_name name);
int USemaphoreArrOpen(USemaphoreArr *sem, int size, global_name name);
int USemaphoreArrRelease(USemaphoreArr sem, int size);
int USemaphoreArrClose(USemaphoreArr sem, int size);
int USemaphoreArrDown(USemaphoreArr sem, int i);
int USemaphoreArrDown(USemaphoreArr sem, int i, unsigned int millisec);
int USemaphoreArrUp(USemaphoreArr sem, int i);



int UUnnamedSemaphoreCreate(UUnnamedSemaphore *sem, int init_value);
int UUnnamedSemaphoreRelease(UUnnamedSemaphore *sem);
int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem);
// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem, unsigned int millisec);
int UUnnamedSemaphoreUp(UUnnamedSemaphore *sem);


#endif


