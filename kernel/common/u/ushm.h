/*
 * File:  ushm.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USHMEM_H
#define _USHMEM_H

#include "u.h"
#include "usecurity.h"

#ifdef _WIN32

typedef HANDLE UShMem;

#else

#include <sys/shm.h>

typedef int UShMem;

#endif



int uCreateShMem(UShMem *id, global_name name, int size, USECURITY_ATTRIBUTES* sa);

int uOpenShMem(UShMem *id, global_name key, int size);

int uReleaseShMem(UShMem id);

int uCloseShMem(UShMem id);

void* uAttachShMem(UShMem id, void *ptr, int size);

int uDettachShMem(UShMem id, void * ptr);



#endif

