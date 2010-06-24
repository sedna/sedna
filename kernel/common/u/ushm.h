/*
 * File:  ushm.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USHMEM_H
#define _USHMEM_H

#include "common/u/u.h"
#include "common/u/usecurity.h"

#ifdef _WIN32

typedef struct {
    size_t size;
    HANDLE id;
} UShMem;

#else

typedef struct {
    int id;
    size_t size;
} UShMem;

#endif

#ifdef __cplusplus
extern "C" {
#endif

int uCreateShMem(UShMem *id, global_name name, size_t size, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);

int uOpenShMem(UShMem *id, global_name key, size_t size, sys_call_error_fun fun);

int uReleaseShMem(UShMem id, global_name name, sys_call_error_fun fun);

int uCloseShMem(UShMem id, sys_call_error_fun fun);

void* uAttachShMem(UShMem id, void *ptr, size_t size, sys_call_error_fun fun);

int uDettachShMem(UShMem id, void * ptr, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif

