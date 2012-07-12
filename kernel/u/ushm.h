/*
 * File:  ushm.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USHMEM_H
#define _USHMEM_H

#include "u/u.h"
#include "u/usecurity.h"

#ifdef _WIN32

typedef struct {
    size_t size;
    HANDLE id;
} UShMem;

#else

/*
 * size: size of mapping
 * id: id of shm_open file
 * to_guarantee: 1 -- we need to guarantee mapping later; 0 -- don't need to
 *
 * Typically we want to guarantee pure shmem allocation on create, but since
 * attach and create are separated it would be more efficient to make it on
 * attach (mmap). For the whole idea of 'guarantee' see comment above
 * _guarantee_buffer_pool function on SIGBUS hack.
 */
typedef struct {
    size_t size;
    int id;
    int to_guarantee;   /* 1, if this is a new shared memory segment; 0, if we open existent */
} UShMem;

#endif

#ifdef __cplusplus
extern "C" {
#endif

int uCreateShMem(UShMem *id, global_name gname, size_t size, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);

int uOpenShMem(UShMem *id, global_name gname, sys_call_error_fun fun);

int uReleaseShMem(UShMem *id, global_name gname, sys_call_error_fun fun);

int uCloseShMem(UShMem *id, sys_call_error_fun fun);

void* uAttachShMem(UShMem *id, void *addr, size_t size, sys_call_error_fun fun);

int uDettachShMem(UShMem *id, void * ptr, sys_call_error_fun fun);

/*
 * Guarantees that we've got shared segment of size 'size' by address 'addr'.
 * Primarily needed for Linux-like OSs that do overcommitment.
 *
 * Should be used with care since we don't check consistency for addr and size
 * values.
 *
 * Returns:
 *     0  -- all ok
 *     -1 -- cannot guarantee
 *     -2 -- some internal error (i.e. signal manipulations failed)
 */
int uGuaranteeSharedMemory(void *addr, size_t size, sys_call_error_fun fun);


#ifdef __cplusplus
}
#endif

#endif
