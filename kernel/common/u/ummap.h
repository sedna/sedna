/*
 * File:  ummap.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _UMMAP_H
#define _UMMAP_H

#include "common/u/uhdd.h"
#include "common/u/usecurity.h"

#ifdef _WIN32

#define U_INVALID_FILEMAPPING(m)			((m).map == NULL)

typedef struct {
    size_t size;
	HANDLE map;
    UFile fd;
} UMMap;

#else

#include <sys/mman.h>

#define U_INVALID_FILEMAPPING(m)			((m).map == -1)

typedef struct {
	int map;
    size_t size;
    int to_file;
} UMMap;

#endif

#ifdef __cplusplus
extern "C" {
#endif

// check the result by U_INVALID_FILEMAPPING macros
// pass U_INVALID_FD as fd if you want to create object in swap file
UMMap uCreateFileMapping(UFile fd, size_t size, const char* name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);
UMMap uOpenFileMapping(UFile fd, size_t size, const char *name, sys_call_error_fun fun);
/*
UMMap uCreateFileMapping(const char* file_name, UShareMode share, UFlag attr, global_name g_name);
UMMap uOpenFileMapping(const char* file_name, UShareMode share, UFlag attr, global_name g_name);
*/
// returns -1 in case of error
int   uReleaseFileMapping(UMMap m, const char *name, sys_call_error_fun fun);
int   uCloseFileMapping(UMMap m, sys_call_error_fun fun);

// returns 0 in case of error
void *uMapViewOfFile(UMMap m, void *addr, size_t size, uint64_t offs, sys_call_error_fun fun);

// returns -1 in case of error
int uUnmapViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun);
int uFlushViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun);

int uMemLock(void *addr, size_t size, sys_call_error_fun fun);
int uMemUnlock(void *addr, size_t size, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif

