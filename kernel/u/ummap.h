/*
 * File:  ummap.h -- File Mapping Interface
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * File mapping interface. This interface implements file-mapping with _real_ files.
 * If you need to work with shared memory use interface provided by ushm.h instead.
 *
 * On Windows, it is possible to pass INVALID_HANDLE as an fd to create shared
 * memory mappings. This is not recommended and may be subject to further changes.
 *
 */


#ifndef _UMMAP_H
#define _UMMAP_H

#include "u/uhdd.h"
#include "u/usecurity.h"

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

/* map: id of real file
 * size: size of mapping
 */
typedef struct {
	UFile fd;
    size_t size;
} UMMap;

#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * These functions create-open file mappings. Parameters are self-evident.
 *   name -- global name of the object. Should be Sedna global name.
 *
 * Returns:
 *     UMMap structure,which you should check with U_INVALID_FILEMAPPING macros.
 */
UMMap uCreateFileMapping(UFile fd, size_t size, const char* name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun);
UMMap uOpenFileMapping(UFile fd, const char *name, sys_call_error_fun fun);

/*
 * Close-release file mappings. Parameters are self-evident.
 *
 * Returns:
 *     0 -- all ok
 *    -1 -- system error
 */
int   uReleaseFileMapping(UMMap m, const char *name, sys_call_error_fun fun);
int   uCloseFileMapping(UMMap m, sys_call_error_fun fun);

/*
 * map-unmam-flush views of file. Parameters are self-evident.
 * Returns:
 *     0 -- all ok
 *    -1 -- system error (except map returns NULL)
 */
void *uMapViewOfFile(UMMap m, void *addr, size_t size, uint64_t offs, sys_call_error_fun fun);
int uUnmapViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun);
int uFlushViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun);

/*
 * Lock-unlock virtual memory.
 * Returns:
 *     0 -- all ok
 *    -1 -- system error (non-fatal)
 */
int uMemLock(void *addr, size_t size, sys_call_error_fun fun);
int uMemUnlock(void *addr, size_t size, sys_call_error_fun fun);

#ifdef __cplusplus
}
#endif

#endif /* _UMMAP_H */
