#ifndef _LFS_GLOBALS_
#define _LFS_GLOBALS_

#include <stdint.h>
#include <stddef.h>

typedef uint64_t LSN;

#define LFS_INVALID_LSN  UINT64_C(0xFFFFFFFFFFFFFFFF)
#define LFS_INVALID_FILE UINT64_C(0xFFFFFFFFFFFFFFFF)

#endif
