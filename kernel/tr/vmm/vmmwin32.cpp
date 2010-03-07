/*
 * File:  vmmwin32.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifdef _WIN32

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/ummap.h"
#include "common/u/uthread.h"
#include "common/errdbg/d_printf.h"
#include "tr/vmm/vmminternal.h"

/* As far as we know Windows does not provide option to protect page from reading */

const static int map_to_win[4] = {FILE_MAP_READ, FILE_MAP_READ, FILE_MAP_ALL_ACCESS, FILE_MAP_ALL_ACCESS};

int _uvmm_map(void *addr, ramoffs offs, UMMap * mapping, enum vmm_map_protection_t p)
{
    HANDLE m = mapping->map;

#ifndef VMM_DEBUG_CHECKP
    p = access_readwrite;
#endif /* VMM_DEBUG_CHECKP */

    addr = MapViewOfFileEx(
        m,                      // handle to file-mapping object
        map_to_win[p],          // access mode
        0,                      // high-order DWORD of offset
        offs,                   // low-order DWORD of offset
        PAGE_SIZE,              // number of bytes to map
        addr                    // starting address
    );

    if (addr == NULL) {
        d_printf1("MapViewOfFileEx failed\n");
        d_printf3("Error %d; addr = 0x%x\n", GetLastError(), (int)(addr));

        return -1;
    }

    return 0;
}

int _uvmm_unmap(void *addr)
{
    return (UnmapViewOfFile(addr) == 0 ? -1 : 0);
}

int __vmm_check_region(uint32_t cur, void ** res_addr, uint32_t * segment_size, bool log, FILE * logfile)
{
    *res_addr = VirtualAlloc(
        NULL,                      // system determines where to allocate the region
        cur + (uint32_t)PAGE_SIZE, // additional PAGE is used to perform aligning afterwards
        MEM_RESERVE,               // type of allocation
        PAGE_READONLY);            // READONLY here is enough

    if (*res_addr) {
        if (log) fprintf(logfile, "PASSED\n");
        *segment_size = cur;
        VirtualFree(*res_addr, 0, MEM_RELEASE);
        *res_addr = (void*)(((uint32_t)*res_addr + (uint32_t)PAGE_SIZE) & PAGE_BIT_MASK);
        return 1;
    } else if(log)
        fprintf(logfile, "FAILED with error %d\n", GetLastError());
    return 0;
}

void __vmm_set_sigusr_handler() {}

#endif /* _WIN32 */
