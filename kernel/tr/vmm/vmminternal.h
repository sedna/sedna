/*
 * File:  vmminternal.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _VMM_INTERNAL_H
#define _VMM_INTERNAL_H

#include "common/sedna.h"
#include "common/base.h"
#include "u/ummap.h"
#include "common/xptr/sm_vmm_data.h"

#include "tr/vmm/vmm.h"

UShMem smMemoryMapping;

/* Is any block mapped on position */
inline static bool _vmm_is_address_busy(void * p) {
    return (XADDR(((vmm_sm_blk_hdr*)p)->p) != NULL);
}

enum vmm_map_protection_t {
    access_null = 0,
    access_readonly = 1,
    access_readwrite = 2,
    access_writeonly = 3
};

int _uvmm_map(void *addr, ramoffs offs, UShMem * mapping, enum vmm_map_protection_t p);
int _uvmm_unmap(void *addr);
int __vmm_check_region(lsize_t cur, void ** res_addr, lsize_t * segment_size,
        bool log, FILE * logfile);

void __vmm_set_sigusr_handler();

#ifndef _WIN32
void _vmm_signal_handler(int signo, siginfo_t *info, void *cxt);
#endif /* _WIN32 */


inline static void check_bounds(xptr p) {
    if (p.getOffs() >= LAYER_ADDRESS_SPACE_SIZE)
        throw USER_EXCEPTION(SE1036);
}

#endif /* _VMM_INTERNAL_H */
