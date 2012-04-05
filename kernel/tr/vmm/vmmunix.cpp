/*
 * File:  vmmunix.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _WIN32

#include "u/uthread.h"
#include "common/errdbg/d_printf.h"
#include "tr/vmm/vmminternal.h"

const static int map_to_unix[4] = {PROT_NONE, PROT_READ, PROT_READ | PROT_WRITE, PROT_WRITE};

int _uvmm_map(void *addr, ramoffs offs, UShMem * mapping, enum vmm_map_protection_t p)
{
    int m = mapping->id;

    /*
     * We need read access in any case, except linux debug checkp
     * which implements special handlers for CHECKP().
     * Even in debug checkp (don't mess it with linux debug checkp)
     * we read block in CHECP(p) to test layer.
     */

#ifndef VMM_LINUX_DEBUG_CHECKP
    p = access_readwrite;
#endif /* VMM_LINUX_DEBUG_CHECKP */

    void* res = mmap(addr, PAGE_SIZE, map_to_unix[p], MAP_SHARED | MAP_FIXED,
	                 m, (off_t)offs);

    /*
     * If implementation doesn't support mmap-over-mmap then try to unmap first
     * and mmap again.
     */
    if (res == MAP_FAILED && errno == EINVAL) {
        elog(EL_DBG, ("mmap-over-mmap failed on address = 0x%"PRIXPTR,
		              (uintptr_t)addr));
        munmap(addr, PAGE_SIZE);
        res = mmap(addr, PAGE_SIZE, map_to_unix[p], MAP_SHARED | MAP_FIXED, m,
                (off_t)offs);
    }

    if (res == MAP_FAILED) {
        elog(EL_DBG, ("mmap failed on address = 0x%"PRIXPTR, (uintptr_t)addr));
		d_perror("mmap failed");
        d_printf2("Addr = 0x%"PRIXPTR"\n", (uintptr_t)(addr));
        return -1;
    }

    return 0;
}

int _uvmm_unmap(void *addr)
{
    /* do nothing since we're using mmap-over-mmap */
    return 0;
}

int __vmm_check_region(lsize_t cur, void ** res_addr, lsize_t * segment_size,
        bool log, FILE * logfile)
{
    /* additional PAGE_SIZE needed for alignment of res_addr on page boundary */
    *res_addr = mmap(0, cur + (uint32_t)PAGE_SIZE, PROT_READ, MAP_PRIVATE | U_MAP_NORESERVE | U_MAP_ANONYMOUS, -1, 0);

    if (*res_addr != MAP_FAILED) {
        if (log) fprintf(logfile, "PASSED\n");
        *segment_size = cur;
        *res_addr = (void*)(((uintptr_t)*res_addr + (uint32_t)PAGE_SIZE) & PAGE_BIT_MASK);
        return 1;
    } else if(log) {
        fprintf(logfile, "FAILED with error: %s\n", strerror(errno));
    }

    return 0;
}

void __vmm_set_sigusr_handler()
{
    struct sigaction sig_act;

    memset(&sig_act, '\0', sizeof(struct sigaction));
    sig_act.sa_sigaction = _vmm_signal_handler;
    sig_act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGUSR1, &sig_act, NULL) == -1)
        throw USER_EXCEPTION(SE1033);
}

#endif /* _WIN32 */
