#ifndef _WIN32

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/ummap.h"
#include "common/u/uthread.h"
#include "common/errdbg/d_printf.h"
#include "tr/vmm/vmminternal.h"

const static int map_to_unix[4] = {PROT_NONE, PROT_READ, PROT_READ | PROT_WRITE, PROT_WRITE};

int _uvmm_map(void *addr, ramoffs offs, UMMap * mapping, enum vmm_map_protection_t p)
{
    int m = mapping->map;

#ifndef VMM_DEBUG_CHECKP
#ifndef VMM_LINUX_DEBUG_CHECKP
    p = access_readwrite;
#endif /* VMM_LINUX_DEBUG_CHECKP */
#endif /* VMM_DEBUG_CHECKP */

    addr = mmap(addr, PAGE_SIZE, map_to_unix[p], MAP_SHARED | MAP_FIXED, m, offs);

    if (addr == MAP_FAILED) {
        d_perror("mmap failed");
        d_printf2("Addr = 0x%x\n", (int)(addr));
        return -1;
    }

    return 0;
}

int _uvmm_unmap(void *addr)
{
    return munmap(addr, PAGE_SIZE);
}

int __vmm_check_region(uint32_t cur, void ** res_addr, uint32_t * segment_size, bool log, FILE * logfile)
{
    *res_addr = mmap(0, cur + (__uint32)PAGE_SIZE, PROT_READ, MAP_PRIVATE | U_MAP_NORESERVE | U_MAP_ANONYMOUS, -1, 0);

    if (*res_addr != MAP_FAILED) {
        if (log) fprintf(logfile, "PASSED\n");
        *segment_size = cur;
        munmap(*res_addr, cur);
        *res_addr = (void*)(((__uint32)*res_addr + (__uint32)PAGE_SIZE) & PAGE_BIT_MASK);
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
