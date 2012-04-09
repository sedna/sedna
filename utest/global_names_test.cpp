#include "common/globalobjects/sednaregistry.h"
#include "u/ushm.h"
#include <stdio.h>

int test1() {
    GlobalObjectsCollector collector;

    global_name buf_mem_global = createSednaGlobalName(GLOBAL_NAME(BUFFER_MEMORY_SHM));
    UShMem shmem;

    uCreateShMem(&shmem, buf_mem_global, 1024, NULL, __sys_call_error_nop);

    collector.cleanup();

    return 0;
};

int main() {
    initSednaGlobalNameRegistry(1200, 0, 0);

    test1();

    releaseSednaGlobalNameRegistry();

    printf("OK");

    return 0;
};