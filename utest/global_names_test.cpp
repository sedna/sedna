#include "common/globalobjects/globalnames.h"
#include "u/ugnames.h"
#include "u/ushm.h"
#include "u/usem.h"

#include <stdio.h>
#include <errno.h>

static
const char * sedna_base = "/tmp";

static
global_name buffer_memory_shm = "buffer_memory_shm";

static
void test_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
    fprintf(stderr, "Error at %s : %s; @%s:%d %s\n", sys_call, strerror(errno), filename, lineno, funcname);
//    get_current_dir_name();
};

int test1()
{
    UShMem shmem;

    printf("Creating shared memory\n");

    uCreateShMem(&shmem, buffer_memory_shm, 1024, NULL, test_error);
    uOpenShMem(&shmem, buffer_memory_shm, test_error);
    uReleaseShMem(&shmem, buffer_memory_shm, test_error);

    printf("OK\nCreating shared memory once more\n");

    uCreateShMem(&shmem, buffer_memory_shm, 1024, NULL, test_error);

    printf("Waiting to be killed\nZZZ>>>");
    fflush(stdout);

    sleep(10);

    printf("\nPassed\n");

    return 0;
};

int test2()
{
    USemaphoreArr sem;
    int init[16] = {};

    printf("Creating sem array\n");

    USemaphoreArrCreate(&sem, 16, init, "sem_array_test", NULL, test_error);
    USemaphoreArrOpen(&sem, 16, "sem_array_test", test_error);
    USemaphoreArrRelease(sem, 16, test_error);

    printf("OK\nCreating sem array once more\n");

    USemaphoreArrCreate(&sem, 16, init, "sem_array_test", NULL, test_error);

    printf("Waiting to be killed\nZZZ>>>");
    fflush(stdout);

    sleep(10);

    printf("\nPassed\n");

    return 0;
};

int main() {
    GlobalObjectsCollector collector(sedna_base);
    uSetGlobalNameGeneratorBase(sedna_base, "0");

    test2();

    collector.cleanup();

    return 0;
};
