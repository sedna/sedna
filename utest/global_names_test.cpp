#define TEST_NAME "0001.globalNamesTest.xml"

#include "common/globalobjects/globalnames.h"
#include "u/ugnames.h"
#include "u/ushm.h"
#include "u/usem.h"
#include "gtest/gtest.h"

#include <stdio.h>
#include <errno.h>

static
const char * sedna_base = "/tmp";

static
global_name buffer_memory_shm = "buffer_memory_shm";

int errorCount = 0;

static
void test_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
    fprintf(stderr, "Error at %s : %s; @%s:%d %s\n", sys_call, strerror(errno), filename, lineno, funcname);
    fflush(stderr);
    errorCount++;
    ASSERT_FALSE(true);
//    get_current_dir_name();
};

TEST(objectsCreation, Shmem)
{
    UShMem shmem;

    printf("Creating shared memory\n");

    uCreateShMem(&shmem, buffer_memory_shm, 1024, NULL, test_error);
    uOpenShMem(&shmem, buffer_memory_shm, test_error);
    uReleaseShMem(&shmem, buffer_memory_shm, test_error);

    printf("OK\nCreating shared memory once more\n");

    uCreateShMem(&shmem, buffer_memory_shm, 1024, NULL, test_error);

    fflush(stdout);
};

TEST(objectsCreation, Semaphors)
{
    USemaphoreArr sem;
    int init[16] = {};

    printf("Creating sem array\n");

    USemaphoreArrCreate(&sem, 16, init, "sem_array_test", NULL, test_error);
    USemaphoreArrOpen(&sem, 16, "sem_array_test", test_error);
    USemaphoreArrRelease(sem, 16, test_error);

    printf("OK\nCreating sem array once more\n");

    USemaphoreArrCreate(&sem, 16, init, "sem_array_test", NULL, test_error);

    fflush(stdout);
};

int main(int argc, char** argv) {
    if(argc == 1) {
        std::string cmd;
#ifdef WIN32
        cmd.append("xml:reports\\");
#else
        cmd.append("xml:reports/");
#endif   
//         cmd.append(TEST_NAME);
         ::testing::GTEST_FLAG(output) = cmd.c_str();
//         ::testing::GTEST_FLAG(color) = "yes";
         ::testing::GTEST_FLAG(stack_trace_depth) = ::testing::kMaxStackTraceDepth;
    }
    ::testing::InitGoogleTest(&argc, argv);
    
    GlobalObjectsCollector collector(sedna_base);
    uSetGlobalNameGeneratorBase(sedna_base, "0");

    RUN_ALL_TESTS();

    collector.cleanup();

    return errorCount;
};
