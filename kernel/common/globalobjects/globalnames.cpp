#include "globalnames.h"

#include "u/usem.h"
#include "common/errdbg/event_log.h"

#include <map>
#include <string>

struct GlobalObjectsCollectorImplementation {
    std::map<std::string, GlobalObjectDescriptor *> objects;
};

class SemaphoreDescriptor : public GlobalObjectDescriptor {
    USemaphore sem;
public:
    SemaphoreDescriptor(global_name name, USemaphore _sem)
      : GlobalObjectDescriptor(name), sem(_sem) {};
    virtual void cleanup();
};

void SemaphoreDescriptor::cleanup()
{
    if (USemaphoreOpen(&sem, name, __sys_call_error_nop) == 0) {
        if (USemaphoreRelease(sem, __sys_call_error) == 0) {
            elog(EL_ERROR, ("Failed to cleanup semapore %s", name));
//            throw SYSTEM_EXCEPTION();
        };
    }
}



#define EVENT_CLEANUP(name)                                             if (UEventOpen(&ev, name, __sys_call_error) == 0)                                               \
                                                {                                                                                                                                                       \
                                                    UEventCloseAndUnlink(&ev, __sys_call_error);                                                                \
                                                    d_printf1("Event cleanup    : "#name"\n");                                                  \
                                                }

#define SEMAPHORE_CLEANUP(name)

#define SHAREDMEM_CLEANUP(name)                         if (uOpenShMem(&shm, name, __sys_call_error) == 0)                                              \
                                                {                                                                                                                                                       \
                                                    uReleaseShMem(&shm, name, __sys_call_error);                                                                        \
                                                    d_printf1("Shared memory cleanup: "#name"\n");                                                      \
                                                 }

#define SEMAPHORE_CLEANUP2(name, name2)                 if (USemaphoreOpen(&sem, name, __sys_call_error) == 0)                                          \
                                                {                                                                                                                                                       \
                                                    USemaphoreRelease(sem, __sys_call_error);                                                           \
                                                    d_printf1("Semaphore cleanup    : "#name2"\n");                                                     \
                                                }

#define FILE_MAPPING_CLEANUP(name)                              if (!U_INVALID_FILEMAPPING(map = uOpenFileMapping(U_INVALID_FD, 0, name, __sys_call_error)))    \
                                                {                                                                                                                                                       \
                                                    uReleaseFileMapping(map, name, __sys_call_error);                                           \
                                                    d_printf1("Filemapping cleanup    : "#name"\n");                                            \
                                                }



GlobalObjectDescriptor * createSemDescriptor();
GlobalObjectDescriptor * createShmDescriptor();

