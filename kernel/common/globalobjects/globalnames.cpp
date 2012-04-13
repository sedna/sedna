#include "globalnames.h"

#include "u/usem.h"
#include "u/ushm.h"
#include "u/uevent.h"
#include "u/uhdd.h"

#include "aux/cppcast.h"
#include "aux/counted_ptr.h"

#include "common/sedna.h"
#include "common/errdbg/event_log.h"
#include "common/errdbg/errors.h"

#include <string>
#include <map>


typedef std::map<std::string, GlobalObjectDescriptor *> GlobalObjectMap;
typedef std::map<std::string, GlobalObjectDescriptorFactory> GlobalObjectFactoryMap;

struct GlobalObjectsCollectorImplementation {
    UFile objectLogFile;
    GlobalObjectMap objects;
    GlobalObjectFactoryMap objFactoryMap;
};

static GlobalObjectsCollectorImplementation * gcimpl = NULL;
UGlobalGarbageCollector gogc = {};

void GlobalObjectsCollector::registerFactory(const char* objType, GlobalObjectDescriptorFactory factory)
{
    gcimpl->objFactoryMap.insert(GlobalObjectFactoryMap::value_type(objType, factory));
}

void GlobalObjectsCollector::add(GlobalObjectDescriptor* desc)
{
    gcimpl->objects.insert(GlobalObjectMap::value_type(desc->getId(), desc));
}

void GlobalObjectsCollector::clear(const std::string& id)
{
    gcimpl->objects.erase(id);
}

void GlobalObjectsCollector::cleanupObjects(std::istream* stream)
{
//    stream << ;
}


class SemaphoreDescriptor : public GlobalObjectDescriptor {
    USemaphore sem;
public:
    SemaphoreDescriptor(global_name name, USemaphore _sem)
        : GlobalObjectDescriptor(name), sem(_sem) {
        id = "SEM" + cast_to_string(sem);
    };

    static SemaphoreDescriptor * createSemDescriptor(const char * definition);

    virtual void cleanup() const;
    virtual void saveTo(std::ostream* stream) const;
};

class SemaphoreArrayDescriptor : public GlobalObjectDescriptor {
    USemaphoreArr sem;
    int size;
public:
    SemaphoreArrayDescriptor(global_name name, USemaphoreArr _sem, int _size)
        : GlobalObjectDescriptor(name), sem(_sem), size(_size) {
        id = "SEA" + cast_to_string(sem);
    }

    static SemaphoreArrayDescriptor * createSemArrayDescriptor(const char * definition);

    virtual void cleanup() const;
    virtual void saveTo(std::ostream* stream) const;
};

class SharedMemoryDescriptor : public GlobalObjectDescriptor {
    UShMem shm;
public:
    SharedMemoryDescriptor(global_name name, UShMem * _shm)
        : GlobalObjectDescriptor(name), shm(*_shm) {
        id = "SHM" + cast_to_string(shm.id);
    };

    static SemaphoreArrayDescriptor * createSharedMemoryDescriptor(const char * definition);

    virtual void cleanup() const;
    virtual void saveTo(std::ostream* stream) const;
};

class EventDescriptor : public GlobalObjectDescriptor {
    UEvent evt;
public:
    EventDescriptor(global_name name, UEvent * _evt)
        : GlobalObjectDescriptor(name), evt(*_evt) {
        id = "EVT" + cast_to_string(evt.semid);
    };

    static EventDescriptor * createEventDescriptor(const char * definition);

    virtual void cleanup() const;
    virtual void saveTo(std::ostream* stream) const;
};

void SemaphoreDescriptor::cleanup() const
{
    USemaphore tsem = sem;

    UGlobalObjectsGC = NULL;
    if (USemaphoreOpen(&tsem, name, __sys_call_error_nop) == 0) {
        if (USemaphoreRelease(tsem, __sys_call_error) != 0) {
            elog(EL_ERROR, ("Failed to cleanup semapore %s", name));
            //            throw SYSTEM_EXCEPTION();
        };
    }
    UGlobalObjectsGC = &gogc;
}

void SemaphoreDescriptor::saveTo(std::ostream* stream) const
{
    (*stream) << "SEM " << sem << "\n";
}

void EventDescriptor::cleanup() const
{
    UEvent tevt = evt;

    UGlobalObjectsGC = NULL;
    if (UEventOpen(&tevt, name, __sys_call_error_nop) == 0) {
        UEventCloseAndUnlink(&tevt, __sys_call_error);
    }
    UGlobalObjectsGC = &gogc;
}

void EventDescriptor::saveTo(std::ostream* stream) const
{
    (*stream) << "EVT " << evt.semid << "\n";
}

void SemaphoreArrayDescriptor::cleanup() const
{
    USemaphoreArr tsem = sem;

    UGlobalObjectsGC = NULL;
    if (USemaphoreArrOpen(&tsem, size, name, __sys_call_error_nop) == 0) {
        if (USemaphoreArrRelease(tsem, size, __sys_call_error) != 0) {
            elog(EL_ERROR, ("Failed to cleanup semapore %s", name));
        };
    }
    UGlobalObjectsGC = &gogc;
}

void SemaphoreArrayDescriptor::saveTo(std::ostream* stream) const
{
    (*stream) << "SEA " << sem << " " << size << "\n";
}

void SharedMemoryDescriptor::cleanup() const
{
    UShMem tshm = shm;

    UGlobalObjectsGC = NULL;
    if (uOpenShMem(&tshm, name, __sys_call_error_nop) == 0) {
        if (uReleaseShMem(&tshm, name, __sys_call_error) != 0) {
            elog(EL_ERROR, ("Failed to cleanup shared memory %s", name));
        };
    }
    UGlobalObjectsGC = &gogc;
}

void SharedMemoryDescriptor::saveTo(std::ostream* stream) const
{
    (*stream) << "SHM " << shm.id << "\n";
}

struct GlobalObjectsLess {
    bool operator()(GlobalObjectDescriptor * l, GlobalObjectDescriptor * r) {
        return l->getId() < r->getId();
    };
};

static
void globalObjectCleanup(global_name name, const char * type, void * data, int arg1, int arg2)
{
    scoped_ptr<GlobalObjectDescriptor> tmp_desc;

    if (strcmp(type, "SEM") == 0) {
        tmp_desc = new SemaphoreDescriptor(name, arg1);
    } else if (strcmp(type, "SEA") == 0) {
        tmp_desc = new SemaphoreArrayDescriptor(name, arg1, arg2);
    } else if (strcmp(type, "SHM") == 0) {
        tmp_desc = new SharedMemoryDescriptor(name, (UShMem *) data);
    } else if (strcmp(type, "EVT") == 0) {
        tmp_desc = new EventDescriptor(name, (UEvent *) data);
    } else {
        return;
    };

    tmp_desc->cleanup();
};


static
void globalObjectCreate(global_name name, const char * type, void * data, int arg1, int arg2)
{
    GlobalObjectDescriptor * desc;

    if (strcmp(type, "SEM") == 0) {
        desc = new SemaphoreDescriptor(name, arg1);
    } else if (strcmp(type, "SEA") == 0) {
        desc = new SemaphoreArrayDescriptor(name, arg1, arg2);
    } else if (strcmp(type, "SHM") == 0) {
        desc = new SharedMemoryDescriptor(name, (UShMem *) data);
    } else if (strcmp(type, "EVT") == 0) {
        desc = new EventDescriptor(name, (UEvent *) data);
    } else {
        return;
    };

    GlobalObjectsCollector::add(desc);
};

static
void globalObjectDestroy(global_name name, const char * type, void * data, int arg1, int arg2)
{
    scoped_ptr<GlobalObjectDescriptor> tmp_desc;
    
    if (strcmp(type, "SEM") == 0) {
        tmp_desc = new SemaphoreDescriptor(name, arg1);
    } else if (strcmp(type, "SEA") == 0) {
        tmp_desc = new SemaphoreArrayDescriptor(name, arg1, arg2);
    } else if (strcmp(type, "SHM") == 0) {
        tmp_desc = new SharedMemoryDescriptor(name, (UShMem *) data);
    } else if (strcmp(type, "EVT") == 0) {
        tmp_desc = new EventDescriptor(name, (UEvent *) data);
    } else {
        return;
    };

    GlobalObjectsCollector::clear(tmp_desc->getId());
};

void GlobalObjectsCollector::cleanup()
{
    for (GlobalObjectMap::iterator it = gcimpl->objects.begin(); it != gcimpl->objects.end(); ++it) {
        it->second->cleanup();
        delete it->second;
    };

    gcimpl->objects.clear();
}

GlobalObjectsCollector::GlobalObjectsCollector()
{
    if (gcimpl != NULL) {
        throw SYSTEM_EXCEPTION("Global object collector singleton violation");
    };

    gcimpl = new GlobalObjectsCollectorImplementation();
//    gcimpl->objectLogFile = uFile

#ifndef _WIN32
    gogc.onCleanup = globalObjectCleanup;
    gogc.onCreate = globalObjectCreate;
    gogc.onDestroy = globalObjectDestroy;

    UGlobalObjectsGC = &gogc;
#endif  /* _WIN32 */
}

GlobalObjectsCollector::~GlobalObjectsCollector()
{
    cleanup();

    UGlobalObjectsGC = NULL;

//    if (gcimpl->objectLogFile != NULL) {
//        fclose(gcimpl->objectLogFile);
        // delete file
//    };

    delete gcimpl;
    gcimpl = NULL;
}

