#include "globalnames.h"

#include "u/usem.h"
#include "u/ushm.h"
#include "u/uevent.h"
#include "u/uhdd.h"

#include "auxiliary/cppcast.h"
#include "auxiliary/counted_ptr.h"

#include "common/sedna.h"
#include "common/errdbg/event_log.h"
#include "common/errdbg/errors.h"

#include <string>
#include <map>

typedef std::map<std::string, GlobalObjectDescriptor *> GlobalObjectMap;
typedef std::map<int, GlobalObjectDescriptorFactory> GlobalObjectFactoryMap;

struct GlobalObjectsCollectorImplementation {
    UFile objectLogFile;
    GlobalObjectMap objects;
    GlobalObjectFactoryMap objFactoryMap;
};

static GlobalObjectsCollectorImplementation * gcimpl = NULL;
UGlobalGarbageCollector gogc = {};

void GlobalObjectsCollector::registerFactory(int objType, GlobalObjectDescriptorFactory factory)
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

    virtual void cleanup() const;
    virtual void saveTo(std::ostream* stream) const;
};

class EventDescriptor : public GlobalObjectDescriptor {
    UEvent evt;
public:
    EventDescriptor(global_name name, UEvent * _evt)
        : GlobalObjectDescriptor(name), evt(*_evt) {
#ifndef _WIN32
        id = "EVT" + cast_to_string(evt.semid);
#else
        id = "EVT" + cast_to_string(evt.handle);
#endif /* _WIN32 */
    };

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
#ifndef _WIN32
    (*stream) << "EVT " << evt.semid << "\n";
#else
    (*stream) << "EVT " << evt.handle << "\n";
#endif /* _WIN32 */
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
void globalObjectCleanup(global_name name, gobj_info_t data)
{
    scoped_ptr<GlobalObjectDescriptor> tmp_desc;

    switch (data.type) {
      case GOBJECT_EVENT:      tmp_desc = new EventDescriptor(name, (UEvent *) data.data); break;
      case GOBJECT_SEM:        tmp_desc = new SemaphoreDescriptor(name, * (USemaphore *) data.data); break;
      case GOBJECT_SEM_ARRAY:  tmp_desc = new SemaphoreArrayDescriptor(name, * (USemaphoreArr *) data.data, data.arg1); break;
      case GOBJECT_SHARED_MEM: tmp_desc = new SharedMemoryDescriptor(name, (UShMem *) data.data); break;
      default:
        U_ASSERT(false);
        break;
    };

    tmp_desc->cleanup();
};


static
void globalObjectCreate(global_name name, gobj_info_t data)
{
    GlobalObjectDescriptor * desc;

    switch (data.type) {
      case GOBJECT_EVENT:      desc = new EventDescriptor(name, (UEvent *) data.data); break;
      case GOBJECT_SEM:        desc = new SemaphoreDescriptor(name, * (USemaphore *) data.data); break;
      case GOBJECT_SEM_ARRAY:  desc = new SemaphoreArrayDescriptor(name, * (USemaphoreArr *) data.data, data.arg1); break;
      case GOBJECT_SHARED_MEM: desc = new SharedMemoryDescriptor(name, (UShMem *) data.data); break;
      default:
        U_ASSERT(false);
        break;
    };

    GlobalObjectsCollector::add(desc);
};

static
void globalObjectDestroy(global_name name, gobj_info_t data)
{
    scoped_ptr<GlobalObjectDescriptor> tmp_desc;

    switch (data.type) {
      case GOBJECT_EVENT:      tmp_desc = new EventDescriptor(name, (UEvent *) data.data); break;
      case GOBJECT_SEM:        tmp_desc = new SemaphoreDescriptor(name, * (USemaphore *) data.data); break;
      case GOBJECT_SEM_ARRAY:  tmp_desc = new SemaphoreArrayDescriptor(name, * (USemaphoreArr *) data.data, data.arg1); break;
      case GOBJECT_SHARED_MEM: tmp_desc = new SharedMemoryDescriptor(name, (UShMem *) data.data); break;
      default:
        U_ASSERT(false);
        break;
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

GlobalObjectsCollector::GlobalObjectsCollector(const char * sedna_base_dir)
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

