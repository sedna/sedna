#ifndef _GLOBAL_NAMES_H
#define _GLOBAL_NAMES_H

#include "u/u.h"
#include "u/ugnames.h"

#define GLOBAL_NAME(NAME) #NAME

#define __GLOBAL_NAME_REGISTRY_ENTRY(NAME, PREFIX, MAX_ITEMS, TAG) \
  {GLOBAL_NAME(NAME), PREFIX, MAX_ITEMS, TAG}

#define MAX_GLOBAL_NAME_LEN 128

struct GlobalObjectsCollectorImplementation;

class GlobalObjectDescriptor {
protected:
    global_name name;
public:
    GlobalObjectDescriptor(global_name _name) : name(_name) {};

    virtual ~GlobalObjectDescriptor() {};
    virtual void cleanup() = 0;
};

class GlobalObjectsCollector {
private:
    GlobalObjectsCollectorImplementation * impl;
public:
    GlobalObjectsCollector();
    ~GlobalObjectsCollector();

    void globalCleanup();

    void cleanup();

    void add(global_name gname, GlobalObjectDescriptor * );
    void clear(global_name gname);
};

#endif /* _GLOBAL_NAMES_H */
