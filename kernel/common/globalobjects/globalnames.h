#ifndef _GLOBAL_NAMES_H
#define _GLOBAL_NAMES_H

#include "u/u.h"
#include "u/ugnames.h"

#include <string>
#include <iosfwd>

#define GLOBAL_NAME(NAME) #NAME

#define __GLOBAL_NAME_REGISTRY_ENTRY(NAME, PREFIX, MAX_ITEMS) \
  {GLOBAL_NAME(NAME), PREFIX, MAX_ITEMS, -1}

#define MAX_GLOBAL_NAME_LEN 128

class GlobalObjectDescriptor {
protected:
    global_name name;
    std::string id;
public:
    GlobalObjectDescriptor(global_name _name) : name(_name) {};

    virtual ~GlobalObjectDescriptor() {};
    virtual void cleanup() const = 0;
    virtual void saveTo(std::ostream * stream) const = 0;

    std::string getId() const { return id; };
};

typedef GlobalObjectDescriptor * (*GlobalObjectDescriptorFactory) (const std::string &);

// Singleton class,

class GlobalObjectsCollector { 
public:
    static void registerFactory(const char * objType, GlobalObjectDescriptorFactory factory);
    static void cleanupObjects(std::istream * stream);

    static void add(GlobalObjectDescriptor * );
    static void clear(const std::string & id);

    GlobalObjectsCollector();
    ~GlobalObjectsCollector();

    void cleanup();
};

#endif /* _GLOBAL_NAMES_H */
