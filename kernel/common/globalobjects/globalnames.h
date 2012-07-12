#ifndef _GLOBAL_NAMES_H
#define _GLOBAL_NAMES_H

#include "u/u.h"
#include "u/ugnames.h"

#include <string>
#include <iosfwd>

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
    static void registerFactory(int objType, GlobalObjectDescriptorFactory factory);
    static void cleanupObjects(std::istream * stream);

    static void add(GlobalObjectDescriptor * );
    static void clear(const std::string & id);

    GlobalObjectsCollector(const char * sedna_base_dir);
    ~GlobalObjectsCollector();

    void cleanup();
};

#endif /* _GLOBAL_NAMES_H */
