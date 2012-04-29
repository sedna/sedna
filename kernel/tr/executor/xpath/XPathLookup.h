/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef XPATHLOOKUP_H
#define XPATHLOOKUP_H

#include "XPathTypes.h"
#include "tr/structures/nodeinterface.h"

#include <string>
#include <map>

class AxisHints;
class DataRoot;

namespace pe {

struct Path;

class IPathIterator {
  public:
    virtual ~IPathIterator() {};
    virtual Node next() = 0;
};

typedef counted_ptr<IPathIterator> NodeIterator;

class SchemaLookup {
    Path path;
public:
    SchemaLookup(const pe::Path& _path);
    ~SchemaLookup();

    AtomizedPath atomizedPath;
    AtomizedPath reversePath;

    SchemaLookup & compile();

    void execute(schema_node_cptr base, std::vector<schema_node_xptr> * output);
    void executeLimited(schema_node_cptr base, std::vector<schema_node_xptr> * output, int limit);

    /** This method tries to find schema nodes in entity, that satisfies the condition */
    void findSomething(const DataRoot& root, std::vector<schema_node_xptr> * output, int limit);
};

/*
class PathLookup {
protected:
    Path path;
public:
    PathLookup(const Path & path);
    virtual ~PathLookup() {};

    virtual void compile() = 0;
    virtual NodeIterator execute(const Node& node) = 0;

    static PathLookup * createPathLookup(const pe::Path &path);
    static PathLookup * createStepLookup(const pe::Path &path);
};

class AbsPathLookup {
};

struct LookupInfo;

};
*/

#endif /* XPATHLOOKUP_H */
