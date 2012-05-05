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
#include <stack>

struct PathExecutionEvironment;

class AxisHints;
class DataRoot;

namespace pe {

struct Path;

bool executeSchemaPathTest(schema_node_cptr base, const AtomizedPath & path, SchemaNodePtrSet * output, bool _fast = false);

class SchemaLookup {
    Path path;
public:
    SchemaLookup(const pe::Path& _path) : path(_path) {};
    ~SchemaLookup() {};

    AtomizedPath atomizedPath;
    AtomizedPath reversePath;

    SchemaLookup(const AtomizedPath& _path) : atomizedPath(_path) {};

    SchemaLookup & compile();

    bool exists(schema_node_cptr base);
    void execute(schema_node_cptr base, SchemaNodePtrList * output);
    void executeLimited(schema_node_cptr base, SchemaNodePtrList * output, int limit);

    /** This method tries to find schema nodes in entity, that satisfies the condition */
    void findSomething(const DataRoot& root, SchemaNodePtrList * output, int limit);
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

struct LookupInfo;
*/

/* namespace pe */
};

#endif /* XPATHLOOKUP_H */
