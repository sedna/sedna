/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef XPATHLOOKUP_H
#define XPATHLOOKUP_H

#include "XPathTypes.h"
#include "AtomizedPath.h"

#include "tr/structures/nodeinterface.h"
#include "tr/opt/algorithms/SequenceModel.h"

#include <string>
#include <map>
#include <stack>

class PathSchemaMerge;
struct PathTraverse;
struct PathExecutionEvironment;

class AxisHints;
class DataRoot;

namespace pe {

struct Path;

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
    void executeAll(const SchemaNodePtrList * in, SchemaNodePtrList * output);
    void executeLimited(schema_node_cptr base, SchemaNodePtrList * output, int limit);

    /** This method tries to find schema nodes in entity, that satisfies the condition */
    int findSomething(const DataRoot& root, SchemaNodePtrList * output, int limit);
};


class PathSchemaCheck : public phop::ItemOperator {
    RTTI_DECL(sequence_operator_PathSchemaCheck, phop::ItemOperator)

    typedef std::map<schema_node_xptr, bool> SchemaCache;

    SchemaLookup scnLookup;
    SchemaCache cache;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    PathSchemaCheck(const AtomizedPath& apath, const phop::MappedTupleIn & _in, unsigned _size, unsigned _resultIdx);
    virtual void reset();
};

class PathEvaluateTraverse : public phop::ItemOperator {
    RTTI_DECL(sequence_operator_PathEvaluateTraverse, phop::ItemOperator)
    PathTraverse * traverse;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    PathEvaluateTraverse(const AtomizedPath& apath, const phop::MappedTupleIn & _in, unsigned _size, unsigned _resultIdx);
    virtual ~PathEvaluateTraverse();
    virtual void reset();
};

class PathSchemaResolve : public phop::ItemOperator {
    RTTI_DECL(sequence_operator_PathSchemaResolve, phop::ItemOperator)

    typedef std::map<schema_node_xptr, SchemaNodeList> SchemaCache;

    SchemaLookup scnLookup;
    SchemaCache cache;
    PathSchemaMerge * merge;
protected:
    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    PathSchemaResolve(const AtomizedPath& apath, const phop::MappedTupleIn & _in, unsigned _size, unsigned _resultIdx);
    virtual ~PathSchemaResolve();
    virtual void reset();
};

/* namespace pe */
};

#endif /* XPATHLOOKUP_H */
