/*
 * File:  XPathOnSchema.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATHONSCHEMA_H
#define _XPATHONSCHEMA_H

#include <vector>
#include <set>

#include "common/sedna.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/XPath.h"

typedef std::vector<schema_node_xptr> t_scmnodes;
typedef std::set<schema_node_xptr> t_scmnodes_set;

typedef std::vector<int> SchemaPath;
typedef std::vector<SchemaPath> SchemaPathList;

class ISchemaTest {
  protected:
    t_scmnodes* result;
    SchemaPathList * pathCollection;
  public:
    virtual bool empty() const = 0;
    virtual bool test(schema_node_cptr) const = 0;

    virtual t_item getTypeMask() const = 0;

    /* Here we call it offsprings, not to mess it with xquery "children" */
    virtual void getOffsprings(schema_node_cptr) const = 0;
    virtual void getOffsprings(const t_scmnodes_set&) const = 0;

    virtual void getDescendants(schema_node_cptr) const = 0;
    virtual void getDescendants(const t_scmnodes_set&) const = 0;

    void setResultCollector(t_scmnodes* _result) { result = _result; };
    void setPathCollector(SchemaPathList * _pathCollector) { pathCollection = _pathCollector; };
};

ISchemaTest * createSchemaTest(const xpath::NodeTest &nt);

/** Evaluates a NodeTest step for a given node by descriptive schema  */
void executeNodeTest(schema_node_cptr node, const xpath::NodeTest& nt, t_scmnodes* result,
    t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

void executeNodeTestPath(schema_node_cptr node, const xpath::NodeTest& nt, t_scmnodes* result,
     SchemaPathList * pathList);

t_scmnodes * executePathExpression(const t_scmnodes& nodes, const xpath::PathExpression &pe, t_scmnodes * result,
    t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

t_scmnodes * executePathExpression(schema_node_cptr node, const xpath::PathExpression &pe, t_scmnodes * result,
    t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes);

struct ExecuteNodeTest {
    xpath::NodeTest nodeTest;
    t_scmnodes_set* extended_nodes;
    t_scmnodes_set* extender_nodes;
    t_scmnodes* result;

    ExecuteNodeTest(const xpath::NodeTest & _nodeTest, t_scmnodes_set* _extended_nodes, t_scmnodes_set* _extender_nodes, t_scmnodes* _result) :
      nodeTest(_nodeTest), extended_nodes(_extended_nodes), extender_nodes(_extender_nodes), result(_result) { };

    void operator()(const schema_node_xptr & x) {
        executeNodeTest(x, nodeTest, result, extended_nodes, extender_nodes);
    }
};

struct CompareSchemaNode {
    bool operator()(const schema_node_xptr& a, const schema_node_xptr& b) const {
        return a.to_uint64() < b.to_uint64();
    }
};

struct SameSchemaNode {
    bool operator()(const schema_node_xptr& a, const schema_node_xptr& b) const {
        return a.to_uint64() == b.to_uint64();
    }
};

#endif /* _XPATHONSCHEMA_H */

