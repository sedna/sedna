/*
 * File:  XPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATH_H
#define _XPATH_H

#include <string>
#include <vector>
#include <iostream>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xsd.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/structures/schema.h"
#include "tr/structures/system_tables.h"

namespace xpath {

class dynamic_context;

enum Axis
{
    axis_child,
    axis_descendant,
    axis_attribute,
    axis_self,
    axis_descendant_or_self,
    axis_descendant_attr,
    axis_parent
};

enum NodeTestType
{
    /* KindTest */
    node_test_processing_instruction, // processing-instruction(name)
    node_test_comment,                // comment()
    node_test_text,                   // text()
    node_test_node,                   // node()
    node_test_element,                // element(name | *)
    node_test_attribute,              // attribute(name | *)
    node_test_document,               // document-node(element(* | name))

    /* NameTest */
    node_test_qname,                  // QName
    node_test_wildcard_star,          // *
    node_test_wildcard_ncname_star,   // NCName:*
    node_test_wildcard_star_ncname,   // *:NCName
};

struct NodeTestData
{
    xsd::AnyURI uri;
    xsd::NCName prefix;
    xsd::NCName local;
};

struct NodeTest {
    xpath::Axis axis;
    xpath::NodeTestType type;
    xpath::NodeTestData data;

    std::string toString() const;
    void toLR(std::ostream& str) const;
};

typedef std::vector<xpath::NodeTest *> NodeTestList;

struct NodeTestUnion {
    NodeTestList nodes;

    std::string toString() const;
    void toLR(std::ostream& str) const;
};

typedef std::vector<size_t> PathExprDistr;

typedef std::vector<xpath::NodeTestOr *> NodeTestUnionList;

struct AbsPathExpression {
    NodeTestUnionList nodeUnion;

    std::string toString() const;
    void toLR(std::ostream& str) const;
};


struct PathExprRoot
{
private:
    counted_ptr<db_entity> db_ent;
    PPOpIn name;

public:
    PathExprRoot(counted_ptr<db_entity> _db_ent_,
                 PPOpIn _name_): db_ent(_db_ent_),
                                 name(_name_) {}

    PathExprRoot(counted_ptr<db_entity> _db_ent_): db_ent(_db_ent_) {}

    void close();
    void open();
    void reopen();
    void release();

    const PPOpIn& get_operation() const { return name; }
    const counted_ptr<db_entity>& get_entity() const { return db_ent; } ;

    void set_name(PPOpIn &_name_)
    {
        name = _name_;
    }

    const counted_ptr<db_entity>& get_entity(const char* obj_name,
                                             const char* op_name);
};

void * create_PathExpr(const PathExprDistr &distr, void * memory_parent);


// void PathExpr2lr(PathExpr *path, std::ostream& str);
//PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, void * memory_parent);
//PathExpr *lr2PathExpr(dynamic_context *cxt, const char *str, void * memory_parent);
//PathExpr *build_PathExpr(schema_node_cptr from, schema_node_cptr to);
//void set_node_test_type_and_data(scheme_list *lst,
//                                 NodeTestType &nt_type, //out parameter
//                                 NodeTestData &nt_data, //out parameter
//                                 void * memory_parent);
}

#endif /* _XPATH_H */

