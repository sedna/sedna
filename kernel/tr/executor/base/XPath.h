/*
 * File:  XPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPATH_H
#define _XPATH_H

#include <string>
#include <vector>
#include <iostream>
#include <map>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xsd.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/cat/catptr.h"

class dynamic_context;

namespace xpath {

typedef std::map<std::string, std::string> namespaces_map;
typedef std::pair<std::string, std::string> namespaces_map_item;
typedef std::map<std::string, std::string>::iterator namespaces_map_iter;

enum Axis
{
    axis_any = 0, /* When not used, or implied */

    axis_child,
    axis_descendant,
    axis_attribute,
    axis_self,
    axis_descendant_or_self,
    axis_descendant_attr, /* Depricated, should not be used. There is no such axis in XQuery. */
    axis_parent,

    __axis_err,

    axis_ancestor,
    axis_ancestor_or_self,
    axis_following,
    axis_following_sibling,
    axis_preceding,
    axis_preceding_sibling,

    __axis_last,
};

enum NodeTestType
{
    node_test_invalid = 0,

    /* KindTest */
    node_test_pi, // processing-instruction(name)
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

    __node_test_last
};

struct NodeTest {
    xpath::Axis axis;
    xpath::NodeTestType type;

  private:
    const char * uri; /* for wildcard uri */
    const char * local; /* for wildcard localname (either prefix or ncname) or PI name */
    const char * qname; /* for full qname */

    void set(const scheme_list * path_lst);
  public:
    NodeTest() : axis(axis_any), type(node_test_invalid), uri(NULL), local(NULL), qname(NULL) {};

    NodeTest(xpath::Axis _axis, xpath::NodeTestType _type) : axis(_axis), type(_type), uri(NULL), local(NULL), qname(NULL) {};
    NodeTest(const scheme_list * path_lst) : uri(NULL), local(NULL), qname(NULL) { set(path_lst); }
    NodeTest(const char * str);

    xsd::QName getQName() const { return xsd::QName::deserialize(qname); };
    xsd::AnyURI getUri() const { return xsd::AnyURI(uri); };
    xsd::NCName getLocal() const  { return xsd::NCName(local); };
    bool isAnyQName() const { return qname == NULL; }

    std::string toString() const;
    std::string toXPathString() const;
    std::ostream& toStream(std::ostream& str) const;
    void getDefinedNamespaces(namespaces_map& res) const;
};

struct NodeTestUnion {
    size_t size;
    xpath::NodeTest * nodes;

    std::string toString() const;
    std::string toXPathString() const;
    std::ostream& toStream(std::ostream& str) const;
    void getDefinedNamespaces(namespaces_map& res) const;
};

struct PathExpression {
  private:
    size_t _size;
    xpath::NodeTestUnion * nodes;

    void set(const scheme_list* path_lst, dynamic_context* cxt);
  public:
    PathExpression();
    PathExpression(const scheme_list * path_lst, dynamic_context * cxt);
    PathExpression(const char * str, dynamic_context * cxt);
    PathExpression(schema_node_cptr from, schema_node_cptr to);

    size_t size() const { return _size; };
    NodeTestUnion & operator[](size_t i) const { return nodes[i]; };

    void * operator new(size_t size);
    void operator delete(void * mem);

    std::string toString() const;
    std::string toXPathString() const;
    std::string toLRString() const { return toString(); };
    std::ostream& toStream(std::ostream& str) const;
    namespaces_map getDefinedNamespaces() const;
    void getDefinedNamespaces(namespaces_map& res) const;
};



struct PathExprRoot {
private:
    counted_ptr<db_entity> db_ent;
    PPOpIn name;

public:
    PathExprRoot(counted_ptr<db_entity> _db_ent_, PPOpIn _name_)
      : db_ent(_db_ent_), name(_name_) {}

    PathExprRoot(counted_ptr<db_entity> _db_ent_): db_ent(_db_ent_) {}

    void close();
    void open();
    void reopen();
    void release();

    const PPOpIn& get_operation() const { return name; }
    const counted_ptr<db_entity>& get_entity() const { return db_ent; } ;

    void set_name(PPOpIn &_name_) { name = _name_; }

    const counted_ptr<db_entity>& get_entity(const char* obj_name, const char* op_name);
};

//void * create_PathExpr(const PathExprDistr &distr, void * memory_parent);
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

