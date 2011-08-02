/*
 * File:  XPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <list>

#include <iostream>
#include <sstream>
#include <string>

#include "common/sedna.h"

#include "common/base.h"
#include "tr/executor/base/XPath.h"
#include "tr/executor/base/PPUtils.h"
#include "common/errdbg/d_printf.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"
#include "common/commutil.h"

using namespace std;
using namespace xpath;

template <typename T> struct de_scheme_list_delete {
    inline static void deallocate(T * p) { delete_scheme_list(p); }
};

NodeTest::NodeTest(const char* str) : uri(NULL), local(NULL), qname(NULL)
{
    set(AutoSchemeList(str).get());
}

PathExpression::PathExpression() : _size(0), nodes(NULL)
{

}

PathExpression::PathExpression(const scheme_list* path_lst, dynamic_context* cxt) {
    set(path_lst, cxt);
}

string NodeTest::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
}

string NodeTestUnion::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
}

string PathExpression::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
}


PathExpression::PathExpression(const char* str, dynamic_context* cxt)
{
    set(AutoSchemeList(str).get(), cxt);
}

void* PathExpression::operator new(size_t size)
{
    return cat_malloc(default_context_space, size);
}

void PathExpression::operator delete(void* mem)
{
    return;
}

PathExpression::PathExpression(schema_node_cptr from, schema_node_cptr to)
{
    U_ASSERT(false);
}

////////////////////////////////////////////////////////////////////////////////
/// PathExprRoot - manages document or collection PathExpr is evaluated on
////////////////////////////////////////////////////////////////////////////////

void PathExprRoot::release()
{
    if (name.op != NULL) {
        delete name.op;
        name.op = NULL;
    }
}

void PathExprRoot::open()
{
    if (name.op != NULL) {
        name.op->open();
    }
}

void PathExprRoot::reopen()
{
    if (name.op != NULL) {
        name.op->reopen();
    }
}

void PathExprRoot::close()
{
    if (name.op != NULL) {
        name.op->close();
    }
}

const counted_ptr<db_entity>& PathExprRoot::get_entity(const char* obj_name, const char* op_name)
{
    U_ASSERT(db_ent->type != dbe_module);

    if(name.op != NULL)
    {
       tuple_cell root_tc = get_name_from_PPOpIn(name, obj_name, op_name);
       if (db_ent->name) delete [] db_ent->name;
       db_ent->name = se_new char[root_tc.get_strlen_mem() + 1];
       strcpy(db_ent->name, root_tc.get_str_mem());
    }
    return db_ent;
}


/*
    ******** ALL SERIALIZATION HERE *******************************************************

    Serialized XPath form:

    xpath := (xpath [node-test]*)
    node-test-or-union := (union [node-test]*) | [node-test]
    node-test := (test [axis] [test])
    test := ([test-name] [test-data]?)
    axis := [axis-name]
*/

struct axis_pair_t {
  xpath::Axis type;
  const char * lr;
};

struct node_test_pair_t {
  xpath::NodeTestType type;
  const char * lr;
  const char * str;
};

static const axis_pair_t axisTypeMap[] = {
  { axis_any, "errornous" },
  { axis_child, "child" },
  { axis_descendant, "descendant" },
  { axis_attribute, "attribute" },
  { axis_self, "self" },
  { axis_descendant_or_self, "descendant-or-self" },
  { axis_descendant_attr, "descendant-attribute" },
  { axis_parent, "parent" },
  { __axis_last, "errornous" },
  { axis_ancestor, "ancestor" },
  { axis_ancestor_or_self, "ancestor-or-self" },
  { axis_following, "following" },
  { axis_following_sibling, "following-sibling" },
  { axis_preceding, "preceding" },
  { axis_preceding_sibling, "preceding-sibling" },
};

static const node_test_pair_t nodeTypeMap[] = {
  { node_test_invalid, "errornous", "Errornous" },
  { node_test_pi, "processing-instruction", "node_test_pi" },
  { node_test_comment, "comment", "node_test_comment" },
  { node_test_text, "text", "node_test_text" },
  { node_test_node, "node", "node_test_node" },
  { node_test_element, "element", "node_test_element" },
  { node_test_attribute, "attribute", "node_test_attribute" },
  { node_test_document, "document-node", "node_test_document" },
  { node_test_qname, "qname", "node_test_qname" },
  { node_test_wildcard_star, "wildcard_star", "node_test_wildcard_star" },
  { node_test_wildcard_ncname_star, "wildcard_ncname_star", "node_test_wildcard_ncname_star" },
  { node_test_wildcard_star_ncname, "wildcard_star_ncname", "node_test_wildcard_star_ncname" }
};

#define axisTypeMapSize ((int) (sizeof(axisTypeMap) / sizeof(axis_pair_t)))
#define nodeTypeMapSize ((int) (sizeof(nodeTypeMap) / sizeof(node_test_pair_t)))

static inline
xpath::Axis axisTypeFromLR(const char * axis) {
    for (int i = 0; i < axisTypeMapSize; ++i) {
        if (strcmp(axis, axisTypeMap[i].lr) == 0) {
            return axisTypeMap[i].type;
        }
    }

    return axis_any;
}

static inline
const char * axisTypeToStr(xpath::Axis axis) {
    if (axis >= axis_any && axis < __axis_last) {
        return axisTypeMap[axis].lr;
    } else {
        return NULL;
    }
}

static inline
const char * axisTypeToLR(xpath::Axis axis) {
    if (axis >= axis_any && axis < __axis_last) {
        return axisTypeMap[axis].lr;
    } else {
        return NULL;
    }
}


static inline
xpath::NodeTestType nodeTypeFromStr(const char * node) {
    for (int i = 0; i < nodeTypeMapSize; ++i) {
        if (strcmp(node, nodeTypeMap[i].str) == 0) {
            return nodeTypeMap[i].type;
        }
    }

    return node_test_invalid;
}

static inline
xpath::NodeTestType nodeTypeFromLR(const char * node) {
    for (int i = 0; i < nodeTypeMapSize; ++i) {
        if (strcmp(node, nodeTypeMap[i].lr) == 0) {
            return nodeTypeMap[i].type;
        }
    }

    return node_test_invalid;
}

static inline
const char * nodeTypeToStr(xpath::NodeTestType node) {
    if (node >= node_test_invalid && node < __node_test_last) {
        return nodeTypeMap[node].str;
    } else {
        return NULL;
    }
}

static inline
const char * nodeTypeToLR(xpath::NodeTestType node) {
    if (node >= node_test_invalid && node < __node_test_last) {
        return nodeTypeMap[node].lr;
    } else {
        return NULL;
    }
}



string NodeTest::toXPathString() const
{
    string result;

    if (axis != axis_any) {
        result += string(axisTypeToStr(axis)) + "::";
    }

    const char * typeName = nodeTypeToLR(type);

    switch (type) {
      case node_test_pi:
        result += string(typeName) + "(" + (local != NULL ? getLocal().getValue() : "") + ")"; break;
      case node_test_comment :
      case node_test_text :
      case node_test_node :
        result += string(typeName) + "()"; break;
      case node_test_element :
      case node_test_attribute :
        result += string(typeName) + "(" + (isAnyQName() ? "" : getQName().getColonizedName()) + ")"; break;
      case node_test_document :
        if (xsd::NCName(local).valid()) {
            result += string("document-node(element(") + getLocal().getValue() + "))";
        } else {
            result += string("document-node()");
        }; break;
      case node_test_qname :
        result += getQName().getColonizedName(); break;
      case node_test_wildcard_star :
        result += string("*"); break;
      case node_test_wildcard_ncname_star :
        result += string("declare-ns(") + getUri().getValue() + "):*"; break;
      case node_test_wildcard_star_ncname :
        result += string("*:") + getLocal().getValue(); break;
      default :
        result += typeName; break;
    }

    return result;
}

string NodeTestUnion::toXPathString() const
{
    string result;

    U_ASSERT((size > 0));

    if (size == 1) {
        return nodes[0].toXPathString();
    } else {
        result = string("( ") + nodes[0].toXPathString();

        for (size_t i = 1; i < size; i++) {
            result += " | " + nodes[i].toXPathString();
        }

        result += ")";
    }

    return result;
}

string PathExpression::toXPathString() const
{
    if (size() > 0) {
        string result = nodes[0].toXPathString();

        for (size_t i = 1; i < size(); i++) {
            result += string("/") + nodes[i].toXPathString();
        }

        return result;
    }

    return string();
}


#define CL_CHECK_SYMBOL(X, Pos, Symb) (((X)->at(Pos).type == SCM_SYMBOL) && strcmpex((X)->at(Pos).internal.symb, (Symb)) == 0)

void PathExpression::set(const scheme_list* path_lst, dynamic_context* cxt)
{
    _size = path_lst->size() - 1;

    if (path_lst->size() < 1 || !CL_CHECK_SYMBOL(path_lst, 0, "xpath")) {
        throw USER_EXCEPTION2(SE1004, "Not an xpath expression");
    }

    nodes = (NodeTestUnion *) cat_malloc(default_context_space, _size * sizeof(NodeTestUnion));

    for (scheme_list::size_type i = 1; i < path_lst->size(); i++) {
        if (path_lst->at(i).type != SCM_LIST || path_lst->at(i).internal.list->size() < 1) {
            throw USER_EXCEPTION2(SE1004, "Invalid xpath argument: list expected");
        }

        const scheme_list* arg = path_lst->at(i).internal.list;
        NodeTestUnion * dstNodeUnion = nodes + i - 1;

        if (CL_CHECK_SYMBOL(arg, 0, "union")) {
            dstNodeUnion->size = arg->size() - 1;
            dstNodeUnion->nodes = (NodeTest *) cat_malloc(default_context_space, dstNodeUnion->size * sizeof(NodeTest));

            for (scheme_list::size_type j = 1; j < arg->size(); j++) {
                if (arg->at(j).type != SCM_LIST) {
                  throw USER_EXCEPTION2(SE1004, "Invalid xpath union member");
                }

                *(dstNodeUnion->nodes + j - 1) = NodeTest(arg->at(j).internal.list);
            }
        } else if (CL_CHECK_SYMBOL(arg, 0, "test")) {
            dstNodeUnion->size = 1;
            dstNodeUnion->nodes = (NodeTest *) cat_malloc(default_context_space, dstNodeUnion->size * sizeof(NodeTest));
            *dstNodeUnion->nodes = NodeTest(arg);
        } else {
            throw USER_EXCEPTION2(SE1004, "Unknown xpath list argument");
        }
    }
}

void NodeTest::set(const scheme_list* lst)
{
    if (lst->size() < 3 ||
        !CL_CHECK_SYMBOL(lst, 0, "test") ||
        lst->at(1).type != SCM_SYMBOL ||
        lst->at(2).type != SCM_LIST) {
        throw USER_EXCEPTION2(SE1004, "Node test expected, but not found or invalid");
    }

    axis = axisTypeFromLR(lst->at(1).internal.symb);

    if (axis == axis_any) {
        throw USER_EXCEPTION2(SE1004, "Invalid serialized axis expression");
    }

    /* Parse node test */
    const scheme_list* test_expr = lst->at(2).internal.list;

    if (test_expr->size() < 1 ||
        lst->at(0).type != SCM_SYMBOL) {
        throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression");
    }

    type = nodeTypeFromLR(test_expr->at(0).internal.symb);

    if (type == node_test_invalid) {
        throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression");
    }

    switch(type) {
        case node_test_wildcard_ncname_star: {
            if (test_expr->size() < 2 || test_expr->at(1).type != SCM_STRING) {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression : URI expected");
            }

            uri = xsd::AnyURI::check(test_expr->at(1).internal.str).serialize(default_context_space);
        } break;

        case node_test_wildcard_star_ncname: {
            if (test_expr->size() < 2 || test_expr->at(1).type != SCM_STRING) {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression : NCName expected");
            }

            local = xsd::NCName::check(test_expr->at(1).internal.str, true).serialize(default_context_space);
        } break;

        case node_test_qname:
        case node_test_wildcard_star:
        case node_test_document:
        case node_test_attribute:
        case node_test_element: {
            if (test_expr->size() != 2) {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression: argument expected");
            }

            if (type != node_test_qname &&
                  type != node_test_wildcard_star &&
                  CL_CHECK_SYMBOL(test_expr, 1, "any")) {
                qname = NULL;
            } else if (test_expr->at(1).type == SCM_LIST && test_expr->at(1).internal.list->size() > 1 &&
                  CL_CHECK_SYMBOL(test_expr->at(1).internal.list, 0, "qname")) {
                qname = xsd::QName::fromLR(test_expr->at(1).internal.list).serialize(default_context_space);
            } else {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression: any or QName expected");
            }
        } break;
        case node_test_pi:
        {
            if (test_expr->size() != 2) {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression: argument expected");
            }

            if (CL_CHECK_SYMBOL(test_expr, 1, "any")) {
                local = NULL;
            } else if (test_expr->at(1).type == SCM_STRING) {
                local = xsd::NCName::check(test_expr->at(1).internal.str, true).serialize(default_context_space);
            } else {
                throw USER_EXCEPTION2(SE1004, "Invalid serialized node test expression: any or NCName expected");
            }
        } break;
        default: break;
    }
}

ostream& NodeTest::toStream(ostream& str) const
{
    str << "(test ";
    str << axisTypeToLR(axis) << " ";
    str << "(" << nodeTypeToLR(type) << " ";

    switch (type) {
      case node_test_pi:
        if (local == NULL || *local == '\0') {
            str << "any";
        } else {
            xsd::NCName(local).toLR(str);
        } break;
      case node_test_element :
      case node_test_attribute :
      case node_test_document :
      case node_test_qname :
        if (qname == NULL) {
            str << "any";
        } else {
            xsd::QName::deserialize(qname).toLR(str);
        } break;
      case node_test_wildcard_ncname_star :
        xsd::AnyURI(uri).toLR(str); break;
      case node_test_wildcard_star_ncname :
        xsd::NCName(local).toLR(str); break;
      case node_test_comment :
      case node_test_text :
      case node_test_node :
      case node_test_wildcard_star :
      default :
        break;
    }

    str << "))";

    return str;
}

ostream& NodeTestUnion::toStream(ostream& str) const
{
    U_ASSERT((size > 0));

    if (size == 1) {
        nodes[0].toStream(str);
    } else {
        str << "(union ";

        nodes[0].toStream(str);
        for (size_t i = 1; i < size; i++) {
            str << "";
            nodes[i].toStream(str);
        }

        str << ")";
    }

    return str;
}

std::ostream& PathExpression::toStream(ostream& str) const
{
    str << "(xpath ";

    if (size() > 0) {
        nodes[0].toStream(str);
        for (size_t i = 1; i < size(); i++) {
            str << " ";
            nodes[i].toStream(str);
        }
    }

    str << ")";

    return str;
}



