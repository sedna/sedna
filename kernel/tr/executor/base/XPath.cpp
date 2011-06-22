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

using namespace std;
using namespace xpath;

template <typename T> struct de_scheme_list_delete {
    inline static void deallocate(T * p) { delete_scheme_list(p); }
};

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

void NodeTest::set(scheme_list* lst)
{
    // CHECK Node test: (SYMBOL, SYMBOL, *)

    if (lst->size() != 3 ||
          lst->at(0).type != SCM_SYMBOL ||
          lst->at(1).type != SCM_SYMBOL ) {
        throw USER_EXCEPTION2(SE1004, "Path expression");
    }

    axis = axisTypeFromLR(lst->at(0).internal.symb);

    if (axis == axis_any) {
        throw USER_EXCEPTION2(SE1004, "Path expression");
    }

    type = nodeTypeFromLR(lst->at(1).internal.symb);

    if (type == node_test_invalid) {
        throw USER_EXCEPTION2(SE1004, "Path expression");
    }

    switch(type) {
        case node_test_wildcard_ncname_star: {
            if (lst->at(2).type != SCM_STRING) {
                throw USER_EXCEPTION2(SE1004, "Path expression");
            }

            uri = xsd::AnyURI::check(lst->at(2).internal.str).serialize(default_context_space);
        } break;

        case node_test_wildcard_star_ncname: {
            if (lst->at(2).type != SCM_STRING) {
                throw USER_EXCEPTION2(SE1004, "Path expression");
            }

            local = xsd::NCName::check(lst->at(2).internal.str).serialize(default_context_space);
        } break;

        case node_test_document:
        case node_test_attribute:
        case node_test_element: {
            if (lst->at(2).type != SCM_LIST && lst->at(2).internal.list->size() != 0) {
                throw USER_EXCEPTION2(SE1004, "Path expression");
            }

            if (lst->at(2).internal.list->size() != 0) {
                qname = xsd::QName::fromLR(lst->at(2).internal.list).serialize(default_context_space);
            } else {
                qname = NULL;
            }
        } break;

        case node_test_qname:
        {
            if (lst->at(2).type != SCM_LIST) {
                throw USER_EXCEPTION2(SE1004, "node_test_qname");
            }

            qname = xsd::QName::fromLR(lst->at(2).internal.list).serialize(default_context_space);
        } break;

        case node_test_pi:
        {
            if (lst->at(2).type == SCM_STRING) {
                throw USER_EXCEPTION2(SE1004, "Bad PI node");
            }

            local = lst->at(2).internal.str;

            if (*local != '\0') {
                local = xsd::NCName::check(local).serialize(default_context_space);
            } else {
                local = NULL;
            }
        } break;

        default: break;
    }
}

NodeTest::NodeTest(const char* str) : uri(NULL), local(NULL), qname(NULL)
{
    scoped_ptr<scheme_list> lst = make_tree_from_scheme_list(str);
    set(lst.get());
}

void PathExpression::set(scheme_list* path_lst, dynamic_context* cxt)
{
    _size = path_lst->size();
    nodes = (NodeTestUnion *) cat_malloc(default_context_space, _size * sizeof(NodeTestUnion));

    for (scheme_list::size_type i = 0; i < path_lst->size(); i++) {
        if (path_lst->at(i).type != SCM_LIST || path_lst->at(i).internal.list->size() < 1) {
            throw USER_EXCEPTION2(SE1004, "Path expression");
        }

        scheme_list* srcNodeUnion = path_lst->at(i).internal.list;
        NodeTestUnion * dstNodeUnion = nodes + i;

        dstNodeUnion->size = srcNodeUnion->size();
        dstNodeUnion->nodes = (NodeTest *) cat_malloc(default_context_space, dstNodeUnion->size * sizeof(NodeTest));

        for (scheme_list::size_type j = 0; j < srcNodeUnion->size(); j++) {
            if (srcNodeUnion->at(j).type != SCM_LIST) {
                throw USER_EXCEPTION2(SE1004, "Path expression");
            }

            scheme_list* srcNodes = path_lst->at(i).internal.list;

            *(dstNodeUnion->nodes + i) = NodeTest(srcNodes);
        }
    }
}



PathExpression::PathExpression(scheme_list* path_lst, dynamic_context* cxt) {
    set(path_lst, cxt);
}

string NodeTest::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
/*
    string result;

    if (axis != axis_any) {
        result += string(axisTypeToStr(axis)) + "::";
    }

    const char * typeName = nodeTypeToLR(type);

    switch (type) {
      case node_test_pi:
        result += string(typeName) + "(" + xsd::NCName(local).toString() + ")"; break;
      case node_test_comment :
      case node_test_text :
      case node_test_node :
        result += string(typeName) + "()"; break;
      case node_test_element :
      case node_test_attribute :
        result += string(typeName) + "(" + xsd::QName::getColonizedName(prefix, local) + ")"; break;
      case node_test_document :
        if (xsd::NCName(local).valid()) {
            result += "document-node(element(" + xsd::QName::getColonizedName(prefix, local) + "))";
        } else {
            result += "document-node()";
        }; break;
      case node_test_qname :
        result += xsd::QName::getColonizedName(prefix, local); break;
      case node_test_wildcard_star :
        result += "*"; break;
      case node_test_wildcard_ncname_star :
        result +=  xsd::NCName(prefix).toString() + ":*"; break;
      case node_test_wildcard_star_ncname :
        result += "*:" + xsd::NCName(local).toString(); break;
      default :
        result += typeName; break;
    }

    return result;
*/
}

ostream& NodeTest::toStream(ostream& str) const
{
    str << "(";
    str << axisTypeToLR(axis) << " ";
    str << nodeTypeToLR(type);

    switch (type) {
      case node_test_pi:
        xsd::NCName(local).toLR(str);
      case node_test_comment :
      case node_test_text :
      case node_test_node :
      case node_test_wildcard_star :
        str << " () ";
        break;
      case node_test_element :
      case node_test_attribute :
      case node_test_document :
      case node_test_qname :
        str << "("; xsd::QName::deserialize(qname).toLR(str); str << ")"; break;
      case node_test_wildcard_ncname_star :
        xsd::AnyURI(uri).toLR(str); break;
      case node_test_wildcard_star_ncname :
        xsd::NCName(local).toLR(str); break;
      default :
        break;
    }

    str << ")";

    return str;
}

string NodeTestUnion::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
}

ostream& NodeTestUnion::toStream(ostream& str) const
{
    str << "(";

    U_ASSERT((size > 0));

    nodes[0].toStream(str);
    for (size_t i = 1; i < size; i++) {
        str << " ";
        nodes[i].toStream(str);
    }

    str << ")";

    return str;
}

string PathExpression::toString() const
{
    ostringstream oss(std::ios::out | std::ios::binary);
    toStream(oss);
    return oss.str();
}

std::ostream& PathExpression::toStream(ostream& str) const
{
    str << "(";

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

PathExpression::PathExpression(const char* str, dynamic_context* cxt)
{
    scoped_ptr<scheme_list> lst = make_tree_from_scheme_list(str);
    set(lst.get(), cxt);
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

/*

PathExpr *build_PathExpr(schema_node_cptr from, schema_node_cptr to)
{
    const static PathExprMemoryManager * mm = pe_local_aspace;

    std::vector<schema_node_xptr> scm_nodes;
    schema_node_cptr cur = to;
    while (cur.ptr() != from.ptr())
    {
        scm_nodes.push_back(cur.ptr());
        cur = cur->parent;
    }

    PathExpr *path_expr = (PathExpr*)mm->alloc(sizeof(PathExpr));
    path_expr->nto = (NodeTestOr*)mm->alloc(sizeof(NodeTestOr) * scm_nodes.size());
    path_expr->size = scm_nodes.size();

    unsigned int i = 0;
    schema_node_cptr parent = from;
    for (i = 0; i < scm_nodes.size(); ++i)
    {
        cur = scm_nodes[scm_nodes.size() - i - 1];

        path_expr->nto[i].nt = (NodeTest*)mm->alloc(sizeof(NodeTest));
        path_expr->nto[i].size = 1;

        if (cur->type == attribute)
            path_expr->nto[i].nt->axis = axis_attribute;
        else
            path_expr->nto[i].nt->axis = axis_child;

        switch (cur->type)
        {
            case element      : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    path_expr->nto[i].nt->data.ncname_prefix = xs_NCName_create(cur->name, mm->alloc);
                                    path_expr->nto[i].nt->data.ncname_local =  xs_NCName_create(cur->get_xmlns()->uri, mm->alloc);
                                    break;
                                }
            case text         : {
                                    path_expr->nto[i].nt->type = node_test_text;
                                    break;
                                }
            case attribute    : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    // FIXME:
                                    path_expr->nto[i].nt->data.ncname_prefix = xs_NCName_create(cur->name, mm->alloc);
                                    path_expr->nto[i].nt->data.ncname_local =  xs_NCName_create(cur->get_xmlns()->uri, mm->alloc);
                                    break;
                                }
            case document     : throw USER_EXCEPTION2(SE1003, "build_PathExpr: document as a child node");
            case virtual_root : throw USER_EXCEPTION2(SE1003, "build_PathExpr: virtual_root as a child node");
            case xml_namespace: throw USER_EXCEPTION2(SE1003, "build_PathExpr: xml_namespace as a child node");
            case comment      : {
                                    path_expr->nto[i].nt->type = node_test_comment;
                                    break;
                                }
            case pr_ins       : {
                                    path_expr->nto[i].nt->type = node_test_processing_instruction;
                                    break;
                                }
            default           : throw USER_EXCEPTION2(SE1003, "unexpected case in build_PathExpr");
        }

        parent = cur;
    }

    return path_expr;
}

*/



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
