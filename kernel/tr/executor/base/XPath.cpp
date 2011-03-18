/*
 * File:  XPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <list>

#include "common/sedna.h"

#include "common/base.h"
#include "tr/executor/base/XPath.h"
#include "tr/executor/base/PPUtils.h"
#include "common/errdbg/d_printf.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
/// PathExpr program logic
///////////////////////////////////////////////////////////////////////////////
void * create_PathExpr(const PathExprDistr &distr, void * memory_parent)
{
    PathExpr *path = cat_malloc(memory_parent, sizeof(PathExpr));
    path->size = distr.size();
    if (path->size == 0) path->nto = NULL;
    else
    {
        path->nto = cat_malloc ( NodeTestOr*)mm->alloc(sizeof(NodeTestOr) * path->size);

        for (size_t i = 0; i < path->size; i++)
        {
            NodeTestOr &nto = path->nto[i];
            nto.size = distr[i];
            nto.nt = (NodeTest*)mm->alloc(sizeof(NodeTest) * nto.size);
        }
    }

    return path;
}

std::string
NodeTest::to_string(const NodeTestType& type, const NodeTestData& data)
{
    string res;
    switch (type)
    {
        case node_test_processing_instruction: res += "processing-instruction(" + xsd::NCName(data.ncname_local).;
                                               res += ;
                                               res += ")";
                                               break;

        case node_test_comment               : res += "comment()"; break;
        case node_test_text                  : res += "text()"; break;
        case node_test_node                  : res += "node()"; break;

        case node_test_element               : res += "element(";
                                               if (NULL != data.ncname_local)
                                                   res += xs_QName2string(data.ncname_prefix, data.ncname_local);
                                               res += ")";
                                               break;

        case node_test_attribute             : res += "attribute(";
                                               if (NULL != data.ncname_local)
                                                   res += xs_QName2string(data.ncname_prefix, data.ncname_local);
                                               res += ")";
                                               break;

        case node_test_document              : res += "document-node(";
                                               if (NULL != data.ncname_local)
                                               {
                                                   res += "element(";
                                                   res += xs_QName2string(data.ncname_prefix, data.ncname_local);
                                                   res += ")";
                                               }
                                               res += ")";
                                               break;

        case node_test_qname                 : res += xs_QName2string(data.ncname_prefix, data.ncname_local);
                                               break;

        case node_test_wildcard_star         : res += "*"; break;
        case node_test_wildcard_ncname_star  : res += xs_NCName2string(data.ncname_prefix); res += ":*"; break;
        case node_test_wildcard_star_ncname  : res += "*:"; res += xs_NCName2string(data.ncname_local); break;
        default                              : res += "UNKNOWN";
    }
    return res;
}

std::string NodeTest::to_string() const
{
    string res;
    switch (axis)
    {
        case axis_child             : res += "child"; break;
        case axis_descendant        : res += "descendant"; break;
        case axis_attribute         : res += "attribute"; break;
        case axis_self              : res += "self"; break;
        case axis_descendant_or_self: res += "descendant-or-self"; break;
        case axis_descendant_attr   : res += "descendant-attr"; break;
        case axis_parent            : res += "parent"; break;
        default                     : res += "UNKNOWN";
    }
    res += "::";
    res += NodeTest::to_string(type, data);
    return res;
}



void NodeTest::print_to_lr(std::ostream& str)
{
    str << "(";
    switch (axis)
    {
        case axis_child             : str << "PPAxisChild"; break;
        case axis_descendant        : str << "PPAxisDescendant"; break;
        case axis_attribute         : str << "PPAxisAttribute"; break;
        case axis_self              : str << "PPAxisSelf"; break;
        case axis_descendant_or_self: str << "PPAxisDescendantOrSelf"; break;
        case axis_descendant_attr   : str << "PPAxisDescendantAttr"; break;
        case axis_parent            : str << "PPAxisParent"; break;
        default                     : str << "UNKNOWN";
    }

    str << " ";

    switch (type)
    {
        case node_test_processing_instruction: if(NULL == data.ncname_local)
                                                   str << "processing-instruction ()";
                                               else
                                               {
                                                   str << "processing-instruction ";
                                                   xs_NCName_print_to_lr(data.ncname_local, str);
                                               }
                                               break;
        case node_test_comment               : str << "comment ()"; break;
        case node_test_text                  : str << "text ()"; break;
        case node_test_node                  : str << "node ()"; break;


        case node_test_element               : str << "element (";
                                               if(NULL != data.ncname_local)
                                                   xs_QName_print_to_lr(data.ncname_prefix,
                                                                        data.ncname_local,
                                                                        data.uri,
                                                                        str);
                                               str << ")";
                                               break;

        case node_test_attribute             : str << "attribute (";
                                               if(NULL != data.ncname_local)
                                                   xs_QName_print_to_lr(data.ncname_prefix,
                                                                        data.ncname_local,
                                                                        data.uri,
                                                                        str);
                                               str << ")";
                                               break;

        case node_test_document              : str << "document (";
                                               if(NULL != data.ncname_local)
                                                   xs_QName_print_to_lr(data.ncname_prefix,
                                                                        data.ncname_local,
                                                                        data.uri,
                                                                        str);
                                               str << ")";
                                               break;

        case node_test_qname                 : str << "qname (";
                                               xs_QName_print_to_lr(data.ncname_prefix,
                                                                    data.ncname_local,
                                                                    data.uri,
                                                                    str);
                                               str << ")";
                                               break;

        case node_test_wildcard_star         : str << "wildcard_star ()"; break;
        case node_test_wildcard_ncname_star  : str << "wildcard_ncname_star ";
                                               xs_anyURI_print_to_lr(data.uri, str);
                                               break;

        case node_test_wildcard_star_ncname  : str << "wildcard_star_ncname ";
                                               xs_NCName_print_to_lr(data.ncname_local, str);
                                               break;

        default                              : str << "UNKNOWN";
    }
    str << ")";
}

std::string NodeTestOr::to_string() const
{
    string res ;
    if (size == 1)
    {
        res += nt[0].to_string();
    }
    else
    {
        size_t i = 0;
        res += "(";
        res += nt[0].to_string();
        for (i = 1; i < size; i++)
        {
            res += " | ";
            res += nt[i].to_string();
        }
        res += ")";
    }
    return res;
}

void NodeTestOr::print_to_lr(std::ostream& str)
{
    str << "(";
    size_t i = 0;
    nt[0].print_to_lr(str);
    for (i = 1; i < size; i++)
    {
        str << " ";
        nt[i].print_to_lr(str);
    }
    str << ")";
}

std::string PathExpr::to_string() const
{
    string res;
    size_t i = 0;
    if (size > 0)
    {
        res += nto[0].to_string();
        for (i = 1; i < size; i++)
        {
            res += "/";
            res += nto[i].to_string();
        }
    }
    return res;
}

void PathExpr::print_to_lr(std::ostream& str)
{
    str << "(";
    if (size > 0)
    {
        size_t i = 0;
        nto[0].print_to_lr(str);
        for (i = 1; i < size; i++)
        {
            str << " ";
            nto[i].print_to_lr(str);
        }
    }
    str << ")";
}

void PathExpr2lr(PathExpr *path, std::ostream& str)
{
    path->print_to_lr(str);
}

void static inline
set_node_test_QName_data(scheme_list *lst,
                         NodeTestData &nt_data,
                         PathExprMemoryManager * mm)
{
    if (lst->size() != 3
        || lst->at(0).type != SCM_STRING
        || lst->at(1).type != SCM_STRING
        || lst->at(2).type != SCM_STRING)
        throw USER_EXCEPTION2(SE1004, "Path expression");

    if (*(lst->at(0).internal.str))
            nt_data.uri = xs_anyURI_create(lst->at(0).internal.str, mm->alloc);

    nt_data.ncname_local  = xs_NCName_create(lst->at(1).internal.str, mm->alloc);
    if (*(lst->at(2).internal.str))
        nt_data.ncname_prefix = xs_NCName_create(lst->at(2).internal.str, mm->alloc);
}

void set_node_test_type_and_data(scheme_list *lst,
                                 NodeTestType &nt_type, /* out */
                                 NodeTestData &nt_data, /* out */
                                 PathExprMemoryManager * mm)
{
    if (lst->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "Path expression");

    string type = string(lst->at(1).internal.symb);
    if (type == "processing-instruction")    nt_type = node_test_processing_instruction;
    else if (type == "comment")              nt_type = node_test_comment;
    else if (type == "text")                 nt_type = node_test_text;
    else if (type == "node")                 nt_type = node_test_node;
    else if (type == "element")              nt_type = node_test_element;
    else if (type == "attribute")            nt_type = node_test_attribute;
    else if (type == "document")             nt_type = node_test_document;
    else if (type == "qname")                nt_type = node_test_qname;
    else if (type == "wildcard_star")        nt_type = node_test_wildcard_star;
    else if (type == "wildcard_ncname_star") nt_type = node_test_wildcard_ncname_star;
    else if (type == "wildcard_star_ncname") nt_type = node_test_wildcard_star_ncname;
    else throw USER_EXCEPTION2(SE1004, "Path expression");

    nt_data.uri           = NULL;
    nt_data.ncname_prefix = NULL;
    nt_data.ncname_local  = NULL;

    switch(nt_type)
    {
        case node_test_wildcard_ncname_star:
        {
            if (lst->at(2).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Path expression");
                nt_data.uri = xs_anyURI_create(lst->at(2).internal.str, mm->alloc);
            break;
        }

        case node_test_wildcard_star_ncname:
        {
            if (lst->at(2).type != SCM_STRING)
                throw USER_EXCEPTION2(SE1004, "Path expression");

            nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, mm->alloc);
            break;
        }

        case node_test_document:
        case node_test_attribute:
        case node_test_element:
        {
            if (  lst->at(2).type != SCM_LIST &&
                  ( lst->at(2).internal.list->size() != 3 ||
                    lst->at(2).internal.list->size() != 0 ))
                throw USER_EXCEPTION2(SE1004, "110.1");

            if(lst->at(2).internal.list->size() != 0) {
                scheme_list *qname_lst = lst->at(2).internal.list;

                set_node_test_QName_data(qname_lst, nt_data, mm);
            }
            break;
        }

        case node_test_qname:
        {
            if (lst->size() < 3
                || lst->at(2).type != SCM_LIST
                || lst->at(2).internal.list->size() != 3)
                throw USER_EXCEPTION2(SE1004, "node_test_qname");

            set_node_test_QName_data(lst->at(2).internal.list, nt_data, mm);
            break;
        }

        case node_test_processing_instruction:
        {
            if (   lst->at(2).type == SCM_LIST
                && lst->at(2).internal.list->size() == 0)
            {
                nt_data.ncname_local  = NULL;
            }
            else if (lst->at(2).type == SCM_STRING)
            {
                nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, mm->alloc);
            }
            else throw USER_EXCEPTION2(SE1004, "110");
            break;
        }

        default: break;
    }
}

void set_node_test_parameters(scheme_list *lst,
                              NodeTest &nt, //out parameter
                              PathExprMemoryManager * mm)
{
    if (   lst->size() != 3
        || lst->at(0).type != SCM_SYMBOL)
    throw USER_EXCEPTION2(SE1004, "Path expression");

    string axis = string(lst->at(0).internal.symb);
    if (axis == "PPAxisChild") nt.axis = axis_child;
    else if (axis == "PPAxisAttribute") nt.axis = axis_attribute;
    else if (axis == "PPAxisParent") nt.axis = axis_parent;
    else if (axis == "PPAxisSelf") nt.axis = axis_self;
    else if (axis == "PPAxisDescendant") nt.axis = axis_descendant;
    else if (axis == "PPAxisDescendantOrSelf") nt.axis = axis_descendant_or_self;
    else if (axis == "PPAxisDescendantAttr") nt.axis = axis_descendant_attr;
    else throw USER_EXCEPTION2(SE1004, "Path expression");

    set_node_test_type_and_data(lst, nt.type, nt.data, mm);
}

PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, void * memory_parent)
{
    size_t i = 0, j = 0;
    PathExprDistr distr(path_lst->size(), 0);

    for (i = 0; i < path_lst->size(); i++)
    {
        if (   path_lst->at(i).type != SCM_LIST
            || path_lst->at(i).internal.list->size() < 1)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        // node-test-or
        scheme_list* node_test_or_lst = path_lst->at(i).internal.list;
        distr[i] = node_test_or_lst->size();
    }

    PathExpr *path_expr = (PathExpr*)create_PathExpr(distr, memory_parent);

    for (i = 0; i < path_lst->size(); i++)
    {
        // node-test-or
        scheme_list* node_test_or_lst = path_lst->at(i).internal.list;

        for (j = 0; j < node_test_or_lst->size(); j++)
        {
            if (node_test_or_lst->at(j).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "Path expression");

            set_node_test_parameters(node_test_or_lst->at(j).internal.list, path_expr->nto[i].nt[j], mm);
        }
    }

    return path_expr;
}

PathExpr *lr2PathExpr(dynamic_context *cxt, const char *str, PathExprMemoryManager * mm)
{
    scheme_list *lst = make_tree_from_scheme_list(str);
    PathExpr *path = lr2PathExpr(cxt, lst, mm);
    delete_scheme_list(lst); // !!! free memory in case of exception too

    return path;
}

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

////////////////////////////////////////////////////////////////////////////////
/// PathExprRoot - manages document or collection PathExpr is evaluated on
////////////////////////////////////////////////////////////////////////////////

void PathExprRoot::release()
{
    if(name.op != NULL)
    {
        delete name.op;
        name.op = NULL;
    }
}

void PathExprRoot::open()
{
    if(name.op != NULL)
    {
        name.op->open();
    }
}

void PathExprRoot::reopen()
{
    if(name.op != NULL)
    {
        name.op->reopen();
    }
}

void PathExprRoot::close()
{
    if(name.op != NULL)
    {
        name.op->close();
    }
}

const counted_ptr<db_entity>& PathExprRoot::get_entity(const char* obj_name,
                                                       const char* op_name)
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
