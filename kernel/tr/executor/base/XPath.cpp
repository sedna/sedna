/*
 * File:  XPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <list>

#include "common/sedna.h"

#include "common/base.h"
#include "tr/executor/base/XPath.h"
#include "common/ph/pers_heap.h"
#include "common/errdbg/d_printf.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"



using namespace std;

////////////////////////////////////////////////////////////////////////////////
/// PathExpr memory management
////////////////////////////////////////////////////////////////////////////////
typedef std::list<void*> mem_addr_list;

static mem_addr_list local_addrs;
static mem_addr_list pers_addrs;


void *PathExpr_mem_alloc(size_t size)
{
    void *p = malloc(size);
    local_addrs.push_back(p);
    return p;
}

void *PathExpr_pers_alloc(size_t size)
{
    void *p = pers_malloc(size);
    pers_addrs.push_back(p);
    return p;
}

void PathExpr_local_free()
{
    void *p = NULL;
    for (mem_addr_list::iterator it = local_addrs.begin(); it != local_addrs.end(); it++) 
    {
        p = *it;
        free(p);
    }
    local_addrs.clear();
}

void PathExpr_pers_free()
{
    void *p = NULL;
    for (mem_addr_list::iterator it = pers_addrs.begin(); it != pers_addrs.end(); it++) 
    {
        p = *it;
        pers_free(*it);
    }
    pers_addrs.clear();
}

void PathExpr_reset_pers()
{
    pers_addrs.clear();
}


///////////////////////////////////////////////////////////////////////////////
/// PathExpr program logic
///////////////////////////////////////////////////////////////////////////////
void *create_PathExpr(const PathExprDistr &distr, bool persistent)
{
    PathExpr *path = (PathExpr*)PathExpr_malloc(sizeof(PathExpr), persistent);
    path->s = distr.size();
    if (path->s == 0) path->nto = NULL;
    else 
    {
        path->nto = (NodeTestOr*)PathExpr_malloc(sizeof(NodeTestOr) * path->s, persistent);

        for (int i = 0; i < path->s; i++)
        {
            NodeTestOr &nto = path->nto[i];
            nto.s = distr[i];
            nto.nt = (NodeTest*)PathExpr_malloc(sizeof(NodeTest) * nto.s, persistent);
        }
    }

    return path;
}

void delete_PathExpr(PathExpr *path)
{
    if (IS_PH_PTR((void*)path))
    {
        for (int i = 0; i < path->s; i++)
        {
            NodeTestOr &nto = path->nto[i];
            for (int j = 0; j < nto.s; j++)
            {
                NodeTest &nt = nto.nt[j];

                switch (nt.type)
                {
                    case node_test_wildcard_ncname_star:
                        {
                            xs_NCName_release(nt.data.ncname_prefix, pers_free);
                            nt.data.ncname_prefix = NULL;
                            xs_anyURI_release(nt.data.uri, pers_free);
                            nt.data.uri = NULL;
                            break;
                        }
                    case node_test_wildcard_star_ncname:
                        {
                            xs_NCName_release(nt.data.ncname_local, pers_free);
                            nt.data.ncname_local = NULL;
                            break;
                        }
                    case node_test_qname:
                        {
                            xs_NCName_release(nt.data.ncname_prefix, pers_free);
                            nt.data.ncname_prefix = NULL;
                            xs_anyURI_release(nt.data.uri, pers_free);
                            nt.data.uri = NULL;
                            xs_NCName_release(nt.data.ncname_local, pers_free);
                            nt.data.ncname_local = NULL;
                            break;
                        }
                    default: ;
                }
            }
            pers_free(nto.nt);
        }
        pers_free(path->nto);
        pers_free(path);
    }
    else
    {
        throw USER_EXCEPTION2(SE1003, "delete_PathExpr is not implemented for dynamic memory");
    }
}


void NodeTest::print(std::ostream& str)
{
    switch (axis)
    {
        case axis_child             : str << "child"; break;
        case axis_descendant        : str << "descendant"; break;
        case axis_attribute         : str << "attribute"; break;
        case axis_self              : str << "self"; break;
        case axis_descendant_or_self: str << "descendant-or-self"; break;
        case axis_descendant_attr   : str << "descendant-attr"; break;
        case axis_parent            : str << "parent"; break;
        default                     : str << "UNKNOWN";
    }

    str << "::";

    switch (type)
    {
        case node_test_processing_instruction: str << "processing-instruction()"; break;
        case node_test_comment               : str << "comment()"; break;
        case node_test_text                  : str << "text()"; break;
        case node_test_node                  : str << "node()"; break;
        case node_test_string                : str << "[string]"; break;
        case node_test_qname                 : if(data.ncname_prefix && *data.ncname_prefix) 
                                               {   
                                                  xs_NCName_print(data.ncname_prefix, str); 
                                                  str << ":";
                                               }
                                               xs_NCName_print(data.ncname_local, str); 
                                               break;
        case node_test_wildcard_star         : str << "*"; break;
        case node_test_wildcard_ncname_star  : xs_NCName_print(data.ncname_prefix, str); str << ":*"; break;
        case node_test_wildcard_star_ncname  : str << "*:"; xs_NCName_print(data.ncname_local, str); break;
        case node_test_function_call         : str << "[function]"; break;
        case node_test_var_name              : str << "[var]"; break;
        default                              : str << "UNKNOWN";
    }
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
        case node_test_processing_instruction: str << "processing-instruction ()"; break;
        case node_test_comment               : str << "comment ()"; break;
        case node_test_text                  : str << "text ()"; break;
        case node_test_node                  : str << "node ()"; break;
        case node_test_string                : str << "string ()"; break;
        case node_test_qname                 : str << "qname "; 
                                               str << "(";
                                               xs_anyURI_print_to_lr(data.uri, str);
                                               str << " ";
                                               xs_NCName_print_to_lr(data.ncname_local, str); 
                                               str << " ";
                                               xs_NCName_print_to_lr(data.ncname_prefix, str); 
                                               str << ")";
                                               break;
        case node_test_wildcard_star         : str << "wildcard_star ()"; break;
        case node_test_wildcard_ncname_star  : str << "wildcard_ncname_star "; 
                                               xs_anyURI_print_to_lr(data.uri, str); 
                                               break;
        case node_test_wildcard_star_ncname  : str << "wildcard_star_ncname "; 
                                               xs_NCName_print_to_lr(data.ncname_local, str); 
                                               break;
        case node_test_function_call         : str << "function_call ()"; break;
        case node_test_var_name              : str << "var_name ()"; break;
        default                              : str << "UNKNOWN";
    }
    str << ")";
}

void NodeTestOr::print(std::ostream& str)
{
    if (s == 1)
    {
        nt[0].print(str);
    }
    else
    {
        int i = 0;
        str << "(";
        nt[0].print(str);
        for (i = 1; i < s; i++)
        {
            str << " | ";
            nt[i].print(str);
        }
        str << ")";
    }
}

void NodeTestOr::print_to_lr(std::ostream& str)
{
    str << "(";
    int i = 0;
    nt[0].print_to_lr(str);
    for (i = 1; i < s; i++)
    {
        str << " ";
        nt[i].print_to_lr(str);
    }
    str << ")";
}

void PathExpr::print(std::ostream& str)
{
    int i = 0;
    if (s > 0)
    {
        nto[0].print(str);
        for (i = 1; i < s; i++)
        {
            str << "/";
            nto[i].print(str);
        }
    }
}

void PathExpr::print_to_lr(std::ostream& str)
{
    str << "(";
    if (s > 0)
    {
        int i = 0;
        nto[0].print_to_lr(str);
        for (i = 1; i < s; i++)
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

void set_node_test_type_and_data(scheme_list *lst, 
                                 NodeTestType &nt_type, //out parameter
                                 NodeTestData &nt_data, //out parameter
                                 bool persistent)
{
    if (lst->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "Path expression");

    string type = string(lst->at(1).internal.symb);
    if (type == "processing_instruction") nt_type = node_test_processing_instruction;
    else if (type == "comment") nt_type = node_test_comment;
    else if (type == "text") nt_type = node_test_text;
    else if (type == "node") nt_type = node_test_node;
    else if (type == "string") nt_type = node_test_string;
    else if (type == "qname") nt_type = node_test_qname;
    else if (type == "wildcard_star") nt_type = node_test_wildcard_star;
    else if (type == "wildcard_ncname_star") nt_type = node_test_wildcard_ncname_star;
    else if (type == "wildcard_star_ncname") nt_type = node_test_wildcard_star_ncname;
    else if (type == "function_call") nt_type = node_test_function_call;
    else if (type == "var_name") nt_type = node_test_var_name;
    else throw USER_EXCEPTION2(SE1004, "Path expression");

    nt_data.uri           = NULL;
    nt_data.ncname_prefix = NULL;
    nt_data.ncname_local  = NULL;

    if (nt_type == node_test_wildcard_ncname_star)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        if (strcmp(lst->at(2).internal.str, "http://www.w3.org/XML/1998/namespace") !=0)
            nt_data.uri = xs_anyURI_create(lst->at(2).internal.str, PathExpr_malloc_func(persistent));
        return;
    }

    if (nt_type == node_test_wildcard_star_ncname)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, PathExpr_malloc_func(persistent));
        return;
    }

    if (nt_type == node_test_qname)
    {
        if (   lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 3
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING
            || lst->at(2).internal.list->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        if (*(lst->at(2).internal.list->at(0).internal.str))
            if (strcmp(lst->at(2).internal.list->at(0).internal.str, "http://www.w3.org/XML/1998/namespace") != 0)
                nt_data.uri = xs_anyURI_create(lst->at(2).internal.list->at(0).internal.str, PathExpr_malloc_func(persistent));

        nt_data.ncname_local  = xs_NCName_create(lst->at(2).internal.list->at(1).internal.str, PathExpr_malloc_func(persistent));
        if (*(lst->at(2).internal.list->at(2).internal.str))
            nt_data.ncname_prefix = xs_NCName_create(lst->at(2).internal.list->at(2).internal.str, PathExpr_malloc_func(persistent));
    }

    if (nt_type == node_test_processing_instruction)
    {
        if (   lst->at(2).type == SCM_LIST
            && lst->at(2).internal.list->size() == 0)
        {
            nt_data.ncname_local  = NULL;
        }
        else if (lst->at(2).type == SCM_STRING)
        {
            nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, PathExpr_malloc_func(persistent));
        }
        else throw USER_EXCEPTION2(SE1004, "110");

        return;
    }

    if (   nt_type == node_test_string
        || nt_type == node_test_function_call
        || nt_type == node_test_var_name)
    {
        if (lst->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "112");

         nt_data.ppnode = NULL;
         return;
    }
}

void set_node_test_parameters(scheme_list *lst, 
                              NodeTest &nt, //out parameter
                              bool persistent)
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

    set_node_test_type_and_data(lst, nt.type, nt.data, persistent);
}

PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, bool persistent)
{
    int i = 0, j = 0;
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

    PathExpr *path_expr = (PathExpr*)create_PathExpr(distr, persistent);

    for (i = 0; i < path_lst->size(); i++)
    {
        // node-test-or
        scheme_list* node_test_or_lst = path_lst->at(i).internal.list;

        for (j = 0; j < node_test_or_lst->size(); j++)
        {
            if (node_test_or_lst->at(j).type != SCM_LIST)
                throw USER_EXCEPTION2(SE1004, "Path expression");

            set_node_test_parameters(node_test_or_lst->at(j).internal.list, path_expr->nto[i].nt[j], persistent);
        }
    }

    return path_expr;
}

PathExpr *lr2PathExpr(dynamic_context *cxt, const char *str, bool persistent)
{
    scheme_list *lst = make_tree_from_scheme_list(str);
    PathExpr *path = lr2PathExpr(cxt, lst, persistent);
    delete_scheme_list(lst); // !!! free memory in case of exception too

    return path;
}

PathExpr *build_PathExpr(schema_node *from, schema_node *to)
{
    std::vector<schema_node*> scm_nodes;
    schema_node *cur = to;
    while (cur != from)
    {
        scm_nodes.push_back(cur);
        cur = cur->parent;
    }

    PathExpr *path_expr = (PathExpr*)PathExpr_malloc(sizeof(PathExpr), false);
    path_expr->nto = (NodeTestOr*)PathExpr_malloc(sizeof(NodeTestOr) * scm_nodes.size(), false);
    path_expr->s = scm_nodes.size();

    int i = 0;
    schema_node *parent = from;
    for (i = 0; i < scm_nodes.size(); ++i)
    {
        cur = scm_nodes[scm_nodes.size() - i - 1];

        path_expr->nto[i].nt = (NodeTest*)PathExpr_malloc(sizeof(NodeTest), false);
        path_expr->nto[i].s = 1;

        if (cur->type == attribute)
            path_expr->nto[i].nt->axis = axis_attribute;
        else 
            path_expr->nto[i].nt->axis = axis_child;

        switch (cur->type)
        {
            case element      : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    // FIXME:
                                    path_expr->nto[i].nt->data.ncname_prefix = xs_NCName_create(cur->name, PathExpr_malloc_func(false));
                                    path_expr->nto[i].nt->data.ncname_local =  xs_NCName_create(cur->xmlns->uri, PathExpr_malloc_func(false));
                                    break;
                                }
            case text         : {
                                    path_expr->nto[i].nt->type = node_test_text;
                                    break;
                                }
            case attribute    : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    // FIXME:
                                    path_expr->nto[i].nt->data.ncname_prefix = xs_NCName_create(cur->name, PathExpr_malloc_func(false));
                                    path_expr->nto[i].nt->data.ncname_local =  xs_NCName_create(cur->xmlns->uri, PathExpr_malloc_func(false));
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
