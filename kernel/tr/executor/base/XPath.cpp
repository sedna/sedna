/*
 * File:  XPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <list>

#include "common/sedna.h"

#include "common/base.h"
#include "tr/executor/base/XPath.h"
#include "common/errdbg/d_printf.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"

using namespace std;

////////////////////////////////////////////////////////////////////////////////
/// PathExpr memory management
////////////////////////////////////////////////////////////////////////////////

FastPointerArray pe_local_heap_warden;
FastPointerArray pe_catalog_heap_warden;

void * pe_malloc(size_t size)
{
    return pe_local_heap_warden.add(cat_malloc(NULL, size));
};

void pe_free(void *) { };

void pe_free_all()
{
    pe_local_heap_warden.freeAll();
    pe_local_heap_warden.clear();
};

void * cat_pe_malloc(size_t size)
{
    return pe_catalog_heap_warden.add(cat_malloc(NULL, size));
};

void cat_pe_free(void *) { };

void cat_pe_free_all()
{
    pe_catalog_heap_warden.freeAll();
    pe_catalog_heap_warden.clear();
};

PathExprMemoryManager pe_local_memory_manager = { pe_malloc, pe_free, pe_free_all, NULL };
PathExprMemoryManager pe_catalog_memory_manager = { cat_pe_malloc, cat_pe_free, cat_pe_free_all, NULL };

PathExprMemoryManager * pe_local_aspace = &pe_local_memory_manager;
PathExprMemoryManager * pe_catalog_aspace = &pe_catalog_memory_manager;

///////////////////////////////////////////////////////////////////////////////
/// PathExpr program logic
///////////////////////////////////////////////////////////////////////////////
void *create_PathExpr(const PathExprDistr &distr, PathExprMemoryManager * mm)
{
    PathExpr *path = (PathExpr*)mm->alloc(sizeof(PathExpr));
    path->s = distr.size();
    if (path->s == 0) path->nto = NULL;
    else 
    {
        path->nto = (NodeTestOr*)mm->alloc(sizeof(NodeTestOr) * path->s);

        for (int i = 0; i < path->s; i++)
        {
            NodeTestOr &nto = path->nto[i];
            nto.s = distr[i];
            nto.nt = (NodeTest*)mm->alloc(sizeof(NodeTest) * nto.s);
        }
    }

    return path;
}

void delete_PathExpr(PathExpr *path)
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
                        xs_NCName_release(nt.data.ncname_prefix, pe_free);
                        nt.data.ncname_prefix = NULL;
                        xs_anyURI_release(nt.data.uri, pe_free);
                        nt.data.uri = NULL;
                        break;
                    }
                case node_test_wildcard_star_ncname:
                    {
                        xs_NCName_release(nt.data.ncname_local, pe_free);
                        nt.data.ncname_local = NULL;
                        break;
                    }
                case node_test_qname:
                    {
                        xs_NCName_release(nt.data.ncname_prefix, pe_free);
                        nt.data.ncname_prefix = NULL;
                        xs_anyURI_release(nt.data.uri, pe_free);
                        nt.data.uri = NULL;
                        xs_NCName_release(nt.data.ncname_local, pe_free);
                        nt.data.ncname_local = NULL;
                        break;
                    }
                default: ;
            }
        }
        pe_free(nto.nt);
    }
    pe_free(path->nto);
    pe_free(path);
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
                                 PathExprMemoryManager * mm)
{
    if (lst->at(1).type != SCM_SYMBOL)
        throw USER_EXCEPTION2(SE1004, "Path expression");

    string type = string(lst->at(1).internal.symb);
    if (type == "processing-instruction") nt_type = node_test_processing_instruction;
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
            nt_data.uri = xs_anyURI_create(lst->at(2).internal.str, mm->alloc);
        return;
    }

    if (nt_type == node_test_wildcard_star_ncname)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, mm->alloc);
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
                nt_data.uri = xs_anyURI_create(lst->at(2).internal.list->at(0).internal.str, mm->alloc);

        nt_data.ncname_local  = xs_NCName_create(lst->at(2).internal.list->at(1).internal.str, mm->alloc);
        if (*(lst->at(2).internal.list->at(2).internal.str))
            nt_data.ncname_prefix = xs_NCName_create(lst->at(2).internal.list->at(2).internal.str, mm->alloc);
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
            nt_data.ncname_local = xs_NCName_create(lst->at(2).internal.str, mm->alloc);
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

PathExpr *lr2PathExpr(dynamic_context *cxt, scheme_list *path_lst, PathExprMemoryManager * mm)
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

    PathExpr *path_expr = (PathExpr*)create_PathExpr(distr, mm);

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
    path_expr->s = scm_nodes.size();

    int i = 0;
    schema_node_cptr parent = from;
    for (i = 0; i < scm_nodes.size(); ++i)
    {
        cur = scm_nodes[scm_nodes.size() - i - 1];

        path_expr->nto[i].nt = (NodeTest*)mm->alloc(sizeof(NodeTest));
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
