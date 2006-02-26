
#include "base.h"
#include "XPath.h"
#include "pers_heap.h"
#include "d_printf.h"
#include "exceptions.h"
#include "schema.h"
#include "PPBase.h"
#include <list>


using namespace std;

/// PathExpr memory management
typedef std::list<void*> mem_addr_list;

mem_addr_list local_addrs;
mem_addr_list pers_addrs;


static void *malloc_PathExpr(size_t size, bool persistent)
{
    void *p;
    if (persistent)
    {
        p = pers_malloc(size);
        pers_addrs.push_back(p);
    }
    else
    {
        p = malloc(size);
        local_addrs.push_back(p);
    }
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



/// PathExpr program logic
void set_NCName(NCName *n, const char* value, bool persistent)
{
    int size = strlen(value) + 1;
    n->n = (char*)malloc_PathExpr(size, persistent);
    strcpy(n->n, value);
}

void *create_PathExpr(const PathExprDistr &distr, bool persistent)
{
    PathExpr *path = (PathExpr*)malloc_PathExpr(sizeof(PathExpr), persistent);
    path->s = distr.size();
    if (path->s == 0) path->nto = NULL;
    else 
    {
        path->nto = (NodeTestOr*)malloc_PathExpr(sizeof(NodeTestOr) * path->s, persistent);

        for (int i = 0; i < path->s; i++)
        {
            NodeTestOr &nto = path->nto[i];
            nto.s = distr[i];
            nto.nt = (NodeTest*)malloc_PathExpr(sizeof(NodeTest) * nto.s, persistent);
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
                    case node_test_wildcard_star_ncname:
                        {
                            pers_free(nt.data.ncname.n);
                            break;
                        }
                    case node_test_qname:
                        {
                            pers_free(nt.data.qname.Prefix.n);
                            pers_free(nt.data.qname.LocalPart.n);
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

void NCName::print(std::ostream& str)
{
    str << n;	
}

void NCName::print_to_lr(std::ostream& str)
{
    str << "\"" << n << "\"";
}

void QName::print(std::ostream& str)
{
    if (strlen(Prefix.n) != 0)
    {
        Prefix.print(str);
		str << ":";
    }
    LocalPart.print(str);
}

void QName::print_to_lr(std::ostream& str)
{
    str << "(";
    Prefix.print_to_lr(str);
    str << " ";
    LocalPart.print_to_lr(str);
    str << ")";
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
        case node_test_qname                 : data.qname.print(str); break;
        case node_test_wildcard_star         : str << "*"; break;
        case node_test_wildcard_ncname_star  : data.ncname.print(str); str << ":*"; break;
        case node_test_wildcard_star_ncname  : str << "*:"; data.ncname.print(str); break;
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
        case node_test_qname                 : str << "qname "; data.qname.print_to_lr(str); break;
        case node_test_wildcard_star         : str << "wildcard_star ()"; break;
        case node_test_wildcard_ncname_star  : str << "wildcard_ncname_star "; data.ncname.print_to_lr(str); break;
        case node_test_wildcard_star_ncname  : str << "wildcard_star_ncname "; data.ncname.print_to_lr(str); break;
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

void set_node_test_parameters(variable_context *cxt,
                              scheme_list *lst, 
                              NodeTest &nt, //out parameter
                              bool persistent)
{
    if (   lst->size() != 3
        || lst->at(0).type != SCM_SYMBOL
        || lst->at(1).type != SCM_SYMBOL)
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

    string type = string(lst->at(1).internal.symb);
    if (type == "processing_instruction") nt.type = node_test_processing_instruction;
    else if (type == "comment") nt.type = node_test_comment;
    else if (type == "text") nt.type = node_test_text;
    else if (type == "node") nt.type = node_test_node;
    else if (type == "string") nt.type = node_test_string;
    else if (type == "qname") nt.type = node_test_qname;
    else if (type == "wildcard_star") nt.type = node_test_wildcard_star;
    else if (type == "wildcard_ncname_star") nt.type = node_test_wildcard_ncname_star;
    else if (type == "wildcard_star_ncname") nt.type = node_test_wildcard_star_ncname;
    else if (type == "function_call") nt.type = node_test_function_call;
    else if (type == "var_name") nt.type = node_test_var_name;
    else throw USER_EXCEPTION2(SE1004, "Path expression");


    if (nt.type == node_test_wildcard_ncname_star ||
        nt.type == node_test_wildcard_star_ncname)
    {
        if (lst->at(2).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        set_NCName(&(nt.data.ncname), lst->at(2).internal.str, persistent);
        return;
    }

    if (nt.type == node_test_qname)
    {
        if (   lst->at(2).type != SCM_LIST
            || lst->at(2).internal.list->size() != 2
            || lst->at(2).internal.list->at(0).type != SCM_STRING
            || lst->at(2).internal.list->at(1).type != SCM_STRING)
            throw USER_EXCEPTION2(SE1004, "Path expression");

        set_NCName(&(nt.data.qname.Prefix), lst->at(2).internal.list->at(0).internal.str, persistent);
        set_NCName(&(nt.data.qname.LocalPart), lst->at(2).internal.list->at(1).internal.str, persistent);
        return;
    }

    if (   nt.type == node_test_string
        || nt.type == node_test_function_call
        || nt.type == node_test_var_name)
    {
        if (lst->at(2).type != SCM_LIST)
            throw USER_EXCEPTION2(SE1004, "Path expression");

         nt.data.ppnode = NULL;//make_pp_op(cxt, lst->at(2).internal.list);
         return;
    }
}

PathExpr *lr2PathExpr(variable_context *cxt, scheme_list *path_lst, bool persistent)
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

            set_node_test_parameters(cxt, node_test_or_lst->at(j).internal.list, path_expr->nto[i].nt[j], persistent);
        }
    }

    return path_expr;
}

PathExpr *lr2PathExpr(variable_context *cxt, const char *str, bool persistent)
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

    PathExpr *path_expr = (PathExpr*)malloc_PathExpr(sizeof(PathExpr), false);
    path_expr->nto = (NodeTestOr*)malloc_PathExpr(sizeof(NodeTestOr) * scm_nodes.size(), false);
    path_expr->s = scm_nodes.size();

    int i = 0;
    schema_node *parent = from;
    for (i = 0; i < scm_nodes.size(); ++i)
    {
        cur = scm_nodes[scm_nodes.size() - i - 1];

        path_expr->nto[i].nt = (NodeTest*)malloc_PathExpr(sizeof(NodeTest), false);
        path_expr->nto[i].s = 1;

        if (cur->type == attribute)
            path_expr->nto[i].nt->axis = axis_attribute;
        else 
            path_expr->nto[i].nt->axis = axis_child;

        switch (cur->type)
        {
            case element      : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    set_NCName(&(path_expr->nto[i].nt->data.qname.Prefix), cur->name, false);
                                    set_NCName(&(path_expr->nto[i].nt->data.qname.LocalPart), cur->xmlns->uri, false);
                                    break;
                                }
            case text         : {
                                    path_expr->nto[i].nt->type = node_test_text;
                                    break;
                                }
            case attribute    : {
                                    path_expr->nto[i].nt->type = node_test_qname;
                                    set_NCName(&(path_expr->nto[i].nt->data.qname.Prefix), cur->name, false);
                                    set_NCName(&(path_expr->nto[i].nt->data.qname.LocalPart), cur->xmlns->uri, false);
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
