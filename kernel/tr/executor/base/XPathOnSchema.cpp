/*
 * File:  XPathOnSchema.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/node_utils.h"
#include "common/errdbg/d_printf.h"

#define PNK_ELEMENT				element
#define PNK_ATTRIBUTE			attribute


int compare_schema_node(const void *e1, const void *e2)
{
    return *(int*)e1 - *(int*)e2;
}

inline bool comp_type(schema_node* scm_node, t_item type)
{
    return comp_type(scm_node, NULL, NULL, type);
}

inline bool comp_local_type(const schema_node* scm_node, const char *ncn, t_item type)
{
    return comp_local_type(scm_node, NULL, ncn, type);
}

t_scmnodes_const descendant_or_self_nodes(const schema_node *node, node_type_restriction restriction, bool check = false)
{
    t_scmnodes_const res;
    if (!check || restriction(node->type))
            res.push_back(node);

    for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
        res = vector_concat(res, descendant_or_self_nodes((const schema_node*)ref->snode, restriction, true));

    return res;
}

t_scmnodes_const descendant_nodes(const schema_node *node, node_type_restriction restriction)
{
    t_scmnodes_const res;

    for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
        res = vector_concat(res, descendant_or_self_nodes((const schema_node*)(ref->snode), restriction, true));

    return res;
}


t_scmnodes_const execute_node_test_axis_child(const schema_node *node, const NodeTest& nt)
{
/// principle node kind for child axis
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	:
		{
			for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
				if (/*dm_children_accessor_filter(ref->type) && */is_pi(ref->type)) {res.push_back(ref->snode); break;}
            return res;
		}
    case node_test_comment					: 
		{
			for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
				if (/*dm_children_accessor_filter(ref->type) && */is_comment(ref->type)) {res.push_back(ref->snode);break;}
            return res;
		}
    case node_test_text						: 
        {
            for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
                if (/*dm_children_accessor_filter(ref->type) && */is_text(ref->type)) res.push_back(ref->snode);
            return res;
        }
    case node_test_node						: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (dm_children_accessor_filter(ref->type)/* && is_node(ref->type)*/) res.push_back(ref->snode);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis child");
    case node_test_qname					: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->snode, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (ref->type == PNK_ELEMENT) res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->snode, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->snode, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis child");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis child");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant(const schema_node *node, const NodeTest& nt)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	:
		{
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
				if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==pr_ins) res.push_back(*it);
            return res;
    }
    case node_test_comment					: 
	{
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
				if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==comment) res.push_back(*it);
            return res;
    }
    case node_test_text						:
        {
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_children_accessor_filter((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ELEMENT) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_attribute(const schema_node *node, const NodeTest& nt)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: return res;
    case node_test_node						: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (dm_attribute_accessor_filter(ref->type)) res.push_back(ref->snode);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis attribute");
    case node_test_qname					: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->snode, nt.data.uri, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (ref->type == PNK_ATTRIBUTE) res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->snode, nt.data.uri, NULL, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->snode, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis attribute");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis attribute");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_self(const schema_node *node, const NodeTest& nt)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: 
        {
            if (is_text(node->type)) res.push_back(node);
            return res;
        }
    case node_test_node						: 
        {
            if (is_node(node->type)) res.push_back(node);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis self");
    case node_test_qname					: 
        {
            if (comp_qname_type(node, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            if (node->type == PNK_ELEMENT) res.push_back(node);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            if (comp_uri_type(node, nt.data.uri, NULL, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            if (comp_local_type(node, nt.data.ncname_local, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis self");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis self");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant_or_self(const schema_node *node, const NodeTest& nt)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: 
		{
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==pr_ins) res.push_back(*it);
            return res;
        }
    case node_test_comment					: 
		{
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==comment) res.push_back(*it);
            return res;
        }
    case node_test_text						: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (true/*dm_children_accessor_filter((*it)->type)*/) res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant-or-self");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ELEMENT) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant-or-self");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant-or-self");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant_attr(const schema_node *node, const NodeTest& nt)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: return res;
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_attribute_accessor_filter((*it)->type)) 
                    res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant-attr");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ATTRIBUTE) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant-attr");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant-attr");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test(const schema_node *node, const NodeTest& nt)
{
    switch (nt.axis)
    {
    case axis_child				: return execute_node_test_axis_child(node, nt);
    case axis_descendant		: return execute_node_test_axis_descendant(node, nt);
    case axis_attribute			: return execute_node_test_axis_attribute(node, nt);
    case axis_self				: return execute_node_test_axis_self(node, nt);
    case axis_descendant_or_self: return execute_node_test_axis_descendant_or_self(node, nt);
    case axis_descendant_attr	: return execute_node_test_axis_descendant_attr(node, nt);
    case axis_parent			: throw USER_EXCEPTION2(SE1003, "XPath on Schema: parent axis is unsupported");
    default						: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unexpected axis");
    }
}

t_scmnodes_const execute_abs_path_expr_rec(const t_scmnodes_const &nodes, const PathExpr &pe)
{
    //d_printf1("PPAbsPath::execute_abs_path_expr_rec\n");
    t_scmnodes_const n1 = nodes, n2;

    for (int p = 0; p < pe.s; p++)
    {
        if (n1.size() == 0) return n1;
        const NodeTestOr &nto = pe.nto[p];

        int i, j;
        for (i = 0; i != n1.size(); i++)
            for (j = 0; j != nto.s; j++)
            {   
                t_scmnodes_const tmp = execute_node_test(n1.at(i), nto.nt[j]);
                n2 = vector_concat(n2, tmp);
            }

        n1 = n2;
        n2.clear();
    }

    return n1;
}

t_scmnodes_const execute_abs_path_expr(const schema_node *root, const PathExpr *path_expr)
{
    // Obtain scheme nodes, which satisfy path query
    t_scmnodes_const scmnodes;

    scmnodes.push_back(root);
    scmnodes = execute_abs_path_expr_rec(scmnodes, *path_expr);

    //d_printf2("PPAbsPath::execute_abs_path_expr: size of scmnodes %d\n", scmnodes.size());

    // Eliminate duplicates
    int ar_size = scmnodes.size();
    const schema_node** ar_scmnodes = se_new const schema_node*[ar_size];

    int i = 0;
    t_scmnodes_const::iterator snit;
    for (snit = scmnodes.begin(), i = 0; snit != scmnodes.end(); snit++, i++)
        ar_scmnodes[i] = *snit;

    qsort(ar_scmnodes, ar_size, sizeof(const schema_node*), compare_schema_node);

    scmnodes.clear();

    const schema_node* tmp = NULL;
    for (i = 0; i < ar_size; i++)
    {
        if (tmp == ar_scmnodes[i]) continue;
        
        scmnodes.push_back(ar_scmnodes[i]);
        tmp = ar_scmnodes[i];
    }

    delete ar_scmnodes;
    ar_scmnodes = NULL;

    return scmnodes;
}

t_scmnodes execute_abs_path_expr(schema_node *root, const PathExpr *path_expr)
{
    t_scmnodes_const tmp = execute_abs_path_expr((const schema_node *)root, path_expr);
    t_scmnodes res;
    res.reserve(tmp.size());
    for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
        res.push_back((schema_node*)(*it));
    return res;
}

#ifdef SE_ENABLE_TRIGGERS

bool operator < (schema_node sn1, schema_node sn2)
{
	if (sn1.type < sn2.type) return true;
	else return false;
}

t_scmnodes_const descendant_or_self_nodes(const schema_node *node, node_type_restriction restriction, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes, bool check = false)
{
    t_scmnodes_const res;
    if (!check || restriction(node->type))
            res.push_back(node);

    for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
        res = vector_concat(res, descendant_or_self_nodes((const schema_node*)ref->snode, restriction, extended_nodes, extender_nodes, true));

    //if node is in the extended nodes -> add extenders nodes to the result
    if (extended_nodes && extender_nodes)
        if (extended_nodes->find((schema_node*)node) != extended_nodes->end())
           for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
		   {
			   res = vector_concat(res, descendant_or_self_nodes((schema_node*)*i, restriction, extended_nodes, extender_nodes, true));
		   }
        
    return res;
}

t_scmnodes_const descendant_nodes(const schema_node *node, node_type_restriction restriction, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
        res = vector_concat(res, descendant_or_self_nodes((const schema_node*)(ref->snode), restriction, extended_nodes, extender_nodes, true));

    //if node is in the extended nodes -> add extenders nodes to the result
    if (extended_nodes && extender_nodes)
        if (extended_nodes->find((schema_node*)node) != extended_nodes->end())
            for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                 res = vector_concat(res, descendant_or_self_nodes((schema_node*)(*i), restriction, extended_nodes, extender_nodes, true));

    return res;
}

t_scmnodes_const execute_node_test_axis_child(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
/// principle node kind for child axis
    t_scmnodes_const res;
	t_scmnodes_set::iterator i;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: 
        {
            for (sc_ref *ref = node->first_child; ref != NULL; ref = ref->next)
                if (/*dm_children_accessor_filter(ref->type) && */is_text(ref->type)) res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (/*dm_children_accessor_filter(ref->type) && */is_text((*i)->type)) res.push_back(*i);
            return res;
        }
    case node_test_node						: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (dm_children_accessor_filter(ref->type)/* && is_node(ref->type)*/) res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (dm_children_accessor_filter((*i)->type)/* && is_node(ref->type)*/) res.push_back(*i);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis child");
    case node_test_qname					: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->snode, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_qname_type((*i), nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                        res.push_back(*i);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (ref->type == PNK_ELEMENT) res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if ((*i)->type == PNK_ELEMENT) res.push_back(*i);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->snode, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_uri_type((*i), nt.data.uri, NULL, PNK_ELEMENT))
                        res.push_back(*i);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->snode, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_local_type((*i), nt.data.ncname_local, PNK_ELEMENT))
                        res.push_back(*i);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis child");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis child");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: 
		{
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==pr_ins) res.push_back(*it);
            return res;
        }
    case node_test_comment					: 
		{
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==comment) res.push_back(*it);
            return res;
        }
    case node_test_text						:
        {
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_children_accessor_filter((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ELEMENT) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_attribute(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;
    t_scmnodes_set::iterator i;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: return res;
    case node_test_node						: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (dm_attribute_accessor_filter(ref->type)) res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (dm_attribute_accessor_filter((*i)->type)) res.push_back(*i);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis attribute");
    case node_test_qname					: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->snode, nt.data.uri, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_qname_type((*i), nt.data.uri, nt.data.ncname_local, PNK_ATTRIBUTE))
                        res.push_back(*i);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (ref->type == PNK_ATTRIBUTE) res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if ((*i)->type == PNK_ATTRIBUTE) res.push_back(*i);                
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->snode, nt.data.uri, NULL, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_uri_type((*i), nt.data.uri, NULL, PNK_ATTRIBUTE))
                        res.push_back(*i);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            for (sc_ref* ref = node->first_child; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->snode, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(ref->snode);
            if (extender_nodes)
                for (i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_local_type((*i), nt.data.ncname_local, PNK_ATTRIBUTE))
                        res.push_back(*i);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis attribute");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis attribute");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_self(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: 
        {
            if (is_text(node->type)) res.push_back(node);
            return res;
        }
    case node_test_node						: 
        {
            if (is_node(node->type)) res.push_back(node);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis self");
    case node_test_qname					: 
        {
            if (comp_qname_type(node, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            if (node->type == PNK_ELEMENT) res.push_back(node);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            if (comp_uri_type(node, nt.data.uri, NULL, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            if (comp_local_type(node, nt.data.ncname_local, PNK_ELEMENT))
                res.push_back(node);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis self");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis self");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant_or_self(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: 
		{
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==pr_ins) res.push_back(*it);
            return res;
        }
    case node_test_comment					: 
		{
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */(*it)->type==comment) res.push_back(*it);
            return res;
        }
    case node_test_text						: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (/*dm_children_accessor_filter((*it)->type) && */is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, /*is_node*/dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (true/*dm_children_accessor_filter((*it)->type)*/) res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant-or-self");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ELEMENT) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ELEMENT))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant-or-self");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant-or-self");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test_axis_descendant_attr(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_processing_instruction	: return res;
    case node_test_comment					: return res;
    case node_test_text						: return res;
    case node_test_node						: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_attribute_accessor_filter((*it)->type)) 
                    res.push_back(*it);
            return res;
        }
    case node_test_string					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_string on axis descendant-attr");
    case node_test_qname					: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.data.uri, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star			: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == PNK_ATTRIBUTE) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.data.uri, NULL, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname		: 
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, nt.data.ncname_local, PNK_ATTRIBUTE))
                    res.push_back(*it);
            return res;
        }
    case node_test_function_call			: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_function_call on axis descendant-attr");
    case node_test_var_name					: throw USER_EXCEPTION2(SE1002, "XPath on Schema: node_test_var_name on axis descendant-attr");
    default									: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unknown node test");
    }
}

t_scmnodes_const execute_node_test(const schema_node *node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    switch (nt.axis)
    {
    case axis_child				: return execute_node_test_axis_child(node, nt, extended_nodes, extender_nodes);
    case axis_descendant		: return execute_node_test_axis_descendant(node, nt, extended_nodes, extender_nodes);
    case axis_attribute			: return execute_node_test_axis_attribute(node, nt, extended_nodes, extender_nodes);
    case axis_self				: return execute_node_test_axis_self(node, nt, extended_nodes, extender_nodes);
    case axis_descendant_or_self: return execute_node_test_axis_descendant_or_self(node, nt, extended_nodes, extender_nodes);
    case axis_descendant_attr	: return execute_node_test_axis_descendant_attr(node, nt, extended_nodes, extender_nodes);
    case axis_parent			: throw USER_EXCEPTION2(SE1003, "XPath on Schema: parent axis is unsupported");
    default						: throw USER_EXCEPTION2(SE1003, "XPath on Schema: unexpected axis");
    }
}

t_scmnodes_const execute_abs_path_expr_rec(const t_scmnodes_const &nodes, const PathExpr &pe, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    //d_printf1("PPAbsPath::execute_abs_path_expr_rec\n");
    t_scmnodes_const n1 = nodes, n2;

    for (int p = 0; p < pe.s; p++)
    {
        if (n1.size() == 0) return n1;
        const NodeTestOr &nto = pe.nto[p];

        int i, j;
        for (i = 0; i != n1.size(); i++)
            for (j = 0; j != nto.s; j++)
            {
                t_scmnodes_const tmp;
                tmp = execute_node_test(n1.at(i), nto.nt[j], extended_nodes, extender_nodes);
                n2 = vector_concat(n2, tmp);
            }

        n1 = n2;
        n2.clear();
    }

    return n1;
}

t_scmnodes_const execute_abs_path_expr(const schema_node *root, const PathExpr *path_expr, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    // Obtain scheme nodes, which satisfy path query
    t_scmnodes_const scmnodes;

    scmnodes.push_back(root);
	scmnodes = execute_abs_path_expr_rec(scmnodes, *path_expr, extended_nodes, extender_nodes);

    //d_printf2("PPAbsPath::execute_abs_path_expr: size of scmnodes %d\n", scmnodes.size());

    // Eliminate duplicates
    int ar_size = scmnodes.size();
    const schema_node** ar_scmnodes = se_new const schema_node*[ar_size];

    int i = 0;
    t_scmnodes_const::iterator snit;
    for (snit = scmnodes.begin(), i = 0; snit != scmnodes.end(); snit++, i++)
        ar_scmnodes[i] = *snit;

    qsort(ar_scmnodes, ar_size, sizeof(const schema_node*), compare_schema_node);

    scmnodes.clear();

    const schema_node* tmp = NULL;
    for (i = 0; i < ar_size; i++)
    {
        if (tmp == ar_scmnodes[i]) continue;
        
        scmnodes.push_back(ar_scmnodes[i]);
        tmp = ar_scmnodes[i];
    }

    return scmnodes;
}

t_scmnodes execute_abs_path_expr(schema_node *root, const PathExpr *path_expr, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const tmp = execute_abs_path_expr((const schema_node *)root, path_expr, extended_nodes, extender_nodes);
    t_scmnodes res;
    res.reserve(tmp.size());
    for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
        res.push_back((schema_node*)(*it));
    return res;
}


#endif
