/*
 * File:  XPathOnSchema.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/dm_accessors.h"
#include "common/errdbg/d_printf.h"

#include "tr/structures/nodetypes.h"

///////////////////////////////////////////////////////////////////////////////
/// Some usefull static helpers
///////////////////////////////////////////////////////////////////////////////

using namespace xpath;

typedef bool (*node_type_restriction)(t_item);

inline static
bool is_text(t_item t)
{
    return t == text;
}

inline static
bool is_pi(t_item t)
{
    return t == pr_ins;
}

inline static
bool is_node(t_item t)
{
    return (   t == element
            || t == text
            || t == attribute
            || t == document
            || t == xml_namespace
            || t == comment
            || t == pr_ins);
}

inline static
bool is_comment(t_item t)
{
    return t == comment;
}

inline static
bool is_document(t_item t)
{
    return t == document;
}

static inline int compare_schema_node(const void *e1, const void *e2)
{
    schema_node_xptr v1 = *(schema_node_xptr*)e1;
    schema_node_xptr v2 = *(schema_node_xptr*)e2;
    if ( v1 == v2 ) return 0;
    else if ( v1 < v2 ) return -1;
    return 1;
}

static t_scmnodes_const descendant_nodes(schema_node_cptr node,
                                         node_type_restriction restriction,
                                         t_scmnodes_set* extended_nodes,
                                         t_scmnodes_set* extender_nodes)
{

    t_scmnodes_const res;

    /* Recursively get actual descendants of this node */
    for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
    {
        if (restriction(ref->object.snode->type))
            res.push_back(ref->object.snode);

        res = vector_concat(res,
                            descendant_nodes(ref->object.snode,
                                             restriction,
                                             extended_nodes,
                                             extender_nodes));
    }

    /*
     * If node is in the extended nodes -> add extenders nodes to the result.
     * We consider extenders just like actual children in this case.
     */
    if (extended_nodes &&
        extender_nodes &&
        extended_nodes->find(node.ptr()) != extended_nodes->end())
    {
        for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
        {
            if (restriction((*i)->type)) res.push_back(*i);

            res = vector_concat(res,
                                descendant_nodes(*i,
                                                 restriction,
                                                 extended_nodes,
                                                 extender_nodes));
        }
    }
    return res;
}

static inline t_scmnodes_const descendant_or_self_nodes(schema_node_cptr node,
                                          node_type_restriction restriction,
                                          t_scmnodes_set* extended_nodes,
                                          t_scmnodes_set* extender_nodes,
                                          bool check = false)
{
    t_scmnodes_const res;

    /* Check and push back node itself */
    if (!check || restriction(node->type))
        res.push_back(node.ptr());

    /* Get all descendants */
    res = vector_concat(res,
                        descendant_nodes(node,
                                         restriction,
                                         extended_nodes,
                                         extender_nodes));

    return res;
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Child Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_child(schema_node_cptr node,
                             const NodeTest& nt,
                             t_scmnodes_set* extended_nodes,
                             t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;
    NodeTestType type = nt.type;

    if (type == node_test_element)
        type = (nt.local == NULL ? node_test_wildcard_star : node_test_qname);

    bool extend = extended_nodes &&
                  extender_nodes &&
                  extended_nodes->find(node.ptr()) != extended_nodes->end();

    switch (type)
    {
    case node_test_pi   :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (is_pi(ref->object.type) &&
                      (NULL == nt.local ||
                       comp_local_type(ref->object.snode, NULL, nt.local, pr_ins)))
                {
                    res.push_back(ref->object.snode);
                    break;
                }

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i != extender_nodes->end(); i++)
                    if (is_pi((*i)->type) &&
                          (NULL == nt.local ||
                           comp_local_type(*i, NULL, nt.local, pr_ins)))
                    {
                        res.push_back(*i);
                        break;
                    }

            }
            return res;
        }
    case node_test_comment                  :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (is_comment(ref->object.type)) {res.push_back(ref->object.snode);break;}

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (is_comment((*i)->type)) {res.push_back(*i); break;}
            }

            return res;
        }
    case node_test_text                     :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (is_text(ref->object.type)) res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (is_text((*i)->type)) res.push_back(*i);
            }

            return res;
        }
    case node_test_node                     :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (dm_children_accessor_filter(ref->object.type)) res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (dm_children_accessor_filter((*i)->type)) res.push_back(*i);
            }

            return res;
        }
    case node_test_qname                    :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->object.snode, nt.uri, nt.local, element))
                    res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_qname_type(*i, nt.uri, nt.local, element))
                        res.push_back(*i);
            }

            return res;
        }
    case node_test_wildcard_star            :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (ref->object.type == element) res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (dm_children_accessor_filter((*i)->type)) res.push_back(*i);
            }

            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->object.snode, nt.uri, NULL, element))
                    res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_uri_type(*i, nt.uri, NULL, element))
                        res.push_back(*i);
            }

            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->object.snode, NULL, nt.local, element))
                    res.push_back(ref->object.snode);

            if (extend)
            {
                for (t_scmnodes_set::iterator i=extender_nodes->begin(); i!=extender_nodes->end(); i++)
                    if (comp_local_type(*i, NULL, nt.local, element))
                        res.push_back(*i);
            }

            return res;
        }
    case node_test_attribute                :
    case node_test_document                 :
       return res;

    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: child axis unknown node test");
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Descendant Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_descendant(schema_node_cptr node,
                                  const NodeTest& nt,
                                  t_scmnodes_set* extended_nodes,
                                  t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;
    NodeTestType type = nt.type;

    if (type == node_test_element)
        type = (nt.local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_pi   :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type==pr_ins)
                {
                    if(NULL == nt.local || comp_local_type(*it, NULL, nt.local, pr_ins))
                        res.push_back(*it);
                }
            return res;
    }
    case node_test_comment                  :
    {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type==comment) res.push_back(*it);
            return res;
    }
    case node_test_text                     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node                     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_children_accessor_filter((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_qname                    :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.uri, nt.local, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star            :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == element) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.uri, NULL, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, NULL, nt.local, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_attribute                :
    case node_test_document                 :
        return res;
    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: descendant axis unknown node test");
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Attribute Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_attribute(schema_node_cptr node,
                                 const NodeTest& nt,
                                 t_scmnodes_set* extended_nodes,
                                 t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;
    NodeTestType type = nt.type;

    if (type == node_test_attribute)
        type = (nt.local == NULL ? node_test_wildcard_star : node_test_qname);

    bool extend = extended_nodes &&
                  extender_nodes &&
                  extended_nodes->find(node.ptr()) != extended_nodes->end();

    switch (type)
    {
    case node_test_pi   : return res;
    case node_test_comment                  : return res;
    case node_test_text                     : return res;
    case node_test_element                  : return res;
    case node_test_document                 : return res;
    case node_test_node                     :
        {
            for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
                if (dm_attribute_accessor_filter(ref->object.type)) res.push_back(ref->object.snode);
            if(extend)
            {
                for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                    if (dm_attribute_accessor_filter((*i)->type)) res.push_back(*i);
            }
            return res;
        }
    case node_test_qname                    :
        {
            for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_qname_type(ref->object.snode, nt.uri, nt.local, attribute))
                    res.push_back(ref->object.snode);
            if(extend)
            {
                for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                    if (comp_qname_type(*i, nt.uri, nt.local, attribute))
                        res.push_back(*i);
            }
            return res;
        }
    case node_test_wildcard_star            :
        {
            for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
                if (ref->object.type == attribute) res.push_back(ref->object.snode);
            if(extend)
            {
                for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                    if ((*i)->type == attribute) res.push_back(*i);
            }
            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_uri_type(ref->object.snode, nt.uri, NULL, attribute))
                    res.push_back(ref->object.snode);
            if(extend)
            {
                for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                    if (comp_uri_type(*i, nt.uri, NULL, attribute))
                        res.push_back(*i);
            }
            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            for (sc_ref_item * ref = node->children->first; ref != NULL; ref = ref->next)
                if (comp_local_type(ref->object.snode, NULL, nt.local, attribute))
                    res.push_back(ref->object.snode);
            if(extend)
            {
                for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                   if (comp_local_type(*i, NULL, nt.local, attribute))
                        res.push_back(*i);
            }
            return res;
        }
    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: attribute axis unknown node test");
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Self Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_self(schema_node_cptr node,
                            const NodeTest& nt,
                            t_scmnodes_set* extended_nodes,
                            t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    switch (nt.type)
    {
    case node_test_pi   :
        {
            if (is_pi(node->type))
            {
                if(NULL == nt.local || comp_local_type(node, NULL, nt.local, pr_ins))
                    res.push_back(node.ptr());
            }
            return res;
        }
    case node_test_comment                  :
        {
            if (is_comment(node->type)) res.push_back(node.ptr());
            return res;
        }
    case node_test_text                     :
        {
            if (is_text(node->type)) res.push_back(node.ptr());
            return res;
        }
    case node_test_node                     :
        {
            if (is_node(node->type)) res.push_back(node.ptr());
            return res;
        }
    case node_test_element                  :
    case node_test_qname                    :
    case node_test_wildcard_star            :
        {
            if(node->type == element &&
                 (NULL == nt.local ||
                  comp_qname_type(node, nt.uri, nt.local, element)))
                res.push_back(node.ptr());
            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            if (comp_uri_type(node, nt.uri, NULL, element))
                res.push_back(node.ptr());
            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            if (comp_local_type(node, NULL, nt.local, element))
                res.push_back(node.ptr());
            return res;
        }
    case node_test_attribute                :
        {
            if(node->type == attribute &&
                 (NULL == nt.local ||
                  comp_qname_type(node, nt.uri, nt.local, attribute)))
                res.push_back(node.ptr());
            return res;
        }
    case node_test_document                 :
        {
            if(is_document(node->type))
            {
                if(nt.local != NULL)
                {
                    t_scmnodes_const elements;

                    for (sc_ref_item *ref = node->children->first; ref != NULL; ref = ref->next)
                        if (comp_qname_type(ref->object.snode, nt.uri, nt.local, element))
                            elements.push_back(ref->object.snode);

                    bool extend = extended_nodes &&
                                  extender_nodes &&
                                  extended_nodes->find(node.ptr()) != extended_nodes->end();
                    if(extend)
                    {
                        for (t_scmnodes_set::iterator i = extender_nodes->begin(); i != extender_nodes->end(); i++)
                            if (comp_qname_type(*i, nt.uri, nt.local, element))
                                elements.push_back(*i);
                    }

                    if (elements.size() == 1)
                        res.push_back(node.ptr());
                }
                else
                    res.push_back(node.ptr());
            }
            return res;
        }
    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: self axis unknown node test");
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Descendant-or-Self Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_descendant_or_self(schema_node_cptr node,
                                          const NodeTest& nt,
                                          t_scmnodes_set* extended_nodes,
                                          t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    NodeTestType type = nt.type;

    if (type == node_test_element)
        type = (nt.local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_pi   :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
            {
                if ((*it)->type==pr_ins)
                {
                    if(NULL == nt.local || comp_local_type(*it, NULL, nt.local, pr_ins))
                        res.push_back(*it);
                }
            }
            return res;
        }
    case node_test_comment                  :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type==comment) res.push_back(*it);
            return res;
        }
    case node_test_text                     :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (is_text((*it)->type)) res.push_back(*it);
            return res;
        }
    case node_test_node                     :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                res.push_back(*it);
            return res;
        }
    case node_test_qname                    :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.uri, nt.local, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star            :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == element) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.uri, NULL, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            t_scmnodes_const tmp = descendant_or_self_nodes(node, dm_children_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, NULL, nt.local, element))
                    res.push_back(*it);
            return res;
        }
    case node_test_attribute                :
    case node_test_document                 :
        return execute_node_test_axis_self(node, nt, extended_nodes, extender_nodes);

    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: descendant-or-self axis unknown node test");
    }
}


///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Axis Descendant-Attribute Test
///////////////////////////////////////////////////////////////////////////////


static t_scmnodes_const
execute_node_test_axis_descendant_attr(schema_node_cptr node,
                                       const NodeTest& nt,
                                       t_scmnodes_set* extended_nodes,
                                       t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const res;

    NodeTestType type = nt.type;

    if (type == node_test_attribute)
        type = (nt.local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_pi   : return res;
    case node_test_comment                  : return res;
    case node_test_text                     : return res;
    case node_test_element                  : return res;
    case node_test_document                 : return res;
    case node_test_node                     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (dm_attribute_accessor_filter((*it)->type))
                    res.push_back(*it);
            return res;
        }
    case node_test_qname                    :
        {
            t_scmnodes_const tmp = descendant_nodes(node, is_node, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_qname_type(*it, nt.uri, nt.local, attribute))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star            :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if ((*it)->type == attribute) res.push_back(*it);
            return res;
        }
    case node_test_wildcard_ncname_star     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_uri_type(*it, nt.uri, NULL, attribute))
                    res.push_back(*it);
            return res;
        }
    case node_test_wildcard_star_ncname     :
        {
            t_scmnodes_const tmp = descendant_nodes(node, dm_attribute_accessor_filter, extended_nodes, extender_nodes);
            for (t_scmnodes_const::iterator it = tmp.begin(); it != tmp.end(); it++)
                if (comp_local_type(*it, NULL, nt.local, attribute))
                    res.push_back(*it);
            return res;
        }
    default                                 : throw USER_EXCEPTION2(SE1003, "XPath on Schema: descendant-attribute axis unknown node test");
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Absolute XPath Entry Points
///////////////////////////////////////////////////////////////////////////////


t_scmnodes_const execute_node_test(schema_node_cptr node, const NodeTest& nt, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    switch (nt.axis)
    {
    case axis_child             : return execute_node_test_axis_child(node, nt, extended_nodes, extender_nodes);
    case axis_descendant        : return execute_node_test_axis_descendant(node, nt, extended_nodes, extender_nodes);
    case axis_attribute         : return execute_node_test_axis_attribute(node, nt, extended_nodes, extender_nodes);
    case axis_self              : return execute_node_test_axis_self(node, nt, extended_nodes, extender_nodes);
    case axis_descendant_or_self: return execute_node_test_axis_descendant_or_self(node, nt, extended_nodes, extender_nodes);
    case axis_descendant_attr   : return execute_node_test_axis_descendant_attr(node, nt, extended_nodes, extender_nodes);
    default                     : throw USER_EXCEPTION2(SE1003, "XPath on Schema: unexpected axis");
    }
}

t_scmnodes_const execute_abs_path_expr_rec(const t_scmnodes_const &nodes, const PathExpression &pe, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    t_scmnodes_const n1 = nodes, n2;

    for (size_t p = 0; p < pe.size(); p++)
    {
        if (n1.size() == 0) return n1;
        const NodeTestUnion &nto = pe[p];

        for (size_t i = 0; i != n1.size(); i++)
            for (size_t j = 0; j != nto.size; j++)
            {
                t_scmnodes_const tmp;
                tmp = execute_node_test(n1.at(i), nto.nodes[j], extended_nodes, extender_nodes);
                n2 = vector_concat(n2, tmp);
            }

        n1 = n2;
        n2.clear();
    }

    return n1;
}

t_scmnodes_const execute_abs_path_expr(schema_node_cptr root, const PathExpression *path_expr, t_scmnodes_set* extended_nodes, t_scmnodes_set* extender_nodes)
{
    // Obtain scheme nodes, which satisfy path query
    t_scmnodes_const scmnodes;

    scmnodes.push_back(root.ptr());
    scmnodes = execute_abs_path_expr_rec(scmnodes, *path_expr, extended_nodes, extender_nodes);

    // Eliminate duplicates
    size_t ar_size = scmnodes.size();
    schema_node_xptr * ar_scmnodes = new schema_node_xptr[ar_size];

    size_t i = 0;
    t_scmnodes_const::iterator snit;
    for (snit = scmnodes.begin(), i = 0; snit != scmnodes.end(); snit++, i++)
        ar_scmnodes[i] = *snit;

    qsort(ar_scmnodes, ar_size, sizeof(schema_node_xptr), compare_schema_node);

    scmnodes.clear();

    schema_node_xptr tmp = XNULL;
    for (i = 0; i < ar_size; i++)
    {
        if (tmp == ar_scmnodes[i]) continue;

        scmnodes.push_back(ar_scmnodes[i]);
        tmp = ar_scmnodes[i];
    }

    delete[] ar_scmnodes;
    ar_scmnodes = NULL;

    return scmnodes;
}
