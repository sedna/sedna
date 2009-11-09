/*
* File:  PPAxisParent.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisParent.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/merge.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisParent
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPAxisParent::PPAxisParent(dynamic_context *_cxt_,
                           operation_info _info_, 
                           PPOpIn _child_,
                           NodeTestType _nt_type_,
                           NodeTestData _nt_data_) : PPIterator(_cxt_, _info_),
                           child(_child_),
                           nt_type(_nt_type_),
                           nt_data(_nt_data_)
{
    NodeTestType type = nt_type;
    
    if (type == node_test_element) 
        type = (nt_data.ncname_local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_processing_instruction   : next_fun = &PPAxisParent::next_processing_instruction; break;
    case node_test_comment                  : next_fun = &PPAxisParent::next_comment; break;
    case node_test_text                     : next_fun = &PPAxisParent::next_text; break;
    case node_test_node                     : next_fun = &PPAxisParent::next_node; break;
    case node_test_qname                    : next_fun = &PPAxisParent::next_qname; break;
    case node_test_attribute                : next_fun = &PPAxisParent::next_attribute; break;
    case node_test_document                 : next_fun = &PPAxisParent::next_document; break;
    case node_test_wildcard_star            : next_fun = &PPAxisParent::next_wildcard_star; break;
    case node_test_wildcard_ncname_star     : next_fun = &PPAxisParent::next_wildcard_ncname_star; break;
    case node_test_wildcard_star_ncname     : next_fun = &PPAxisParent::next_wildcard_star_ncname; break;
    default                                 : throw USER_EXCEPTION2(SE1003, "PPParent: unexpected node test");
    }
}

PPAxisParent::~PPAxisParent()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisParent::do_open ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisParent::do_reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisParent::do_close()
{
    child.op->close();
}


void PPAxisParent::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisParent::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisParent::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisParent::next_attribute(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}



void PPAxisParent::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        if (cur!=XNULL)
        {
            CHECKP(cur);
            if (GETSCHEMENODEX(cur)->type==virtual_root)
                cur=XNULL;
        }
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_qname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        if (cur==XNULL) continue;
        CHECKP(cur);
        if (!comp_qname_type(GETSCHEMENODEX(cur),
            nt_data.uri,
            nt_data.ncname_local, 
            element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        if (cur==XNULL) continue;
        CHECKP(cur);
        if (!comp_type(GETSCHEMENODEX(cur), 
            NULL,
            NULL, 
            element)) cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_ncname_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);

        if (cur==XNULL) continue;
        CHECKP(cur);
        if (!
            comp_uri_type(GETSCHEMENODEX(cur),
            nt_data.uri,
            NULL, 
            element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_star_ncname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);

        if (cur==XNULL) continue;
        CHECKP(cur);
        if (!
            comp_local_type(GETSCHEMENODEX(cur),
            NULL,
            nt_data.ncname_local,
            element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_document(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);

        if (cur == XNULL) continue;
        CHECKP(cur);
        
        if(GETSCHEMENODEX(cur)->type != document) 
        {
            cur = XNULL;
        }
        else if(nt_data.ncname_local != NULL) 
        {
            RelChildAxisMerge merge;
            xptr desc = merge.init(cur,
                                   nt_data.uri,
                                   nt_data.ncname_local,
                                   element,
                                   comp_qname_type);

            if(desc == XNULL || merge.next(desc) != XNULL) cur = XNULL;
        }
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}


PPIterator* PPAxisParent::do_copy(dynamic_context *_cxt_)
{
    PPAxisParent *res = se_new PPAxisParent(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

