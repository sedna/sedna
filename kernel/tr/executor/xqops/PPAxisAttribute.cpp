/*
* File:  PPAxisAttribute.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisAttribute.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"


PPAxisAttribute::PPAxisAttribute(dynamic_context *_cxt_,
                                 operation_info _info_, 
                                 PPOpIn _child_,
                                 NodeTestType _nt_type_,
                                 NodeTestData _nt_data_) : PPIterator(_cxt_, _info_),
                                 child(_child_),
                                 nt_type(_nt_type_),
                                 nt_data(_nt_data_)
{
    NodeTestType type = nt_type;

    if (type == node_test_attribute) 
        type = (nt_data.ncname_local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_processing_instruction   : next_fun = &PPAxisAttribute::next_processing_instruction; break;
    case node_test_comment                  : next_fun = &PPAxisAttribute::next_comment; break;
    case node_test_text                     : next_fun = &PPAxisAttribute::next_text; break;
    case node_test_node                     : next_fun = &PPAxisAttribute::next_node; break;
    case node_test_qname                    : next_fun = &PPAxisAttribute::next_qname; break;
    case node_test_element                  : next_fun = &PPAxisAttribute::next_element; break;
    case node_test_document                 : next_fun = &PPAxisAttribute::next_document; break;
    case node_test_wildcard_star            : next_fun = &PPAxisAttribute::next_wildcard_star; break;
    case node_test_wildcard_ncname_star	    : next_fun = &PPAxisAttribute::next_wildcard_ncname_star; break;
    case node_test_wildcard_star_ncname	    : next_fun = &PPAxisAttribute::next_wildcard_star_ncname; break;

    default									: throw USER_EXCEPTION2(SE1003, "PPAxisAttribute: unexpected node test");
    }
}

PPAxisAttribute::~PPAxisAttribute()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisAttribute::do_open ()
{
    child.op->open();
    cur = XNULL;
}

void PPAxisAttribute::do_reopen()
{
    child.op->reopen();
    cur = XNULL;
}

void PPAxisAttribute::do_close()
{
    child.op->close();
}


void PPAxisAttribute::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisAttribute::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisAttribute::next_element(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisAttribute::next_document(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisAttribute::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAttribute::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstByOrderAttributeChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderAttribute(cur);
}

void PPAxisAttribute::next_qname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
            nt_data.uri,
            nt_data.ncname_local,
            attribute,
            comp_qname_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

void PPAxisAttribute::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstByOrderAttributeChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderAttribute(cur);
}

void PPAxisAttribute::next_wildcard_ncname_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         nt_data.uri,
                         NULL,
                         attribute,
                         comp_uri_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

void PPAxisAttribute::next_wildcard_star_ncname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         NULL,
                         nt_data.ncname_local,
                         attribute,
                         comp_local_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

PPIterator* PPAxisAttribute::do_copy(dynamic_context *_cxt_)
{
    PPAxisAttribute *res = se_new PPAxisAttribute(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
