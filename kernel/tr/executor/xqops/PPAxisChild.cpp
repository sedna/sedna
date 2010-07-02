/*
 * File:  PPAxisChild.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisChild.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/visitor/PPVisitor.h"

#include "tr/structures/nodeutils.h"

PPAxisChild::PPAxisChild(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_) : PPIterator(_cxt_, _info_, "PPAxisChild"),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_),
                                                   cur(XNULL)
{
    NodeTestType type = nt_type;

    if (type == node_test_element)
        type = (nt_data.ncname_local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
        case node_test_processing_instruction   : next_fun = &PPAxisChild::next_processing_instruction; break;
        case node_test_comment                  : next_fun = &PPAxisChild::next_comment; break;
        case node_test_text                     : next_fun = &PPAxisChild::next_text; break;
        case node_test_node                     : next_fun = &PPAxisChild::next_node; break;
        case node_test_qname                    : next_fun = &PPAxisChild::next_qname; break;
        case node_test_wildcard_star            : next_fun = &PPAxisChild::next_wildcard_star; break;
        case node_test_wildcard_ncname_star     : next_fun = &PPAxisChild::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname     : next_fun = &PPAxisChild::next_wildcard_star_ncname; break;
        case node_test_attribute                : next_fun = &PPAxisChild::next_eos; break;
        case node_test_document                 : next_fun = &PPAxisChild::next_eos; break;
        default									: throw USER_EXCEPTION2(SE1003, "PPAxisChild: unexpected node test");
    }
}

PPAxisChild::~PPAxisChild()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisChild::do_open ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisChild::do_reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisChild::do_close()
{
    child.op->close();
}

void PPAxisChild::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

static inline bool
pi_node_name_equals(const xptr& node, const char* local)
{
    CHECKP(node);
    return PINode(node).compareTarget(local) == 0;
}

void PPAxisChild::next_processing_instruction(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstChildByType(child.get(t).get_node(), pr_ins);

        while (cur!=XNULL && nt_data.ncname_local)
        {
            if (pi_node_name_equals(cur, nt_data.ncname_local))
                break;
            else
                cur=getNextSiblingOfSameSort(cur);
        }
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextSiblingOfSameSort(cur);

    while (cur!=XNULL)
    {
        if (nt_data.ncname_local)
		{
            if (pi_node_name_equals(cur, nt_data.ncname_local)) return;
		}
        else return;
         cur = getNextSiblingOfSameSort(cur);
    }
}

void PPAxisChild::next_comment(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstChildByType(child.get(t).get_node(), comment);
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextSiblingOfSameSort(cur);
}

void PPAxisChild::next_text(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstChildByType(child.get(t).get_node(), text);
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextSiblingOfSameSort(cur);
}

void PPAxisChild::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getIndirectionSafeCP(getFirstNonAttributeChild(child.get(t).get_node()));
    }

    t.copy(tuple_cell::node_indir(cur));
    cur = getIndirectionSafeCP(getNextNonAttribute(indirectionDereferenceCP(cur)));
}


void PPAxisChild::next_qname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         nt_data.uri,
                         nt_data.ncname_local,
                         element,
                         comp_qname_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstElementChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextElement(cur);
}

void PPAxisChild::next_wildcard_ncname_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         nt_data.uri,
                         NULL,
                         element,
                         comp_uri_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_wildcard_star_ncname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         NULL,
                         nt_data.ncname_local,
                         element,
                         comp_local_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_eos(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

PPIterator* PPAxisChild::do_copy(dynamic_context *_cxt_)
{
    PPAxisChild *res = se_new PPAxisChild(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
