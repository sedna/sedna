/*
 * File:  PPConGen.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPConGen.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPConGen1
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPConGen1::PPConGen1(variable_context *_cxt_,
                     var_dsc _dsc_,
                     PPOpIn _child_) : PPIterator(_cxt_),
                                       dsc(_dsc_),
                                       child(_child_),
                                       data(child.ts)
{
}

PPConGen1::~PPConGen1()
{
    delete child.op;
    child.op = NULL;
}

void PPConGen1::open  ()
{
    child.op->open();

    producer &p = cxt->producers[dsc];
    if (p.c1u == NULL)
    {
        p.type = pt_congen1;
        p.c1u = new congen1_usage;
    }
    p.c1u->total++;
}

void PPConGen1::reopen()
{
    child.op->reopen();
    cxt->producers[dsc].c1u->reopen();
}

void PPConGen1::close ()
{
    child.op->close();
}

void PPConGen1::next  (tuple &t)
{
    child.op->next(data);

    congen1_usage &p = *(cxt->producers[dsc].c1u);

    if (data.is_eos())
    {
        t.set_eos();
        p.over++;
        if (p.over == p.total) p.reopen();
    }
    else
    {
        // put data and congen1_var into t
        t.copy(data, tuple_cell::atomic((__int64)p.counter));
        p.counter++;
    }
}

PPIterator* PPConGen1::copy(variable_context *_cxt_)
{
    PPConGen1 *res = new PPConGen1(_cxt_, dsc, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPConGen1::result(PPIterator* cur, variable_context *cxt, void*& r)
{
/*
    PPOpIn child;
    ((PPConGen1*)cur)->children(child);

    void *nk_r;
    bool nk_s = (child.op->res_fun())(child.op, cxt, nk_r);

    if (!nk_s) // if expression is not strict
    { // create PPConGen1 and transmit state
        child.op = (PPIterator*)nk_r;
        r = new PPConGen1(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nk_r;
    if (d_seq->size() != 1) throw QEPTypeError("#????: Argument of node-kind is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw QEPTypeError("#????: Argument of node-kind is not a node");

    dm_node_kind_type res = dm_node_kind(tc.get_node());

    switch (res)
    {
        case nk_document				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "document"));
             return true;
        case nk_element					: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "element"));
             return true;
        case nk_attribute				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "attribute"));
             return true;
        case nk_text					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "text"));
             return true;
        case nk_namespace				:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "namespace"));
             return true;
        case nk_processing_instruction	:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             return true;
        case nk_comment					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "comment"));
             return true;
        default							: 
             throw QEPSystemError("#????: Impossible value of dm_node_kind_type");
    }
*/
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPConGen2
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPConGen2::PPConGen2(variable_context *_cxt_,
                     PPOpIn _child_) : PPIterator(_cxt_),
                                       child(_child_),
                                       data(child.ts)
{
}

PPConGen2::~PPConGen2()
{
    delete child.op;
    child.op = NULL;
}

void PPConGen2::open  ()
{
    child.op->open();
    s = new sequence(child.ts);
    counter = 0;
}

void PPConGen2::reopen()
{
    child.op->reopen();
    s->clear();
    counter = 0;
}

void PPConGen2::close ()
{
    child.op->close();
    delete s;
}

void PPConGen2::next  (tuple &t)
{
    if (counter == 0)
    {
        while (true)
        {
            child.op->next(data);
            if (data.is_eos()) break;
            else s->add(data);
        }

        counter = 1;
    }

    if (counter - 1 == s->size())
    {
        t.set_eos();
        s->clear();
        counter = 0;
    }
    else
    {
        s->get(data, counter - 1);
        t.copy(data, tuple_cell::atomic((__int64)counter), tuple_cell::atomic((__int64)(s->size())));
        counter++;
    }
}

PPIterator* PPConGen2::copy(variable_context *_cxt_)
{
    PPConGen2 *res = new PPConGen2(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPConGen2::result(PPIterator* cur, variable_context *cxt, void*& r)
{
/*
    PPOpIn child;
    ((PPConGen2*)cur)->children(child);

    void *nk_r;
    bool nk_s = (child.op->res_fun())(child.op, cxt, nk_r);

    if (!nk_s) // if expression is not strict
    { // create PPConGen2 and transmit state
        child.op = (PPIterator*)nk_r;
        r = new PPConGen2(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nk_r;
    if (d_seq->size() != 1) throw QEPTypeError("#????: Argument of node-kind is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw QEPTypeError("#????: Argument of node-kind is not a node");

    dm_node_kind_type res = dm_node_kind(tc.get_node());

    switch (res)
    {
        case nk_document				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "document"));
             return true;
        case nk_element					: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "element"));
             return true;
        case nk_attribute				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "attribute"));
             return true;
        case nk_text					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "text"));
             return true;
        case nk_namespace				:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "namespace"));
             return true;
        case nk_processing_instruction	:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             return true;
        case nk_comment					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "comment"));
             return true;
        default							: 
             throw QEPSystemError("#????: Impossible value of dm_node_kind_type");
    }
*/
    return true;
}

