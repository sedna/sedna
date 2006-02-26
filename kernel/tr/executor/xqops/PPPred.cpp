/*
 * File:  PPPred.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPPred.h"
#include "PPUtils.h"
#include "PPSLStub.h"
#include "PPSResLStub.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred1
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPPred1::PPPred1(variable_context *_cxt_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_, 
                 PPOpIn _data_child_,
                 var_dsc _pos_dsc_) : PPVarIterator(_cxt_),
                                      var_dscs(_var_dscs_),
                                      source_child(_source_child_),
                                      data_child(_data_child_),
                                      data(_data_child_.ts),
                                      pos_dsc(_pos_dsc_)
{
}
/*
PPPred1::PPPred1(variable_context *_cxt_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_,
                   tuple _source_) : PPVarIterator(_cxt_),
                                     var_dscs(_var_dscs_),
                                     source_child(_source_child_),
                                     data_child(_data_child_),
                                     data(_data_child_.ts),
                                     source(_source_)
{
    first_time = false;
    eos_reached = true;
    standard = false;
}
*/
PPPred1::~PPPred1()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPPred1::open ()
{
    source_child.op->open();

    pos = 0;
    cur_tuple = NULL;
    first_time = true;
    eos_reached = true;

    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = i;
    }

    if (pos_dsc >= 0)
    {
        producer &p = cxt->producers[pos_dsc];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = 0;
    }

    data_child.op->open();
}

void PPPred1::reopen ()
{
    pos = 0;
    source_child.op->reopen();
}

void PPPred1::close ()
{
    source_child.op->close();
    data_child.op->close();
}

void PPPred1::next(tuple &t)
{
    while (true)
    {
        source_child.op->next(t);
        ++pos;
        cur_tuple = &t;

        if (t.is_eos()) { pos = 0; return; }

        if (first_time) first_time = false;
        else
        {
            reinit_consumer_table();
            if (!eos_reached) data_child.op->reopen();
        }

        tuple_cell tc = predicate_boolean_value(data_child, data, eos_reached, pos);

        if (tc.get_xs_boolean()) return;
    }
}

PPIterator* PPPred1::copy(variable_context *_cxt_)
{
    PPPred1 *res = new PPPred1(_cxt_, var_dscs, source_child, data_child, pos_dsc);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

var_c_id PPPred1::register_consumer(var_dsc dsc)
{
    cxt->producers[dsc].svc->push_back(true);
    return cxt->producers[dsc].svc->size() - 1;
}

void PPPred1::next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->producers[dsc];

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        t.copy(dsc == pos_dsc ? tuple_cell::atomic(pos)
                              : cur_tuple->cells[p.tuple_pos]);
    }
    else 
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPPred1::reopen(var_dsc dsc, var_c_id id)
{
    cxt->producers[dsc].svc->at(id) = true;
}

inline void PPPred1::reinit_consumer_table()
{
    int j;
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }

    if (pos_dsc >= 0)
    {
        producer &p = cxt->producers[pos_dsc];
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}

bool PPPred1::result(PPIterator* cur, variable_context *cxt, void*& r)
{
/*
    PPOpIn data_child, source_child;
    ((PPPred1*)cur)->children(source_child, data_child);

    void *source_r;
    bool source_s = (source_child.op->res_fun())(source_child.op, cxt, source_r);

    if (!source_s) // if source is not strict
    { // create PPPred1 and transmit state
        source_child.op = (PPIterator*)source_r;
        data_child.op = data_child.op->copy(cxt);
        PPPred1 *res_op = new PPPred1(cxt, ((PPPred1*)cur)->var_dscs, source_child, data_child);

        r = res_op;
        return false;
    }

    sequence *source_seq = (sequence*)source_r;
    arr_of_var_dsc &var_dscs = ((PPPred1*)cur)->var_dscs;

    // prepare context
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_tuple;
        p.tuple_pos = i;
        p.t = new tuple(1);
    }

    sequence *res_seq = new sequence(source_child.ts);
    tuple source_t(var_dscs.size());
    tuple data_t(1);
    sequence::iterator source_it; 
    for (source_it = source_seq->begin(); source_it != source_seq->end(); ++source_it)
    {
        source_seq->get(source_t, source_it);
        // fill context
        for (int i = 0; i < var_dscs.size(); i++)
        {
            producer &p = cxt->producers[var_dscs[i]];
            p.t->copy(source_t.cells[p.tuple_pos]);
        }

        void *data_r;
        bool data_s = (data_child.op->res_fun())(data_child.op, cxt, data_r);

        if (!data_s) // if data is not strict
        { // create PPPred1 and transmit state
            // create new lazy source child
            PPIterator *new_source_child = source_child.op->copy(cxt);

            // create new source sequence - the rest of the source sequence
            sequence::iterator ssit = source_it;
            sequence *new_source_seq = new sequence(var_dscs.size());

            for (++ssit; ssit != source_seq->end(); ++ssit)
            {
                source_seq->get(source_t, ssit);
                new_source_seq->add(source_t);
            }
            delete source_seq;

            // create stub for source
            PPSLStub *lower_stub = new PPSLStub(cxt, new_source_child, new_source_seq);


            source_child.op = lower_stub;
            data_child.op = (PPIterator*)data_r;
            PPPred1 *ret_op = new PPPred1(cxt, ((PPPred1*)cur)->var_dscs, source_child, data_child, source_t);

            // create stub for PPPred1
            PPSResLStub *upper_stub = new PPSResLStub(cxt, ret_op, res_seq);

            r = upper_stub;
            return false;
        }

        if (effective_boolean_value((sequence*)data_r).get_xs_boolean())
            res_seq->add(source_t);
        delete (sequence*)data_r;
    }

    r = res_seq;
*/
    return true;
}











///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred2
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPPred2::PPPred2(variable_context *_cxt_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_, 
                 PPOpIn _data_child_,
                 var_dsc _pos_dsc_,
                 var_dsc _lst_dsc_) : PPVarIterator(_cxt_),
                                      var_dscs(_var_dscs_),
                                      source_child(_source_child_),
                                      data_child(_data_child_),
                                      data(_data_child_.ts),
                                      pos_dsc(_pos_dsc_),
                                      lst_dsc(_lst_dsc_)
{
}
/*
PPPred2::PPPred2(variable_context *_cxt_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_,
                   tuple _source_) : PPVarIterator(_cxt_),
                                     var_dscs(_var_dscs_),
                                     source_child(_source_child_),
                                     data_child(_data_child_),
                                     data(_data_child_.ts),
                                     source(_source_)
{
    first_time = false;
    eos_reached = true;
    standard = false;
}
*/
PPPred2::~PPPred2()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPPred2::open ()
{
    source_child.op->open();

    s = new sequence(source_child.ts);

    pos = 0;
    cur_tuple = NULL;
    first_time = true;
    eos_reached = true;

    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = i;
    }

    {
        producer &p = cxt->producers[pos_dsc];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = 0;
    }
    {
        producer &p = cxt->producers[lst_dsc];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = 0;
    }

    data_child.op->open();
}

void PPPred2::reopen ()
{
    source_child.op->reopen();
    s->clear();
    pos = 0;
}

void PPPred2::close ()
{
    source_child.op->close();
    data_child.op->close();
    delete s;
}

void PPPred2::next(tuple &t)
{
    if (pos == 0)
    {
        while (true)
        {
            source_child.op->next(t);
            if (t.is_eos()) break;
            else s->add(t);
        }
    }

    while (pos < s->size())
    {
        s->get(t, pos++);
        cur_tuple = &t;

        if (first_time) first_time = false;
        else
        {
            reinit_consumer_table();
            if (!eos_reached) data_child.op->reopen();
        }

        tuple_cell tc = predicate_boolean_value(data_child, data, eos_reached, pos);

        if (tc.get_xs_boolean()) return;
    }

    t.set_eos();
    s->clear();
    pos = 0;
}

PPIterator* PPPred2::copy(variable_context *_cxt_)
{
    PPPred2 *res = new PPPred2(_cxt_, var_dscs, source_child, data_child, pos_dsc, lst_dsc);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

var_c_id PPPred2::register_consumer(var_dsc dsc)
{
    cxt->producers[dsc].svc->push_back(true);
    return cxt->producers[dsc].svc->size() - 1;
}

void PPPred2::next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->producers[dsc];

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        if (dsc == pos_dsc)			t.copy(tuple_cell::atomic(pos));
        else if (dsc == lst_dsc)	t.copy(tuple_cell::atomic(s->size()));
        else						t.copy(cur_tuple->cells[p.tuple_pos]);
    }
    else 
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPPred2::reopen(var_dsc dsc, var_c_id id)
{
    cxt->producers[dsc].svc->at(id) = true;
}

inline void PPPred2::reinit_consumer_table()
{
    int j;
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }

    {
        producer &p = cxt->producers[pos_dsc];
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
    {
        producer &p = cxt->producers[lst_dsc];
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}

bool PPPred2::result(PPIterator* cur, variable_context *cxt, void*& r)
{
/*
    PPOpIn data_child, source_child;
    ((PPPred2*)cur)->children(source_child, data_child);

    void *source_r;
    bool source_s = (source_child.op->res_fun())(source_child.op, cxt, source_r);

    if (!source_s) // if source is not strict
    { // create PPPred2 and transmit state
        source_child.op = (PPIterator*)source_r;
        data_child.op = data_child.op->copy(cxt);
        PPPred2 *res_op = new PPPred2(cxt, ((PPPred2*)cur)->var_dscs, source_child, data_child);

        r = res_op;
        return false;
    }

    sequence *source_seq = (sequence*)source_r;
    arr_of_var_dsc &var_dscs = ((PPPred2*)cur)->var_dscs;

    // prepare context
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_tuple;
        p.tuple_pos = i;
        p.t = new tuple(1);
    }

    sequence *res_seq = new sequence(source_child.ts);
    tuple source_t(var_dscs.size());
    tuple data_t(1);
    sequence::iterator source_it; 
    for (source_it = source_seq->begin(); source_it != source_seq->end(); ++source_it)
    {
        source_seq->get(source_t, source_it);
        // fill context
        for (int i = 0; i < var_dscs.size(); i++)
        {
            producer &p = cxt->producers[var_dscs[i]];
            p.t->copy(source_t.cells[p.tuple_pos]);
        }

        void *data_r;
        bool data_s = (data_child.op->res_fun())(data_child.op, cxt, data_r);

        if (!data_s) // if data is not strict
        { // create PPPred2 and transmit state
            // create new lazy source child
            PPIterator *new_source_child = source_child.op->copy(cxt);

            // create new source sequence - the rest of the source sequence
            sequence::iterator ssit = source_it;
            sequence *new_source_seq = new sequence(var_dscs.size());

            for (++ssit; ssit != source_seq->end(); ++ssit)
            {
                source_seq->get(source_t, ssit);
                new_source_seq->add(source_t);
            }
            delete source_seq;

            // create stub for source
            PPSLStub *lower_stub = new PPSLStub(cxt, new_source_child, new_source_seq);


            source_child.op = lower_stub;
            data_child.op = (PPIterator*)data_r;
            PPPred2 *ret_op = new PPPred2(cxt, ((PPPred2*)cur)->var_dscs, source_child, data_child, source_t);

            // create stub for PPPred2
            PPSResLStub *upper_stub = new PPSResLStub(cxt, ret_op, res_seq);

            r = upper_stub;
            return false;
        }

        if (effective_boolean_value((sequence*)data_r).get_xs_boolean())
            res_seq->add(source_t);
        delete (sequence*)data_r;
    }

    r = res_seq;
*/
    return true;
}

