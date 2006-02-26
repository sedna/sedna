/*
 * File:  PPReturn.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPReturn.h"
#include "PPSLStub.h"
#include "PPSResLStub.h"


PPReturn::PPReturn(variable_context *_cxt_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_) : PPVarIterator(_cxt_),
                                          var_dscs(_var_dscs_),
                                          source_child(_source_child_),
                                          data_child(_data_child_),
                                          source(_source_child_.ts)
{
}

PPReturn::PPReturn(variable_context *_cxt_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_,
                   tuple _source_) : PPVarIterator(_cxt_),
                                     var_dscs(_var_dscs_),
                                     source_child(_source_child_),
                                     data_child(_data_child_),
                                     source(_source_),
                                     first_time(false)
{
}

PPReturn::~PPReturn()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPReturn::open ()
{
    source_child.op->open();
    first_time = true;

    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = new simple_var_consumption;
        p.tuple_pos = i;
    }

	data_child.op->open();
}

void PPReturn::reopen ()
{
    source_child.op->reopen();
    data_child.op->reopen();

    first_time = true;
    reinit_consumer_table();
}

void PPReturn::close ()
{
    source_child.op->close();
    data_child.op->close();
}

void PPReturn::next(tuple &t)
{
    if (first_time)
    {
        t.set_eos();
        first_time = false;
    }
    else data_child.op->next(t);

    while (t.is_eos())
    {
        source_child.op->next(source);

        if (source.is_eos())
        {
            t.set_eos();
            first_time = true;			// reopens automatically
            reinit_consumer_table();	// reopens automatically
            return;
        }

        reinit_consumer_table();

        // there should be 'data_child.op->reopen()' call but data child reopens automatically
        data_child.op->next(t);
    }
}

PPIterator* PPReturn::copy(variable_context *_cxt_)
{
    PPReturn *res = new PPReturn(_cxt_, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

var_c_id PPReturn::register_consumer(var_dsc dsc)
{
    simple_var_consumption &svc = *(cxt->producers[dsc].svc);
    svc.push_back(true);
    return svc.size() - 1;
}

void PPReturn::next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->producers[dsc];

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        t.copy(source.cells[p.tuple_pos]);
    }
	else
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPReturn::reopen(var_dsc dsc, var_c_id id)
{
    cxt->producers[dsc].svc->at(id) = true;
}

inline void PPReturn::reinit_consumer_table()
{
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        for (int j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}

bool PPReturn::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn data_child, source_child;
    ((PPReturn*)cur)->children(source_child, data_child);

    void *source_r;
    bool source_s = (source_child.op->res_fun())(source_child.op, cxt, source_r);

    if (!source_s) // if source is not strict
    { // create PPReturn and transmit state
        source_child.op = (PPIterator*)source_r;
        data_child.op = data_child.op->copy(cxt);
        PPReturn *res_op = new PPReturn(cxt, ((PPReturn*)cur)->var_dscs, source_child, data_child);

        r = res_op;
        return false;
    }

    sequence *source_seq = (sequence*)source_r;
    arr_of_var_dsc &var_dscs = ((PPReturn*)cur)->var_dscs;

    // prepare context
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_tuple;
        p.tuple_pos = i;
        p.t = new tuple(1);
    }

    sequence *res_seq = new sequence(1);
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
        { // create PPReturn and transmit state
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
            PPReturn *ret_op = new PPReturn(cxt, ((PPReturn*)cur)->var_dscs, source_child, data_child, source_t);

            // create stub for PPReturn
            PPSResLStub *upper_stub = new PPSResLStub(cxt, ret_op, res_seq);

            r = upper_stub;
            return false;
        }

       sequence *data_seq = (sequence*)data_r;
       sequence::iterator data_it; 
       for (data_it = data_seq->begin(); data_it != data_seq->end(); data_it++)
       {
           data_seq->get(data_t, data_it);
           res_seq->add(data_t);
       }
       delete data_seq;
    }

    return strict_op_result(cur, res_seq, cxt, r);
}
