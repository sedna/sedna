/*
 * File:  PPSelect.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSelect.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/xqops/PPSResLStub.h"


PPSelect::PPSelect(dynamic_context *_cxt_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_) : PPVarIterator(_cxt_),
                                          var_dscs(_var_dscs_),
                                          source_child(_source_child_),
                                          data_child(_data_child_),
                                          data(_data_child_.ts),
                                          source(_source_child_.ts)
{
}

PPSelect::PPSelect(dynamic_context *_cxt_,
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

PPSelect::~PPSelect()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPSelect::open ()
{
    source_child.op->open();

    cur_tuple = NULL;
    first_time = true;
    eos_reached = true;
    standard = true;

    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->var_cxt.producers[var_dscs[i]];
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = i;
    }

    data_child.op->open();
}

void PPSelect::reopen ()
{
    source_child.op->reopen();
}

void PPSelect::close ()
{
    source_child.op->close();
    data_child.op->close();
}

void PPSelect::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    while (true)
    {
        if (standard) source_child.op->next(t);
        else { t = source; standard = true; }
        cur_tuple = &t;

        if (t.is_eos()) {RESTORE_CURRENT_PP; return;}

        if (first_time) first_time = false;
        else
        {
            reinit_consumer_table();
            if (!eos_reached) data_child.op->reopen();
        }

        tuple_cell tc = effective_boolean_value(data_child, data, eos_reached);

        if (tc.get_xs_boolean()) {RESTORE_CURRENT_PP; return;}
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPSelect::copy(dynamic_context *_cxt_)
{
    PPSelect *res = se_new PPSelect(_cxt_, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

var_c_id PPSelect::register_consumer(var_dsc dsc)
{
    cxt->var_cxt.producers[dsc].svc->push_back(true);
    return cxt->var_cxt.producers[dsc].svc->size() - 1;
}

void PPSelect::next(tuple &t, var_dsc dsc, var_c_id id)
{
    SET_CURRENT_PP_VAR(this);
    
    producer &p = cxt->var_cxt.producers[dsc];

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        t.copy(cur_tuple->cells[p.tuple_pos]);
    }
    else 
    {
        p.svc->at(id) = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP_VAR;
}

void PPSelect::reopen(var_dsc dsc, var_c_id id)
{
    cxt->var_cxt.producers[dsc].svc->at(id) = true;
}

void PPSelect::close(var_dsc dsc, var_c_id id)
{
}

inline void PPSelect::reinit_consumer_table()
{
    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->var_cxt.producers[var_dscs[i]];
        for (unsigned int j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}

bool PPSelect::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    PPOpIn data_child, source_child;
    ((PPSelect*)cur)->children(source_child, data_child);

    void *source_r;
    bool source_s = (source_child.op->res_fun())(source_child.op, cxt, source_r);

    if (!source_s) // if source is not strict
    { // create PPSelect and transmit state
        source_child.op = (PPIterator*)source_r;
        data_child.op = data_child.op->copy(cxt);
        PPSelect *res_op = se_new PPSelect(cxt, ((PPSelect*)cur)->var_dscs, source_child, data_child);

        r = res_op;
        return false;
    }

    sequence *source_seq = (sequence*)source_r;
    arr_of_var_dsc &var_dscs = ((PPSelect*)cur)->var_dscs;

    // prepare context
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_tuple;
        p.tuple_pos = i;
        p.t = se_new tuple(1);
    }

    sequence *res_seq = se_new sequence(source_child.ts);
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
        { // create PPSelect and transmit state
            // create se_new lazy source child
            PPIterator *new_source_child = source_child.op->copy(cxt);

            // create se_new source sequence - the rest of the source sequence
            sequence::iterator ssit = source_it;
            sequence *new_source_seq = se_new sequence(var_dscs.size());

            for (++ssit; ssit != source_seq->end(); ++ssit)
            {
                source_seq->get(source_t, ssit);
                new_source_seq->add(source_t);
            }
            delete source_seq;

            // create stub for source
            PPSLStub *lower_stub = se_new PPSLStub(cxt, new_source_child, new_source_seq);


            source_child.op = lower_stub;
            data_child.op = (PPIterator*)data_r;
            PPSelect *ret_op = se_new PPSelect(cxt, ((PPSelect*)cur)->var_dscs, source_child, data_child, source_t);

            // create stub for PPSelect
            PPSResLStub *upper_stub = se_new PPSResLStub(cxt, ret_op, res_seq);

            r = upper_stub;
            return false;
        }

        if (effective_boolean_value((sequence*)data_r).get_xs_boolean())
            res_seq->add(source_t);
        delete (sequence*)data_r;
    }

    r = res_seq;
    return true;
*/
    return true;
}
