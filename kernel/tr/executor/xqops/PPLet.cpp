/*
 * File:  PPLet.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPLet.h"
#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/xqops/PPSResLStub.h"


PPLet::PPLet(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_) : PPVarIterator(_cxt_),
                                    var_dscs(_var_dscs_),
                                    source_child(_source_child_),
                                    data_child(_data_child_),
                                    source(_source_child_.ts),
                                    need_to_check_type(false)
{
}

PPLet::PPLet(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             const sequence_type& _st_) : PPVarIterator(_cxt_),
                                          var_dscs(_var_dscs_),
                                          source_child(_source_child_),
                                          data_child(_data_child_),
                                          source(_source_child_.ts),
                                          st(_st_),
                                          need_to_check_type(true)
{
}


//PPLet::PPLet(dynamic_context *_cxt_,
//             arr_of_var_dsc _var_dscs_, 
//             PPOpIn _source_child_, 
//             PPOpIn _data_child_,
//             tuple _source_) : PPVarIterator(_cxt_),
//                               var_dscs(_var_dscs_),
//                               source_child(_source_child_),
//                               data_child(_data_child_),
//                               source(_source_),
//                               first_time(false)
//{
//}

PPLet::~PPLet()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPLet::open ()
{
    s = se_new sequence_tmp(source_child.ts);

    source_child.op->open();
    seq_filled = false;
    need_reopen = false;
    first_time = true;

    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->var_cxt.producers[var_dscs[i]];
        p.type = pt_lazy_complex;
        p.op = this;
        p.cvc = se_new complex_var_consumption;
        p.tuple_pos = i;
    }

	data_child.op->open();
}

void PPLet::reopen ()
{
    source_child.op->reopen();
    data_child.op->reopen();

    seq_filled = false;
    first_time = true;
    s->clear();
    need_reopen = false;
    reinit_consumer_table();
}

void PPLet::close ()
{
    source_child.op->close();
    data_child.op->close();

    delete s;
    s = NULL;
}

void PPLet::next(tuple &t)
{
    SET_XQUERY_LINE(__xquery_line);
    
    if (need_reopen)
    {
        if (!seq_filled) source_child.op->reopen();
        seq_filled = false;
        s->clear();
        first_time = true;
        need_reopen = false;
        reinit_consumer_table();
    }

    if(first_time && need_to_check_type)
    {
       if(!type_matches(source_child, s, source, seq_filled, st))
          throw XQUERY_EXCEPTION2(XPTY0004, "Type of a value bound to the variable does not match the declared type according to the rules for SequenceType matching.");
       first_time = false;	
    }

    data_child.op->next(t);

    if (t.is_eos()) need_reopen = true;

    UNDO_XQUERY_LINE;
}

PPIterator* PPLet::copy(dynamic_context *_cxt_)
{
    PPLet *res = need_to_check_type ? se_new PPLet(_cxt_, var_dscs, source_child, data_child, st)
                                    : se_new PPLet(_cxt_, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

var_c_id PPLet::register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(cxt->var_cxt.producers[dsc].cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPLet::next(tuple &t, var_dsc dsc, var_c_id id)
{
    SET_XQUERY_LINE(__xquery_line);

    producer &p = cxt->var_cxt.producers[dsc];
    complex_var_consumption &cvc = *(p.cvc);

    if (cvc[id] < s->size())
    {
        s->get(source, cvc[id]);
        t.copy(source.cells[p.tuple_pos]);
        cvc[id]++;
    }
    else
    {
        if (seq_filled)
        {
            t.set_eos();
            cvc[id] = 0;
        }
        else
        {
            source_child.op->next(source);
            if (source.is_eos())
            {
                seq_filled = true;
                t.set_eos();
                cvc[id] = 0;
            }
            else
            {
                s->add(source);
                t.copy(source.cells[p.tuple_pos]);
                cvc[id]++;
            }
        }
    }

    UNDO_XQUERY_LINE;
}

void PPLet::reopen(var_dsc dsc, var_c_id id)
{
    cxt->var_cxt.producers[dsc].cvc->at(id) = 0;
}

void PPLet::close(var_dsc dsc, var_c_id id)
{
}

inline void PPLet::reinit_consumer_table()
{
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->var_cxt.producers[var_dscs[i]];
        for (int j = 0; j < p.cvc->size(); j++) p.cvc->at(j) = 0;
    }
}

bool PPLet::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    PPOpIn data_child, source_child;
    ((PPLet*)cur)->children(source_child, data_child);

    void *source_r;
    bool source_s = (source_child.op->res_fun())(source_child.op, cxt, source_r);

    if (!source_s) // if source is not strict
    { // create PPLet and transmit state
        source_child.op = (PPIterator*)source_r;
        data_child.op = data_child.op->copy(cxt);
        PPLet *res_op = se_new PPLet(cxt, ((PPLet*)cur)->var_dscs, source_child, data_child);

        r = res_op;
        return false;
    }

    sequence *source_seq = (sequence*)source_r;
    arr_of_var_dsc &var_dscs = ((PPLet*)cur)->var_dscs;

    // prepare context
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_tuple;
        p.tuple_pos = i;
        p.t = se_new tuple(1);
    }

    sequence *res_seq = se_new sequence(1);
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
        { // create PPLet and transmit state
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
            PPLet *ret_op = se_new PPLet(cxt, ((PPLet*)cur)->var_dscs, source_child, data_child, source_t);

            // create stub for PPLet
            PPSResLStub *upper_stub = se_new PPSResLStub(cxt, ret_op, res_seq);

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
*/
    return true;
}
