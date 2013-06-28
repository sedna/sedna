/*
 * File:  PPLet.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPLet.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPLet::PPLet(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_) : PPVarIterator(_cxt_, _info_, "PPLet"),
                                    var_dscs(_var_dscs_),
                                    source_child(_source_child_),
                                    source(_source_child_.ts),
                                    data_child(_data_child_),
                                    need_to_check_type(false)
{
}

PPLet::PPLet(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             const sequence_type& _st_) : PPVarIterator(_cxt_, _info_, "PPLet"),
                                          var_dscs(_var_dscs_),
                                          source_child(_source_child_),
                                          source(_source_child_.ts),
                                          data_child(_data_child_),
                                          need_to_check_type(true),
                                          st(_st_)
{
}

PPLet::~PPLet()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPLet::do_open ()
{
    s = se_new sequence(source_child.ts);

    source_child.op->open();
    seq_filled = false;
    need_reopen = false;
    first_time = true;

    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_complex;
        p.op = this;
        p.cvc = se_new complex_var_consumption;
        p.tuple_pos = i;
    }

	data_child.op->open();
}

void PPLet::do_reopen()
{
    source_child.op->reopen();
    data_child.op->reopen();

    seq_filled = false;
    first_time = true;
    s->clear();
    need_reopen = false;
    reinit_consumer_table();
}

void PPLet::do_close()
{
    source_child.op->close();
    data_child.op->close();

    delete s;
    s = NULL;
}

void PPLet::do_next(xqp_tuple &t)
{
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
}

PPIterator* PPLet::do_copy(dynamic_context *_cxt_)
{
    PPLet *res = need_to_check_type ? se_new PPLet(_cxt_, info, var_dscs, source_child, data_child, st)
                                    : se_new PPLet(_cxt_, info, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);

    return res;
}

void PPLet::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}


var_c_id PPLet::do_register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(cxt->get_var_producer(dsc, var_cxt).cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPLet::do_next(xqp_tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->get_var_producer(dsc, var_cxt);
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
}

void PPLet::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).cvc->at(id) = 0;
}

void PPLet::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPLet::reinit_consumer_table()
{
    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (size_t j = 0; j < p.cvc->size(); j++) p.cvc->at(j) = 0;
    }
}
