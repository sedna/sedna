/*
 * File:  PPReturn.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPReturn.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPReturn::PPReturn(dynamic_context *_cxt_,
                   operation_info _info_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_,
                   var_dsc _pos_dsc_,
                   const sequence_type& _st_) : PPVarIterator(_cxt_, _info_, "PPReturn"),
                                                var_dscs(_var_dscs_),
                                                source_child(_source_child_),
                                                source(_source_child_.ts),
                                                data_child(_data_child_),
                                                need_to_check_type(true),
                                                pos_dsc(_pos_dsc_),
                                                st(_st_)
{
}

PPReturn::PPReturn(dynamic_context *_cxt_,
                   operation_info _info_,
                   arr_of_var_dsc _var_dscs_, 
                   PPOpIn _source_child_, 
                   PPOpIn _data_child_,
                   var_dsc _pos_dsc_) : PPVarIterator(_cxt_, _info_, "PPReturn"),
                                        var_dscs(_var_dscs_),
                                        source_child(_source_child_),
                                        source(_source_child_.ts),
                                        data_child(_data_child_),
                                        need_to_check_type(false),
                                        pos_dsc(_pos_dsc_)
{
}

PPReturn::~PPReturn()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPReturn::do_open ()
{
    pos = 0;
    source_child.op->open();
    first_time = true;

    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = i;
    }

    if (pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = 0;
    }

	data_child.op->open();
}

void PPReturn::do_reopen()
{
    pos = 0;
    source_child.op->reopen();
    data_child.op->reopen();

    first_time = true;
    reinit_consumer_table();
}

void PPReturn::do_close()
{
    source_child.op->close();
    data_child.op->close();
}

void PPReturn::do_next(tuple &t)
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
        
        pos++;
        
        if (source.is_eos())
        {
            t.set_eos();
            first_time = true;          // reopens automatically
            pos = 0;                    // reopens automatically
            reinit_consumer_table();    // reopens automatically
            return;
        }

        if (need_to_check_type)
        {
        	if (st.oi == st_empty || !type_matches_single(source.cells[0], st.type)) 
        		throw XQUERY_EXCEPTION2(XPTY0004, "Type of a value bound to the variable does not match the declared type according to the rules for SequenceType matching.");
        }

        reinit_consumer_table();

        // there should be 'data_child.op->reopen()' call but data child reopens automatically
        data_child.op->next(t);
    }
}

PPIterator* PPReturn::do_copy(dynamic_context *_cxt_)
{
    PPReturn *res = need_to_check_type ? se_new PPReturn(_cxt_, info, var_dscs, source_child, data_child, pos_dsc, st) 
                                       : se_new PPReturn(_cxt_, info, var_dscs, source_child, data_child, pos_dsc); 
    
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);

    return res;
}

void PPReturn::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}

var_c_id PPReturn::do_register_consumer(var_dsc dsc)
{
    simple_var_consumption &svc = *(cxt->get_var_producer(dsc, var_cxt).svc);
    svc.push_back(true);
    return svc.size() - 1;
}

void PPReturn::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->get_var_producer(dsc, var_cxt);

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        t.copy(dsc == pos_dsc ? tuple_cell::atomic(pos)
                              : source.cells[p.tuple_pos]);
    }
	else
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPReturn::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).svc->at(id) = true;
}

void PPReturn::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPReturn::reinit_consumer_table()
{
    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (unsigned int j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }

    if (pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        for (unsigned int j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}
