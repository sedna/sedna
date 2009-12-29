/*
 * File:  PPSelect.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSelect.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPVisitor.h"


PPSelect::PPSelect(dynamic_context *_cxt_,
                   operation_info _info_,
                   arr_of_var_dsc _var_dscs_,
                   PPOpIn _source_child_,
                   PPOpIn _data_child_) : PPVarIterator(_cxt_, _info_),
                                          var_dscs(_var_dscs_),
                                          source_child(_source_child_),
                                          data_child(_data_child_),
                                          data(_data_child_.ts),
                                          source(_source_child_.ts),
                                          check_type(false)
{
}

PPSelect::PPSelect(dynamic_context *_cxt_,
                   operation_info _info_,
                   arr_of_var_dsc _var_dscs_,
                   PPOpIn _source_child_,
                   PPOpIn _data_child_,
                   const sequence_type &_st_) : PPVarIterator(_cxt_, _info_),
                                                var_dscs(_var_dscs_),
                                                source_child(_source_child_),
                                                data_child(_data_child_),
                                                data(_data_child_.ts),
                                                source(_source_child_.ts),
                                                st(_st_),
                                                check_type(true)
{
}

PPSelect::PPSelect(dynamic_context *_cxt_,
                   operation_info _info_,
                   arr_of_var_dsc _var_dscs_,
                   PPOpIn _source_child_,
                   PPOpIn _data_child_,
                   tuple _source_) : PPVarIterator(_cxt_, _info_),
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


void PPSelect::do_open ()
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

void PPSelect::do_reopen()
{
    source_child.op->reopen();
}

void PPSelect::do_close()
{
    source_child.op->close();
    data_child.op->close();
}

void PPSelect::do_next(tuple &t)
{
    while (true)
    {
        if (standard) source_child.op->next(t);
        else { t = source; standard = true; }
        cur_tuple = &t;

        if (t.is_eos()) return;

        if (check_type)
        {
            if (st.oi == st_empty || !type_matches_single(t.cells[0], st.type))
                throw XQUERY_EXCEPTION2(XPTY0004, "Type of a value bound to the variable does not match the declared type according to the rules for SequenceType matching.");
        }

        if (first_time) first_time = false;
        else
        {
            reinit_consumer_table();
            if (!eos_reached) data_child.op->reopen();
        }

        tuple_cell tc = effective_boolean_value(data_child, data, eos_reached);

        if (tc.get_xs_boolean()) return;
    }
}

PPIterator* PPSelect::do_copy(dynamic_context *_cxt_)
{
    PPSelect *res = se_new PPSelect(_cxt_, info, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op   = data_child.op->copy(_cxt_);
    return res;
}

void PPSelect::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}


var_c_id PPSelect::do_register_consumer(var_dsc dsc)
{
    cxt->var_cxt.producers[dsc].svc->push_back(true);
    return cxt->var_cxt.producers[dsc].svc->size() - 1;
}

void PPSelect::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
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
}

void PPSelect::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->var_cxt.producers[dsc].svc->at(id) = true;
}

void PPSelect::do_close(var_dsc dsc, var_c_id id)
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
