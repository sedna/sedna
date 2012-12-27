/*
 * File:  PPSequenceTypes.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPSequenceTypes.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/fo/casting_operations.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCast
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPCast::PPCast(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_) : PPIterator(_cxt_, _info_, "PPCast"),
                                          child(_child_),
                                          target_type(_target_type_),
                                          can_be_empty_seq(_can_be_empty_seq_)
{
}

PPCast::~PPCast()
{
    delete child.op;
    child.op = NULL;
}

void PPCast::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPCast::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPCast::do_close()
{
    child.op->close();
}

void PPCast::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        child.op->next(t);

        if (t.is_eos()) 
        {
            if (can_be_empty_seq)
            {
                first_time = true;
                t.set_eos();
                return;
            }
            else throw XQUERY_EXCEPTION2(XPTY0004, "cast expression ('?' is not specified in target type but empty sequence is given)");
        }

        tuple_cell tc = atomize(child.get(t));

        child.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "cast expression (the result of atomization is a sequence of more than one atomic value)");

        t.copy(cast(tc, target_type));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCast::do_copy(dynamic_context *_cxt_)
{
    PPCast *res = new PPCast(_cxt_, info, child, target_type, can_be_empty_seq);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

void PPCast::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCastable
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPCastable::PPCastable(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_,
                       xmlscm_type _target_type_,
                       bool _can_be_empty_seq_) : PPIterator(_cxt_, _info_, "PPCastable"),
                                                  child(_child_),
                                                  target_type(_target_type_),
                                                  can_be_empty_seq(_can_be_empty_seq_)
{
}

PPCastable::~PPCastable()
{
    delete child.op;
    child.op = NULL;
}

void PPCastable::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPCastable::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPCastable::do_close()
{
    child.op->close();
}

void PPCastable::do_next (tuple &t)
{
    bool res;
    if (first_time)
    {
        first_time = false;
        child.op->next(t);

        if (t.is_eos()) 
        {
            if (can_be_empty_seq) res = true;
            else res = false; //cast expression ('?' is not specified in target type but empty sequence is given)
        }
        else
        {
            tuple_cell tc = atomize(child.get(t));
            child.op->next(t);
            if (!t.is_eos())  res = false; //cast expression (the result of atomization is a sequence of more than one atomic value)
            else res = is_castable(tc, target_type);
        }
        
        t.copy(tuple_cell::atomic(res));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCastable::do_copy(dynamic_context *_cxt_)
{
    PPCastable *res = new PPCastable(_cxt_, info, child, target_type, can_be_empty_seq);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

void PPCastable::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPInstanceOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPInstanceOf::PPInstanceOf(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _child_,
                           const sequence_type& _st_) : PPIterator(_cxt_, _info_, "PPInstanceOf"),
                                                        child(_child_),
                                                        st(_st_)
{
}

PPInstanceOf::~PPInstanceOf()
{
    delete child.op;
    child.op = NULL;
}

void PPInstanceOf::do_open ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::do_reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::do_close()
{
    child.op->close();
}

bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st);

void PPInstanceOf::do_next (tuple &t)
{
        
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        bool res = type_matches(child, t, eos_reached, st);

        t.copy(tuple_cell::atomic(res));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPInstanceOf::do_copy(dynamic_context *_cxt_)
{
    PPInstanceOf *res = new PPInstanceOf(_cxt_, info, child, st);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

void PPInstanceOf::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTreat
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPTreat::PPTreat(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_,
                 const sequence_type& _st_) : PPIterator(_cxt_, _info_, "PPTreat"),
                                              child(_child_),
                                              st(_st_),
                                              s(NULL)
{
}

PPTreat::~PPTreat()
{
    delete child.op;
    child.op = NULL;
}

void PPTreat::do_open ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
    
    s = new sequence(child.ts);
    pos = 0;
}                                     

void PPTreat::do_reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
    pos = 0;
    s->clear();
}

void PPTreat::do_close()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPTreat::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        if (!eos_reached) child.op->reopen();
        bool res = type_matches(child, s, t, eos_reached, st);
        if(res == false) throw XQUERY_EXCEPTION(XPDY0050);
    }
   
    if(pos < s->size())
    {
        s->get(t, pos++);
        return;
    }
    else if(!eos_reached)
    {
        child.op->next(t);
        if(t.is_eos()) eos_reached = true;
        else return;
    }

    t.set_eos();
    first_time = true;
    s->clear();
    pos=0;
}

PPIterator* PPTreat::do_copy(dynamic_context *_cxt_)
{
    PPTreat *res = new PPTreat(_cxt_, info, child, st);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

void PPTreat::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTypeswitch
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPTypeswitch::PPTypeswitch(dynamic_context *_cxt_,
                           operation_info _info_,
                           arr_of_var_dsc _var_dscs_, 
                           PPOpIn _source_child_, 
                           const arr_of_sequence_type& _types_,
                           arr_of_PPOpIn _cases_,
                           PPOpIn _default_child_): PPVarIterator(_cxt_, _info_, "PPTypeswitch"),
                                                    var_dscs(_var_dscs_),
                                                    source_child(_source_child_),
                                                    default_child(_default_child_),
                                                    cases(_cases_),
                                                    types(_types_),
                                                    s(NULL)
{
    if(cases.size() != types.size()) 
        throw USER_EXCEPTION2(SE1003, "PPTypeswitch: number of cases must be equal to number of types");
}


PPTypeswitch::~PPTypeswitch()
{
    delete source_child.op;
    source_child.op = NULL;
    
    for(size_t i = 0; i < cases.size(); i++)
    {
        delete (cases[i].op);
        cases[i].op = NULL;
    }
    
    delete default_child.op;            
    default_child.op = NULL;
}


void PPTypeswitch::do_open ()
{
    s = new sequence(source_child.ts);

    source_child.op->open();
    first_time = true;
    eos_reached = false;
    need_reopen = false;
    effective_case = NULL;
    
    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_complex;
        p.op = this;
        p.cvc = new complex_var_consumption;
        p.tuple_pos = i;
    }
    
    for(size_t i = 0; i < cases.size(); i++)
        (cases[i].op) -> open();
    
    default_child.op->open();
}

void PPTypeswitch::do_reopen()
{
    if(!eos_reached) source_child.op->reopen();
    if(effective_case != NULL) 
    {
        (effective_case->op) -> reopen();
        effective_case = NULL;
    }
    
    eos_reached = false;
    first_time = true;
    need_reopen = false;

    s->clear();
    reinit_consumer_table();
}

void PPTypeswitch::do_close()
{
    source_child.op->close();   

    for(size_t i = 0; i < cases.size(); i++)
        (cases[i].op) -> close();
     
    default_child.op->close();
    delete s;
}

void PPTypeswitch::do_next(tuple &t)
{
    if (first_time)
    {
        if(need_reopen)
        {
            if(!eos_reached) source_child.op->reopen();
            s->clear();
            reinit_consumer_table();
            need_reopen = false;
        }

        first_time = false;
        eos_reached = false;

        effective_case = &default_child;
        for(size_t i = 0; i < cases.size(); i++)
        {
            if(type_matches(source_child, s, t, eos_reached, types[i]))
            {
                effective_case = &cases[i]; 
                break;
            }
        }
    }
   
    (effective_case->op) -> next(t);
    
    if(t.is_eos()) 
    {
        first_time = true;
        need_reopen = true;
    }
}

PPIterator* PPTypeswitch::do_copy(dynamic_context *_cxt_)
{
    PPTypeswitch *res = new PPTypeswitch(_cxt_,
                                         info, 
                                         var_dscs, 
                                         source_child, 
                                         types, 
                                         cases, 
                                         default_child);
    
    for (size_t i = 0; i < cases.size(); i++)
        res->cases[i].op = cases[i].op->copy(_cxt_);
    
    res->source_child.op = source_child.op->copy(_cxt_);
    res->default_child.op = default_child.op->copy(_cxt_);

    return res;
}

void PPTypeswitch::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    source_child.op->accept(v);
    for (size_t i = 0; i < cases.size(); i++)
        cases[i].op->accept(v);
    default_child.op->accept(v);
    v.pop();
}

var_c_id PPTypeswitch::do_register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(cxt->get_var_producer(dsc, var_cxt).cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPTypeswitch::do_next(tuple &t, var_dsc dsc, var_c_id id)                    
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
        if (eos_reached)
        {
            t.set_eos();
            cvc[id] = 0;
        }
        else
        {
            source_child.op->next(source);
            if (source.is_eos())
            {
                eos_reached = true;
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

void PPTypeswitch::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).cvc->at(id) = 0;
}

void PPTypeswitch::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPTypeswitch::reinit_consumer_table()
{
    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (size_t j = 0; j < p.cvc->size(); j++) p.cvc->at(j) = 0;
    }
}
