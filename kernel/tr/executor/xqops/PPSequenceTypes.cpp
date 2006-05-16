/*
 * File:  PPSequenceTypes.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "PPSequenceTypes.h"
#include "PPUtils.h"
#include "casting_operations.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCast
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPCast::PPCast(variable_context *_cxt_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_) : PPIterator(_cxt_),
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

void PPCast::open  ()
{
    child.op->open();
    first_time = true;
}

void PPCast::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPCast::close ()
{
    child.op->close();
}

void PPCast::next  (tuple &t)
{
    if (first_time)
    {
		first_time = false;
        child.op->next(t);

        if (t.is_eos()) 
            if (can_be_empty_seq)
            {
                first_time = true;
                t.set_eos();
                return;
            }
            else throw USER_EXCEPTION2(XP0006, "cast expression ('?' is not specified in target type but empty sequence is given)");

        tuple_cell tc = atomize(child.get(t));

        child.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION2(XP0006, "cast expression (the result of atomization is a sequence of more than one atomic value)");

        t.copy(cast(tc, target_type));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCast::copy(variable_context *_cxt_)
{
    PPCast *res = new PPCast(_cxt_, child, target_type, can_be_empty_seq);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPCast::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPCast*)cur)->children(child);

    void *child_r;
    bool child_s = (child.op->res_fun())(child.op, cxt, child_r);

    if (!child_s) // if expression is not strict
    { // create PPCast and transmit state
        child.op = (PPIterator*)child_r;
        PPCast *res_op = new PPCast(cxt, child, 
                                    ((PPCast*)cur)->target_type, 
                                    ((PPCast*)cur)->can_be_empty_seq);

        r = res_op;
        return false;
    }

    sequence *child_seq = (sequence*)child_r;
    if (child_seq->size() == 0)
        if (((PPCast*)cur)->can_be_empty_seq)
        {
            r = child_seq;
            return true;
        }
        else throw USER_EXCEPTION2(XP0006, "cast expression ('?' is not specified in target type but empty sequence is given)");

    if (child_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "cast expression (the result of atomization is a sequence of more than one atomic value)");

    tuple t(1);
    child_seq->get(t, child_seq->begin());
    t.cells[0] = cast(atomize(t.cells[0]), ((PPCast*)cur)->target_type);
    child_seq->clear();
    child_seq->add(t);

    r = child_seq;
    return true;
}










///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCastable
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPCastable::PPCastable(variable_context *_cxt_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_) : PPIterator(_cxt_),
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

void PPCastable::open  ()
{
    child.op->open();
    first_time = true;
}

void PPCastable::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPCastable::close ()
{
    child.op->close();
}

void PPCastable::next  (tuple &t)
{
    bool res;
    if (first_time)
    {
		first_time = false;
        child.op->next(t);

        if (t.is_eos()) 
        {
            if (can_be_empty_seq)
            {
                first_time = true;
                res = true;
            }
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

PPIterator* PPCastable::copy(variable_context *_cxt_)
{
    PPCastable *res = new PPCastable(_cxt_, child, target_type, can_be_empty_seq);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPCastable::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPCastable::result");
}










///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPInstanceOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPInstanceOf::PPInstanceOf(variable_context *_cxt_,
                           PPOpIn _child_,
                           const sequence_type& _st_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        st(_st_)
{
}

PPInstanceOf::~PPInstanceOf()
{
    delete child.op;
    child.op = NULL;
}

void PPInstanceOf::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::close ()
{
    child.op->close();
}

bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st);

void PPInstanceOf::next  (tuple &t)
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

PPIterator* PPInstanceOf::copy(variable_context *_cxt_)
{
    PPInstanceOf *res = new PPInstanceOf(_cxt_, child, st);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPInstanceOf::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPInstanceOf::result");
}










///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTreat
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPTreat::PPTreat(variable_context *_cxt_,
                 PPOpIn _child_,
                 const sequence_type& _st_) : PPIterator(_cxt_),
                                              child(_child_),
                                              st(_st_)
{
}

PPTreat::~PPTreat()
{
    delete child.op;
    child.op = NULL;
}

void PPTreat::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
    
    s = new sequence(child.ts);
    pos = 0;
}                                     

void PPTreat::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
    pos = 0;
    s->clear();
}

void PPTreat::close ()
{
    child.op->close();
}

void PPTreat::next(tuple &t)
{
    if (first_time)
    {
		first_time = false;
        if (!eos_reached) child.op->reopen();
        bool res = type_matches(child, s, t, eos_reached, st);
        //!!! FIXME: error code must be XPDY0050 there
        if(res == false) throw USER_EXCEPTION(XP0050);
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

PPIterator* PPTreat::copy(variable_context *_cxt_)
{
    PPTreat *res = new PPTreat(_cxt_, child, st);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPTreat::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPTreat::result");
}










///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTypeswitch
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPTypeswitch::PPTypeswitch(variable_context *_cxt_,
                 		   arr_of_var_dsc _var_dscs_, 
                 		   PPOpIn _source_child_, 
				 		   const arr_of_sequence_type& _types_,
 		                   arr_of_PPOpIn _cases_,
            		       PPOpIn _default_child_): PPVarIterator(_cxt_),
				                                    var_dscs(_var_dscs_),
                                                    source_child(_source_child_),
                                                    types(_types_),
                                                    cases(_cases_),
                                                    default_child(_default_child_)
{
	if(cases.size() != types.size()) 
		throw USER_EXCEPTION2(SE1003, "PPTypeswitch: number of cases must be equal to number of types");
}


PPTypeswitch::~PPTypeswitch()
{
    delete source_child.op;
    source_child.op = NULL;
    
    for( int i = 0; i < cases.size(); i++)
    {
    	delete (cases[i].op);
    	cases[i].op = NULL;
    }
    
    delete default_child.op;            
    default_child.op = NULL;
}


void PPTypeswitch::open ()
{
    s = new sequence_tmp(source_child.ts);

    source_child.op->open();
    first_time = true;
    eos_reached = false;
    need_reopen = false;
    effective_case = NULL;
    
    for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        p.type = pt_lazy_simple;
        p.op = this;
        p.cvc = new complex_var_consumption;
        p.tuple_pos = i;
    }
    
    for(int i = 0; i < cases.size(); i++)
		(cases[i].op) -> open();
    
    default_child.op->open();
}

void PPTypeswitch::reopen()
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

void PPTypeswitch::close ()
{
	source_child.op->close();	

	for( int i = 0; i < cases.size(); i++)
        (cases[i].op) -> close();
     
    default_child.op->close();
    delete s;
}

void PPTypeswitch::next(tuple &t)
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
        for(int i = 0; i < cases.size(); i++)
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

PPIterator* PPTypeswitch::copy(variable_context *_cxt_)
{
    PPTypeswitch *res = new PPTypeswitch(_cxt_, 
                                         var_dscs, 
                                         source_child, 
                                         types, 
                                         cases, 
                                         default_child);
    
    for (int i = 0; i < cases.size(); i++)
        res->cases[i].op = cases[i].op->copy(_cxt_);
    
    res->source_child.op = source_child.op->copy(_cxt_);
    res->default_child.op = default_child.op->copy(_cxt_);

    return res;
}

var_c_id PPTypeswitch::register_consumer(var_dsc dsc)
{
	complex_var_consumption &cvc = *(cxt->producers[dsc].cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPTypeswitch::next(tuple &t, var_dsc dsc, var_c_id id)           	       
{
    producer &p = cxt->producers[dsc];
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

void PPTypeswitch::reopen(var_dsc dsc, var_c_id id)
{
    cxt->producers[dsc].svc->at(id) = 0;
}

inline void PPTypeswitch::reinit_consumer_table()
{
	for (int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->producers[var_dscs[i]];
        for (int j = 0; j < p.cvc->size(); j++) p.cvc->at(j) = 0;
    }
}

bool PPTypeswitch::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPTypeswitch::result");
}
