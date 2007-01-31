/*
 * File:  PPVarDecl.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPVarDecl.h"


PPVarDecl::PPVarDecl(dynamic_context *_cxt_,
                     int _v_dsc_, 
                     PPOpIn _child_) : PPVarIterator(_cxt_),
                                       v_dsc(_v_dsc_),
                                       child(_child_),
                                       source(_child_.ts),
                                       need_to_check_type(false)
{
}

PPVarDecl::PPVarDecl(dynamic_context *_cxt_,
                     int _v_dsc_, 
                     PPOpIn _child_, 
                     const sequence_type& _st_) : PPVarIterator(_cxt_),
                                                  v_dsc(_v_dsc_),
                                                  child(_child_),
                                                  source(_child_.ts),
                                                  st(_st_),
                                                  need_to_check_type(true)
{
}

PPVarDecl::~PPVarDecl()
{
    delete child.op;
    child.op = NULL;
}


void PPVarDecl::open ()
{
    s = new sequence_tmp(child.ts);
    child.op->open();
    seq_filled = false;
    first_time = false;
    cxt->glb_var_cxt.producers[v_dsc].op = this;
}

void PPVarDecl::reopen ()
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::reopen");
}

void PPVarDecl::close ()
{
    child.op->close();
    delete s;
    s = NULL;
}

void PPVarDecl::next(tuple &t)
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::next");
}

PPIterator* PPVarDecl::copy(dynamic_context *_cxt_)
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::copy");
}

var_c_id PPVarDecl::register_consumer(var_dsc dsc)
{
    global_producer &p = cxt->glb_var_cxt.producers[dsc];
    if (p.fel.size() != 0)
    {
        int i = p.fel.back();
        p.fel.pop_back();
        p.cvc[i] = 0;
        return i;
    }
    else
    {
        p.cvc.push_back(0);
        return p.cvc.size() - 1;
    }
}

void PPVarDecl::next(tuple &t, var_dsc dsc, var_c_id id)
{
    global_producer &p = cxt->glb_var_cxt.producers[dsc];
    complex_var_consumption &cvc = p.cvc;

    if (first_time && need_to_check_type)
    {
       if (!type_matches(child, s, t, seq_filled, st))
          throw USER_EXCEPTION2(XPTY0004, "Type of a value bound to the variable does not match the declared type according to the rules for SequenceType matching.");
       first_time = false;	
    }


    if (cvc[id] < s->size())
    {
        s->get(t, cvc[id]);
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
            child.op->next(t);
            if (t.is_eos())
            {
                seq_filled = true;
                cvc[id] = 0;
            }
            else
            {
                s->add(t);
                cvc[id]++;
            }
        }
    }
}

void PPVarDecl::reopen(var_dsc dsc, var_c_id id)
{
    cxt->glb_var_cxt.producers[dsc].cvc[id] = 0;
}

void PPVarDecl::close(var_dsc dsc, var_c_id id)
{
    cxt->glb_var_cxt.producers[dsc].fel.push_back(id);
}

bool PPVarDecl::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPVarDecl::result");
}
