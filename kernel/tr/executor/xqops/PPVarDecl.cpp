/*
 * File:  PPVarDecl.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPVarDecl.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPVarDecl::PPVarDecl(dynamic_context *_cxt_,
                     operation_info _info_,
                     int _v_dsc_, 
                     PPOpIn _child_) : PPVarIterator(_cxt_, _info_, "PPVarDecl"),
                                       v_dsc(_v_dsc_),
                                       child(_child_),
                                       source(_child_.ts),
                                       need_to_check_type(false)
{
}

PPVarDecl::PPVarDecl(dynamic_context *_cxt_,
                     operation_info _info_,
                     int _v_dsc_, 
                     PPOpIn _child_, 
                     const sequence_type& _st_) : PPVarIterator(_cxt_, _info_, "PPVarDecl"),
                                                  v_dsc(_v_dsc_),
                                                  child(_child_),
                                                  source(_child_.ts),
                                                  need_to_check_type(true),
                                                  st(_st_)
{
}

PPVarDecl::~PPVarDecl()
{
    delete child.op;
    child.op = NULL;
}


void PPVarDecl::do_open ()
{
    s = new sequence(child.ts);
    child.op->open();
    seq_filled = false;
    first_time = true;

    U_ASSERT(cxt->get_global_var_producer(v_dsc).op == this);
}

void PPVarDecl::do_reopen()
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::do_reopen");
}

void PPVarDecl::do_close()
{
    child.op->close();
    delete s;
    s = NULL;
}

void PPVarDecl::do_next(tuple &t)
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::do_next");
}

PPIterator* PPVarDecl::do_copy(dynamic_context *_cxt_)
{
	throw USER_EXCEPTION2(SE1003, "PPVarDecl::do_copy");
}

void PPVarDecl::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

var_c_id PPVarDecl::do_register_consumer(var_dsc dsc)
{
    global_producer &p = cxt->get_global_var_producer(dsc);
    if (p.fel.size() != 0)
    {
        unsigned i = p.fel.back();
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

void PPVarDecl::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
    global_producer &p = cxt->get_global_var_producer(dsc);
    complex_var_consumption &cvc = p.cvc;

    if (first_time && need_to_check_type)
    {
       if (!type_matches(child, s, source, seq_filled, st))
          throw XQUERY_EXCEPTION2(XPTY0004, "Type of a value bound to the variable does not match the declared type according to the rules for SequenceType matching.");
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

void PPVarDecl::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_global_var_producer(dsc).cvc[id] = 0;
}

void PPVarDecl::do_close(var_dsc dsc, var_c_id id)
{
    cxt->get_global_var_producer(dsc).fel.push_back(id);
}
