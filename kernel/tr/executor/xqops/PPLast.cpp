/*
 * File:  PPStore.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPLast.h"


PPLast::PPLast(dynamic_context *_cxt_,
               operation_info _info_,
               var_dsc _last_dsc_,
               PPOpIn _child_) : PPVarIterator(_cxt_, _info_),
                                 last_dsc(_last_dsc_),
                                 child(_child_),
                                 s(NULL)
{
}

PPLast::~PPLast()
{
    delete (child.op);
    child.op = NULL;
}

void PPLast::do_open ()
{
    child.op->open();
    s = se_new sequence_tmp(child.ts);
    last = 0;
    pos = 0;
    last_computed = false;
    
    producer &p = cxt->var_cxt.producers[last_dsc];
    p.type = pt_lazy_simple;
    p.op = this;
    p.svc = se_new simple_var_consumption;
    p.tuple_pos = 0;
}

void PPLast::do_reopen()
{
    s->clear();
    last = 0;
    pos = 0;
    last_computed = false;

    producer &p = cxt->var_cxt.producers[last_dsc];

    for (unsigned int j = 0; j < p.svc->size(); j++) 
        p.svc->at(j) = true;
}

void PPLast::do_close()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPLast::do_next (tuple &t)
{
    if(last_computed) 
    {
        if(pos < s->size()) 
        {
            s->get(t, pos);
            pos++;
        }
        else
        {
            t.set_eos();
            do_reopen();
        }
    }
    else
    {
        child.op->next(t);
        last++;
        if(t.is_eos()) last = 0;
    }
}

var_c_id PPLast::do_register_consumer(var_dsc dsc)
{
    simple_var_consumption &svc = *(cxt->var_cxt.producers[dsc].svc);
    svc.push_back(true);
    return svc.size() - 1;
}

void PPLast::do_next(tuple &t, var_dsc dsc, var_c_id id)
{  
    producer &p = cxt->var_cxt.producers[dsc];

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;

        if(!last_computed) 
        {
            child.op->next(t);
            while(!t.is_eos()) 
            {
                s->add(t);
                last++;
                child.op->next(t);
            }
            last_computed = true;
        }
        
        t.copy(tuple_cell::atomic(last));
    }
	else
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPLast::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->var_cxt.producers[dsc].svc->at(id) = true;
}

void PPLast::do_close(var_dsc dsc, var_c_id id)
{
}


PPIterator* PPLast::do_copy(dynamic_context *_cxt_)
{
    PPLast *res = se_new PPLast(_cxt_, info, last_dsc, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
