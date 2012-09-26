/*
 * File:  PPStore.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPLast.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPLast::PPLast(dynamic_context *_cxt_,
               operation_info _info_,
               var_dsc _last_dsc_,
               PPOpIn _child_) : PPVarIterator(_cxt_, _info_, "PPLast"),
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
    s = new sequence(child.ts);
    last = 0;
    pos = 0;
    last_computed = false;
    
    producer &p = cxt->get_var_producer(last_dsc, var_cxt);
    p.type = pt_lazy_simple;
    p.op = this;
    p.svc = new simple_var_consumption;
    p.tuple_pos = 0;
}

void PPLast::do_reopen()
{
    s->clear();
    last = 0;
    pos = 0;
    last_computed = false;

    producer &p = cxt->get_var_producer(last_dsc, var_cxt);

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
    simple_var_consumption &svc = *(cxt->get_var_producer(dsc, var_cxt).svc);
    svc.push_back(true);
    return svc.size() - 1;
}

void PPLast::do_next(tuple &t, var_dsc dsc, var_c_id id)
{  
    producer &p = cxt->get_var_producer(dsc, var_cxt);

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
    cxt->get_var_producer(dsc, var_cxt).svc->at(id) = true;
}

void PPLast::do_close(var_dsc dsc, var_c_id id)
{
}


PPIterator* PPLast::do_copy(dynamic_context *_cxt_)
{
    PPLast *res = new PPLast(_cxt_, info, last_dsc, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPLast::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
