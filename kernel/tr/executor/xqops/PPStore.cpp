/*
 * File:  PPStore.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPStore.h"
#include "tr/executor/base/visitor/PPVisitor.h"


using namespace std;


PPStore::t_stored_seqs PPStore::stored_seqs;

PPStore::PPStore(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPStore"),
                                   child(_child_),
								   s(NULL)
{
}

PPStore::~PPStore()
{
    delete (child.op);
    child.op = NULL;
}

void PPStore::do_open ()
{
    child.op->open();

    s = se_new sequence(child.ts);

    pos = 0;
    eos_pos = 0;
    sequence_loaded = false;
}

void PPStore::do_reopen()
{
    pos = 0;
}

void PPStore::do_close()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPStore::do_next (xqp_tuple &t)
{
    if (pos < eos_pos) s->get(t, pos++);
    else
    {
        if (sequence_loaded) 
        {
            pos = 0;
            t.set_eos();
        }
        else
        {
            child.op->next(t);

            if (t.is_eos()) 
            {
                pos = 0;
                sequence_loaded = true;
            }
            else
            {
                s->add(t);
                pos++;
                eos_pos++;
            }
        }
    }
}

PPIterator* PPStore::do_copy(dynamic_context *_cxt_)
{
    PPStore *res = se_new PPStore(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPStore::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
