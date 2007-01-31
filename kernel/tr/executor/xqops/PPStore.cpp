/*
 * File:  PPStore.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPStore.h"

using namespace std;

PPStore::t_stored_seqs PPStore::stored_seqs;

PPStore::PPStore(dynamic_context *_cxt_,
                 PPOpIn _child_) : PPIterator(_cxt_),
                                   child(_child_),
								   s(NULL)
{
}

PPStore::~PPStore()
{
    delete (child.op);
    child.op = NULL;
}

void PPStore::open ()
{
    child.op->open();

    s = new sequence_tmp(child.ts);

    pos = 0;
    eos_pos = 0;
    sequence_loaded = false;
}

void PPStore::reopen ()
{
    pos = 0;
}

void PPStore::close ()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPStore::next (tuple &t)
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

PPIterator* PPStore::copy(dynamic_context *_cxt_)
{
    PPStore *res = new PPStore(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPStore::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    PPOpIn child;
    ((PPStore*)cur)->children(child);

    void *s_r;
    bool s_s = (child.op->res_fun())(child.op, cxt, s_r);

    if (!s_s) // if expression is not strict
    { // create PPStore and transmit state
        child.op = (PPIterator*)s_r;
        r = new PPStore(cxt, child);
        return false;
    }

    t_stored_seqs::iterator it = stored_seqs.find((int)cur);

    if (it == stored_seqs.end())
    {
        it = stored_seqs.insert(pair<int,sequence_tmp*>((int)cur, (sequence_tmp*)s_r)).first;
    }

    sequence *data = it->second;
    sequence *res_seq = new sequence(child.ts);
    tuple t(child.ts);
    for (int i = 0; i < data->size(); i++)
    {
        data->get(t, i);
        res_seq->add(t);
    }

    r = res_seq;
*/
    return true;
}
