/*
 * File:  PPStore.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSTORE_H
#define __PPSTORE_H

#include <map>
#include "PPBase.h"

class PPStore : public PPIterator
{
private:
    PPOpIn child;

    sequence_tmp *s;
    int pos;
    int eos_pos;
    bool sequence_loaded;

    typedef std::map<int, sequence_tmp*> t_stored_seqs;
    static t_stored_seqs stored_seqs;


    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPStore(variable_context *_cxt_,
            PPOpIn _child_);
    virtual ~PPStore();
};


#endif
