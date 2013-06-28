/*
 * File:  PPStore.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSTORE_H
#define __PPSTORE_H

#include <map>
#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPStore : public PPIterator
{
private:
    PPOpIn child;

    sequence *s;
    int pos;
    int eos_pos;
    bool sequence_loaded;

    typedef std::map<int, sequence*> t_stored_seqs;
    static t_stored_seqs stored_seqs;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPStore(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _child_);
    virtual ~PPStore();
};


#endif
