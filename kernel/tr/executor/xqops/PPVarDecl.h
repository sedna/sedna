/*
 * File:  PPVarDecl.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPVARDECL_H
#define __PPVARDECL_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPVarDecl : public PPVarIterator
{
private:
    int v_dsc;

    PPOpIn child;
    tuple source;

    bool need_to_check_type;
    sequence_type st;

    bool seq_filled;
    bool first_time;
    sequence_tmp *s;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    
    PPVarDecl(dynamic_context *_cxt_,
              int _v_dsc_, 
              PPOpIn _child_, 
              const sequence_type& _st_);

    PPVarDecl(dynamic_context *_cxt_,
              int _v_dsc_, 
              PPOpIn _child_);

    virtual ~PPVarDecl();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
    virtual void close (var_dsc dsc, var_c_id id);
};

#endif
