/*
 * File:  PPStore.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPLAST_H
#define __PPLAST_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPLast : public PPVarIterator
{
private:
    var_dsc last_dsc;
    PPOpIn child;
    sequence *s;
    
    int64_t last, pos;
    bool last_computed;
    
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
    virtual var_c_id do_register_consumer(var_dsc dsc);
    virtual void do_next  (xqp_tuple &t, var_dsc dsc, var_c_id id);
    virtual void do_reopen(var_dsc dsc, var_c_id id);
    virtual void do_close (var_dsc dsc, var_c_id id);
    
public:
    PPLast(dynamic_context *_cxt_,
           operation_info _info_,
           var_dsc _last_dsc_,
           PPOpIn _child_);
    virtual ~PPLast();
    inline var_dsc get_last_var_dsc() { return last_dsc; }
};


#endif /* __PPLAST_H */
