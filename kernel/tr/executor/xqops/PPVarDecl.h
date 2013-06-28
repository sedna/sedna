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
protected:
    int v_dsc;

    PPOpIn child;
    xqp_tuple source;

    bool need_to_check_type;
    sequence_type st;

    bool seq_filled;
    bool first_time;
    sequence *s;

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
    PPVarDecl(dynamic_context *_cxt_,
              operation_info _info_,
              int _v_dsc_, 
              PPOpIn _child_, 
              const sequence_type& _st_);

    PPVarDecl(dynamic_context *_cxt_,
              operation_info _info_,
              int _v_dsc_, 
              PPOpIn _child_);

    virtual ~PPVarDecl();
    
    inline bool is_check_type() { return need_to_check_type; }
    inline const sequence_type& get_type() { return st; }
    inline var_dsc get_variable_descriptor() { return v_dsc; }
};

#endif /* __PPVARDECL_H */
