/*
 * File:  PPLet.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPLET_H
#define __PPLET_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPLet : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    xqp_tuple source;

    PPOpIn data_child;

    bool need_to_check_type;
    sequence_type st;

    bool seq_filled;
    bool need_reopen;
    bool first_time;
    sequence *s;

    inline void reinit_consumer_table();

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
    PPLet(dynamic_context *_cxt_,
          operation_info _info_,
          arr_of_var_dsc _var_dscs_, 
          PPOpIn _source_child_, 
          PPOpIn _data_child_,
          const sequence_type& _st_);

    PPLet(dynamic_context *_cxt_,
          operation_info _info_,
          arr_of_var_dsc _var_dscs_, 
          PPOpIn _source_child_, 
          PPOpIn _data_child_);

    virtual ~PPLet();
    
    inline bool is_check_type() const { return need_to_check_type; }
    inline const sequence_type& get_type() const { return st;}
    inline const arr_of_var_dsc& get_variable_descriptors() const { return var_dscs; }
};

#endif
