/*
 * File:  PPReturn.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPRETURN_H
#define __PPRETURN_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/SequenceType.h"


class PPReturn : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    xqp_tuple source;

    PPOpIn data_child;

    bool first_time;
    
    int64_t  pos;               //stores current position;
    bool need_to_check_type;    //if 'st' is absent this flag must be false;
    var_dsc pos_dsc;            //-1 if counter is not used;
    sequence_type st;

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
    PPReturn(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             var_dsc _pos_dsc_,
             const sequence_type& _st_);
    
    PPReturn(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             var_dsc _pos_dsc_);

    virtual ~PPReturn();

    inline bool is_check_type() const { return need_to_check_type; }
    inline const sequence_type& get_type() const { return st;}
    inline const arr_of_var_dsc& get_variable_descriptors() const { return var_dscs; }
    inline var_dsc get_position_var_dsc() const { return pos_dsc; }
};

#endif /* __PPRETURN_H */
