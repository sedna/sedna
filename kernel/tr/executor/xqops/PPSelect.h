/*
 * File:  PPSelect.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSELECT_H
#define __PPSELECT_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPSelect : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    PPOpIn data_child;
    xqp_tuple data;
    xqp_tuple source;
    xqp_tuple *cur_tuple;

    sequence_type st; // type to check in 'every' and 'some'

    bool first_time;
    bool eos_reached;
    bool standard;
    bool check_type;

    inline void reinit_consumer_table();

    PPSelect(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_,
             PPOpIn _source_child_,
             PPOpIn _data_child_,
             xqp_tuple _source_);

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
    PPSelect(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_,
             PPOpIn _source_child_,
             PPOpIn _data_child_);

    PPSelect(dynamic_context *_cxt_,
             operation_info _info_,
             arr_of_var_dsc _var_dscs_,
             PPOpIn _source_child_,
             PPOpIn _data_child_,
             const sequence_type &_st_);

    virtual ~PPSelect();

    inline bool is_check_type() { return check_type; }
    inline const sequence_type& get_sequence_type() { return st; }
    inline const arr_of_var_dsc& get_variable_descriptors() { return var_dscs; }
};



#endif /* __PPSELECT_H */
