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
    tuple source;

    PPOpIn data_child;

    bool need_to_check_type;
    sequence_type st;

    bool seq_filled;
    bool need_reopen;
    bool first_time;
    sequence_tmp *s;

    inline void reinit_consumer_table();

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }

//    PPLet(dynamic_context *_cxt_,
//          arr_of_var_dsc _var_dscs_, 
//          PPOpIn _source_child_, 
//          PPOpIn _data_child_,
//          tuple _source_);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    
    PPLet(dynamic_context *_cxt_,
          arr_of_var_dsc _var_dscs_, 
          PPOpIn _source_child_, 
          PPOpIn _data_child_,
          const sequence_type& _st_);

    PPLet(dynamic_context *_cxt_,
          arr_of_var_dsc _var_dscs_, 
          PPOpIn _source_child_, 
          PPOpIn _data_child_);

    virtual ~PPLet();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
    virtual void close (var_dsc dsc, var_c_id id);
};



#endif
