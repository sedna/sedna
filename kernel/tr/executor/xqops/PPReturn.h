/*
 * File:  PPReturn.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPRETURN_H
#define __PPRETURN_H

#include "sedna.h"
#include "PPBase.h"
#include "SequenceType.h"


class PPReturn : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    tuple source;

    PPOpIn data_child;

    bool first_time;
    
    int pos;				  //stores current position;
    bool need_to_check_type;  //if 'st' is absent this flag must be false;
    var_dsc pos_dsc;          //-1 if counter is not used;
    sequence_type st;         //

    inline void reinit_consumer_table();

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }

    /*PPReturn(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             tuple _source_);*/

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    /*PPReturn(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_);*/

    PPReturn(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             var_dsc _pos_dsc_,
             const sequence_type& _st_);

    
    PPReturn(dynamic_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             var_dsc _pos_dsc_);

    virtual ~PPReturn();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
    virtual void close (var_dsc dsc, var_c_id id);
};



#endif
