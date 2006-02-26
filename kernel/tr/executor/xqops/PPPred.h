/*
 * File:  PPPred.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPPRED_H
#define __PPPRED_H

#include "PPBase.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred1
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
class PPPred1 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    PPOpIn data_child;
    tuple data;
    tuple *cur_tuple;
    int pos;
    var_dsc pos_dsc;

    bool first_time;
    bool eos_reached;


    inline void reinit_consumer_table();

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }

/*
    PPPred1(variable_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             tuple _source_);
*/
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPPred1(variable_context *_cxt_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            PPOpIn _data_child_,
            var_dsc _pos_dsc_ = -1);

    virtual ~PPPred1();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next(tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
};




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred2
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
class PPPred2 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    PPOpIn data_child;
    tuple data;
    tuple *cur_tuple;
    int pos;
    var_dsc pos_dsc;
    var_dsc lst_dsc;
    sequence *s;

    bool first_time;
    bool eos_reached;


    inline void reinit_consumer_table();

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }

/*
    PPPred2(variable_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             tuple _source_);
*/
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPPred2(variable_context *_cxt_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            PPOpIn _data_child_,
            var_dsc _pos_dsc_,
            var_dsc _lst_dsc_);

    virtual ~PPPred2();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next(tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
};


#endif
