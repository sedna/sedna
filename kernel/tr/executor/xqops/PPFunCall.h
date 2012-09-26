/*
 * File:  PPFunCall.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __PPFUNCALL_H
#define __PPFUNCALL_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

/* Function conversion rules */
class fun_conv_rules
{
private:
    const sequence_type *st;
    PPIterator *child;
    int num;
    int arg_num;

public:
    fun_conv_rules(const sequence_type *_st_, 
                   PPIterator *_child_,
                   int _arg_num_) : st(_st_),
                                    child(_child_),
                                    num(0),
                                    arg_num(_arg_num_) {}
    virtual ~fun_conv_rules() {}

    void reopen() { 
        child->reopen(); num = 0; 
    }
    
    void next(tuple &t);
};

/* Function argument */
class fun_arg
{
private:
    fun_conv_rules fcr;
    sequence *s;
    bool seq_filled;

public:
    bool is_filled() { return seq_filled; }
    sequence *get_sequence() { return s; }
    void reopen();
    void next(tuple /*out*/ &t, var_c_id /*out*/ &id);

    fun_arg(const sequence_type *_st_, 
            PPIterator *_child_, 
            int _arg_num_): fcr(_st_, _child_, _arg_num_), 
                            seq_filled(false) 
    {
        s = new sequence(1); 
    }
    
    ~fun_arg() { 
        delete s; 
     }
};

class PPFunCall : public PPVarIterator
{
protected:
    arr_of_var_dsc var_dscs;

    arr_of_PPOpIn ch_arr;
    function_id fn_id;

    PPIterator *body;
    fun_conv_rules *body_fcr;
    fun_arg** args;
    unsigned args_num;

    variable_context *var_cxt;

    bool need_reopen;
    bool is_body_opened;

    inline void reinit_consumer_table();

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

    virtual var_c_id do_register_consumer(var_dsc dsc);
    virtual void do_next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void do_reopen(var_dsc dsc, var_c_id id);
    virtual void do_close (var_dsc dsc, var_c_id id);

public:    
    PPFunCall(dynamic_context *_cxt_,
              operation_info _info_,
              const arr_of_PPOpIn &_ch_arr_,
              function_id _fn_id_);
    virtual ~PPFunCall();

    inline function_id get_function_id() { return fn_id; }
};

#endif /* __PPFUNCALL_H */
