/*
 * File:  PPFunCall.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPFUNCALL_H
#define __PPFUNCALL_H

#include <string>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"


//#define STRICT_FUNS
// actually, the max size of sequence when fun is called in strict mode is STRICT_FUNS_BOUND - 1
#define STRICT_FUNS_BOUND	5

//   function conversion rules   
class fun_conv_rules
{
private:
    const sequence_type *st;
    PPIterator *child;
    int num;
    int arg_num;
    int __xquery_line;

public:
    fun_conv_rules(const sequence_type *_st_, PPIterator *_child_, int _arg_num_, int _xquery_line_ = 0) :
                   st(_st_), child(_child_), num(0), arg_num(_arg_num_), __xquery_line(_xquery_line_) {}
    ~fun_conv_rules() {}

    void reopen() { child->reopen(); num = 0; }
    std::string error();
    void next(tuple &t);
};

//   function argument   
class fun_arg
{
private:
    fun_conv_rules fcr;
    sequence *s;
    bool seq_filled;
    int __xquery_line;

public:

#ifdef STRICT_FUNS
    void init();
#endif

    bool is_filled() { return seq_filled; }
    sequence *get_sequence() { return s; }
    void reopen();
    void next(tuple /*out*/ &t, var_c_id /*out*/ &id);

    fun_arg(const sequence_type *_st_, PPIterator *_child_, int _arg_num_, int _xquery_line_ = 0) :
            fcr(_st_, _child_, _arg_num_, _xquery_line_), seq_filled(false), __xquery_line(_xquery_line_) { s = se_new sequence(1); }
    ~fun_arg() { delete s; }
};



class PPFunCall : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    arr_of_PPOpIn ch_arr;
    function_id fn_id;

    PPIterator *body;
    fun_conv_rules *body_fcr;
    fun_arg** args;
    int args_num;
    dynamic_context *new_cxt;
#ifdef STRICT_FUNS
    sequence* s;
    int spos;
#endif
    bool need_reopen;
    bool is_body_opened;


    inline void reinit_consumer_table();

    void children(arr_of_PPOpIn &_ch_arr_)
    {
        _ch_arr_ = ch_arr;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual char* get_error_msg() const { return "Error in function call."; }

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFunCall(dynamic_context *_cxt_,
              const arr_of_PPOpIn &_ch_arr_,
              function_id _fn_id_);
    virtual ~PPFunCall();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
    virtual void close (var_dsc dsc, var_c_id id);
};



#endif
