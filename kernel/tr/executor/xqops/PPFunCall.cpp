/*
 * File:  PPFunCall.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPFunCall.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/visitor/PPVisitor.h"


////////////////////////////////////////////////////////////////////////////////
/// Function argument check and conversion rules implementation
////////////////////////////////////////////////////////////////////////////////

static void
type_error(const char* message,
           int arg_num,
           const sequence_type* st)
{
    std::string res;
    if(arg_num != 0)
        res = "Error in function call. Argument [" + int2string(arg_num) + "] does not match the required type. Expected type is [" +  st->to_str() +  "]" + message;
    else
        res = "Error in function call. Return value does not match the required type. Expected type is [" +  st->to_str() +  "]" + message;
    throw XQUERY_EXCEPTION2(XPTY0004, res.c_str());
}


void fun_conv_rules::next(xqp_tuple &t)
{
    INCREASE_STACK_DEPTH
    CHECK_STACK_DEPTH

    child->next(t);

    if (!t.is_eos()) num++;

    switch (st->oi)
    {
        case st_empty         : if (num != 0) type_error(".", arg_num, st);
                                break;
        case st_one           : if (num == 0) type_error(", empty sequence is given.", arg_num, st);
                                if (num > 1) type_error(", more than one item is given.", arg_num, st);
                                break;
        case st_optional      : if (!(num == 0 || num == 1)) type_error(", more than one item is given.", arg_num, st);
                                break;
        case st_zero_or_more  : break;
        case st_one_or_more   : if (!(num >= 1)) type_error(", empty sequence is given.", arg_num, st);
                                break;
        default               : throw XQUERY_EXCEPTION2(SE1003, "unexpected case in function conversion rule.");
    }

    if (t.is_eos()) 
    {
        num = 0;
        DECREASE_STACK_DEPTH
        return;
    }

    tuple_cell tc = t.cells[0];
    if (st->type.type == st_atomic_type)
    {
        tc = atomize(tc);

        if(st->type.info.single_type != xs_anyAtomicType)
        {
            if (tc.get_atomic_type() == xs_untypedAtomic)
                tc = cast(tc, st->type.info.single_type);
            else
                type_promotion(tc, st->type.info.single_type);
        }

        t.copy(tc);
    }

    if (!type_matches_single(tc, st->type))
        type_error((", given type is [" + tc.type2string() + "].").c_str(), arg_num, st);

    DECREASE_STACK_DEPTH
}


void fun_arg::reopen()
{
    if (!seq_filled) fcr.reopen();
    seq_filled = false;
    s->clear();
}

void fun_arg::next(xqp_tuple /*out*/ &t, var_c_id /*out*/ &id)
{
    if (id < s->size())
    {
        s->get(t, id);
        id++;
    }
    else
    {
        if (seq_filled)
        {
            t.set_eos();
            id = 0;
        }
        else
        {
            fcr.next(t);
            if (t.is_eos())
            {
                seq_filled = true;
                id = 0;
            }
            else
            {
                s->add(t);
                id++;
            }
        }
    }
}



////////////////////////////////////////////////////////////////////////////////
/// PPFunCall
////////////////////////////////////////////////////////////////////////////////
PPFunCall::PPFunCall(dynamic_context *_cxt_,
                     operation_info _info_,
                     const arr_of_PPOpIn &_ch_arr_,
                     function_id _fn_id_) : PPVarIterator(_cxt_, _info_, "PPFunCall"),
                                            ch_arr(_ch_arr_),
                                            fn_id(_fn_id_),
                                            body(NULL),
                                            body_fcr(NULL),
                                            args(NULL),
                                            args_num(ch_arr.size()),
                                            var_cxt(NULL)
{
    for (unsigned i = 0; i < args_num; i++) {
        if (ch_arr[i].ts != 1)
            throw USER_EXCEPTION2(SE1003, "Children of PPFunCall operation have wrong tuple size");
    }
}

PPFunCall::~PPFunCall()
{
    for (unsigned i = 0; i < args_num; i++)
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }

    delete body;
    body = NULL;
}

void PPFunCall::do_open()
{
    for (unsigned i = 0; i < args_num; i++)
        ch_arr[i].op->open();

    need_reopen    = false;
    is_body_opened = false;
    is_in_arg_evaluation = false;
}

void PPFunCall::do_reopen()
{
    if (body) 
    {
        for (unsigned i = 0; i < args_num; i++) args[i]->reopen();
        body_fcr->reopen();
        reinit_consumer_table();
    }

    need_reopen = false;
}

void PPFunCall::do_close()
{
    unsigned i;

    for (i = 0; i < args_num; i++) ch_arr[i].op->close();

    if (body) 
    {
        if (is_body_opened)
        {
            is_body_opened = false;
            body->close();
        }

        delete body_fcr;
        body_fcr = NULL;
    }

    if (args)
    {
        for (i = 0; i < args_num; i++) 
        {
            delete (args[i]);
            args[i] = NULL;
        }

        delete [] args;
        args = NULL;
    }
}

void PPFunCall::do_next(xqp_tuple &t)
{
    try {
        is_in_arg_evaluation = false;
        do_next_impl(t);
        is_in_arg_evaluation = false;
    } catch (SednaXQueryException &ex) {
        if (!is_in_arg_evaluation) {
            const function_declaration& fd = fn_id.first->get_func_decl(fn_id.second);
            const xquery_stack_frame_info frame(info.query_line, info.query_col, fd.func_name);
            ex.add_frame(frame);
        }
        is_in_arg_evaluation = false;
        throw ex;
    }
}

void PPFunCall::do_next_impl(xqp_tuple &t)
{
    /* here we need to create new body by cloning the old one
     *
     * think recursive functions, for instance, where body is cloned
     * every time the same fun-call is met
     *
     * for example, fact(10) will create 10 cloned PPFunCalls with identical qep-bodies
     */
    if (body == NULL)
    {
        unsigned i;

        function_declaration &fd = fn_id.first->get_func_decl(fn_id.second);

        args = new fun_arg*[args_num];

        for (i = 0; i < args_num; i++)
            args[i] = new fun_arg(&(fd.args[i]), ch_arr[i].op, i + 1);

        /*
         * here we need to create new variable context for our execution
         * since all variable ids start from 0
         *
         * note, that there also be zero-length context in dyn_cxt
         */
        var_cxt = fn_id.first->get_current_var_context();
        var_cxt->setProducers(fd.vars_total);

        /* set producers for arguments */
        for (i = 0; i < args_num; i++)
        {
            var_cxt->producers[i].type = pt_lazy_complex;
            var_cxt->producers[i].op = this;
            var_cxt->producers[i].cvc = new complex_var_consumption;
            var_cxt->producers[i].tuple_pos = 0;
        }

        body = fd.op->copy(fn_id.first);

        /*
         * after copying the body, reset variable context -- this will create
         * new variable context for future copiers
         */
        fn_id.first->reset_local_vars();

        body->open();
        is_body_opened = true;

        /* arg_num == 0 means function return value */
        body_fcr = new fun_conv_rules(&(fd.ret_st), body, 0);
    }

    if (need_reopen)
    {
        for (unsigned i = 0; i < args_num; i++) args[i]->reopen();
        reinit_consumer_table();
        need_reopen = false;
    }

    body_fcr->next(t);

    if (t.is_eos()) need_reopen = true;
}

PPIterator* PPFunCall::do_copy(dynamic_context *_cxt_)
{
    PPFunCall *res = new PPFunCall(_cxt_, info, ch_arr, fn_id);

    for (unsigned i = 0; i < args_num; i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);

    return res;
}

var_c_id PPFunCall::do_register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(var_cxt->producers[dsc].cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPFunCall::do_next(xqp_tuple &t, var_dsc dsc, var_c_id id)
{
    is_in_arg_evaluation = true;
    args[dsc]->next(t, var_cxt->producers[dsc].cvc->at(id));
    is_in_arg_evaluation = false;
}

void PPFunCall::do_reopen(var_dsc dsc, var_c_id id)
{
    var_cxt->producers[dsc].cvc->at(id) = 0;
}

void PPFunCall::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPFunCall::reinit_consumer_table()
{
    for (unsigned i = 0; i < args_num; i++)
    {
        complex_var_consumption *cvc = var_cxt->producers[i].cvc;
        for (unsigned int j = 0; j < cvc->size(); j++) cvc->at(j) = 0;
    }
}

void PPFunCall::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (unsigned i = 0; i < args_num; i++)
        ch_arr[i].op->accept(v);
    v.pop();
}
