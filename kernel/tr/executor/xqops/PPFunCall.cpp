/*
 * File:  PPFunCall.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>
#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPFunCall.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/visitor/PPVisitor.h"


using namespace std;


void fun_conv_rules::next(tuple &t)
{
    INCREASE_STACK_DEPTH
    CHECK_STACK_DEPTH

    child->next(t);

    if (!t.is_eos()) num++;

    switch (st->oi)
    {
        case st_empty			: if (num != 0) throw XQUERY_EXCEPTION2(XPTY0004, (error() + ".").c_str());
                                  break;
        case st_one				: {
                                      if (num == 0) throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", empty sequence is given.").c_str());
                                      if (num > 1) throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", more than one item is given.").c_str());
                                      break;
                                  }
        case st_optional		: if (!(num == 0 || num == 1)) throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", more than one item is given.").c_str());
                                  break;
        case st_zero_or_more	: break;
        case st_one_or_more		: if (!(num >= 1)) throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", empty sequence is given.").c_str());
                                  break;
        default					: throw XQUERY_EXCEPTION2(SE1003, "Unexpected case in fcr::next.");
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
        throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", given type is [" + tc.type2string() + "].").c_str());

    DECREASE_STACK_DEPTH
}

string fun_conv_rules::error()
{
    string res;    

    if(arg_num != 0)
        res = "Error in function call. Argument [" + int2string(arg_num) + "] does not match the required type. Expected type is [" +  st->to_str() +  "]";
    else
        res = "Error in function call. Return value does not match the required type. Expected type is [" +  st->to_str() +  "]";

    return res;
}

void fun_arg::reopen()
{
    if (!seq_filled) fcr.reopen();
    seq_filled = false;
    s->clear();
}

void fun_arg::next(tuple /*out*/ &t, var_c_id /*out*/ &id)
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
    for (unsigned i = 0; i < args_num; i++)
        if (ch_arr[i].ts != 1) throw USER_EXCEPTION2(SE1003, "Children of PPFunCall operation have wrong tuple size");
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

void PPFunCall::do_next(tuple &t)
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
            args[i] = new fun_arg(&(fd.args[i]),
                                    ch_arr[i].op,
                                    i + 1);

        /*
         * here we need to create new variable context for our execution
         * since all variable ids start from 0
         */
        var_cxt = new variable_context();
        var_cxt->setProducers(fd.vars_total);

        // set producers for arguments
        for (i = 0; i < args_num; i++)
        {
            var_cxt->producers[i].type = pt_lazy_complex;
            var_cxt->producers[i].op = this;
            var_cxt->producers[i].cvc = se_new complex_var_consumption;
            var_cxt->producers[i].tuple_pos = 0;
        }

        // this sets proper copy-context
        fn_id.first->add_var_func_context(var_cxt);

        body = fd.op->copy(fn_id.first);
        body->open();
        is_body_opened = true;

        body_fcr = se_new fun_conv_rules(&(fd.ret_st), body, 0);  /// arg_num == 0 means function return value
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
    PPFunCall *res = se_new PPFunCall(_cxt_, info, ch_arr, fn_id);

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

void PPFunCall::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
    args[dsc]->next(t, var_cxt->producers[dsc].cvc->at(id));
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
