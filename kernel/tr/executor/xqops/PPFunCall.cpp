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
#include "tr/executor/xqops/PPSLStub.h"


using namespace std;


void fun_conv_rules::next(tuple &t)
{
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
        default					: throw XQUERY_EXCEPTION2(SE1003, "Unexpected case in fcr::next");
    }

    if (t.is_eos()) 
    {
        num = 0;
        return;
    }

    tuple_cell tc = t.cells[0];
    if (st->type.type == st_atomic_type)
    {
        tc = atomize(tc);

        if(st->type.info.single_type != xs_anyAtomicType)
        {
            if (tc.get_atomic_type() == xs_untypedAtomic)
                tc = cast(tc, st->type.info.single_type, __xquery_line);
            else
                type_promotion(tc, st->type.info.single_type, __xquery_line);
        }

        t.copy(tc);
    }

    if (!type_matches_single(tc, st->type))
        throw XQUERY_EXCEPTION2(XPTY0004, (error() + ", given type is [" + tc.type2string() + "].").c_str());
}

string fun_conv_rules::error()
{
    string res;    

    if(arg_num != 0)
        res = "Argument [" + int2string(arg_num) + "] does not match the required type in function call. Expected type is [" +  st->to_str() +  "]";
    else
        res = "Return value does not match the required type in function call. Expected type is [" +  st->to_str() +  "]";

    return res;
}

#ifdef STRICT_FUNS
void fun_arg::init()
{
    tuple t(1);
    for (int i = 0; i < STRICT_FUNS_BOUND; i++)
    {
        fcr.next(t);
        if (t.is_eos())
        {
            seq_filled = true;
            break;
        }
        else s->add(t);
    }
}
#endif

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
                     const arr_of_PPOpIn &_ch_arr_,
                     function_id _fn_id_) : PPVarIterator(_cxt_),
                                            ch_arr(_ch_arr_),
                                            fn_id(_fn_id_),
                                            body(NULL),
                                            body_fcr(NULL),
                                            args(NULL),
                                            args_num(ch_arr.size()),
                                            new_cxt(NULL)
#ifdef STRICT_FUNS
                                            ,s(NULL)
                                            ,spos(-1)
#endif

{
    for (int i = 0; i < args_num; i++)
        if (ch_arr[i].ts != 1) throw USER_EXCEPTION2(SE1003, "Children of PPFunCall operation have wrong tuple size");
}

PPFunCall::~PPFunCall()
{
    for (int i = 0; i < args_num; i++) 
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }

    delete body;
    body = NULL;
}


void PPFunCall::open ()
{
    for (int i = 0; i < args_num; i++) 
        ch_arr[i].op->open();

    need_reopen = false;
}

void PPFunCall::reopen ()
{
    if (body) 
    {
        for (int i = 0; i < args_num; i++) args[i]->reopen();
        body_fcr->reopen();
        reinit_consumer_table();
    }

    need_reopen = false;

#ifdef STRICT_FUNS
    if (spos != -1)
    {
        spos = -1;
        delete s;
        s = NULL;
    }
#endif
}

void PPFunCall::close ()
{
    int i = 0;
    for (i = 0; i < args_num; i++) ch_arr[i].op->close();

    if (body) 
    {
        body->close();

        delete body_fcr;
        body_fcr = NULL;
    }

    if (new_cxt)
    {
        for (i = 0; i < args_num; i++) new_cxt->var_cxt.producers[i].s = NULL;
        delete new_cxt;
        new_cxt = NULL;
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

#ifdef STRICT_FUNS
    if (spos != -1)
    {
        spos = -1;
        delete s;
        s = NULL;
    }
#endif
}

void PPFunCall::next(tuple &t)
{
#ifdef STRICT_FUNS
    if (spos != -1)
    {
        if (spos < s->size()) s->get(t, spos++);
        else 
        {
            t.set_eos(); 
            spos = -1;
            delete s;
            s = NULL;
        }
        return;
    }
#endif

    if (body == NULL)
    {
        int i = 0;

#ifdef STRICT_FUNS
        if (args == NULL)
        {
#endif
            args = se_new fun_arg*[args_num];

            for (i = 0; i < args_num; i++)
                args[i] = se_new fun_arg(&(dynamic_context::funct_cxt.fun_decls[fn_id].args[i]),
                                         ch_arr[i].op, 
                                         i+1,
                                         __xquery_line);
#ifdef STRICT_FUNS
        }
        else args[i]->reopen();
#endif

#ifdef STRICT_FUNS
        bool strict_mode = true;
        for (i = 0; i < args_num; i++)
        {
            args[i]->init();
            strict_mode = strict_mode && args[i]->is_filled();
        }
#endif

        function_declaration &fd = dynamic_context::funct_cxt.fun_decls[fn_id];

#ifdef STRICT_FUNS
        if (new_cxt == NULL)
#endif
            new_cxt = se_new dynamic_context(fd.st_cxt, fd.cxt_size);

#ifdef STRICT_FUNS
        if (strict_mode)
        {
            for (i = 0; i < args_num; i++)
            {
                new_cxt->var_cxt.producers[i].type = pt_seq;
                new_cxt->var_cxt.producers[i].s = args[i]->get_sequence();
            }

            void *fun_r;
            bool fun_s = (fd.op->res_fun())(fd.op, new_cxt, fun_r);

            if (fun_s) 
            { 
                s = (sequence*)fun_r; 

                if (s->size() == 0)
                {
                    t.set_eos();
                    spos = -1;
                    delete s;
                    s = NULL;
                }
                else { s->get(t, 0); spos = 1; }

                return;
            }
            else
            {
                body = (PPIterator*)fun_r;
                // body->open(); !!! ???
            }
        }
        else
        {
#endif
            for (i = 0; i < args_num; i++)
            {
                new_cxt->var_cxt.producers[i].type = pt_lazy_complex;
                new_cxt->var_cxt.producers[i].op = this;
                new_cxt->var_cxt.producers[i].cvc = se_new complex_var_consumption;
                new_cxt->var_cxt.producers[i].tuple_pos = 0;
            }

            body = fd.op->copy(new_cxt);

            body->open();
#ifdef STRICT_FUNS
        }
#endif

        body_fcr = se_new fun_conv_rules(&(fd.ret_st), body, 0, __xquery_line);  /// arg_num == 0 means function return value
    }


    if (need_reopen)
    {
        for (int i = 0; i < args_num; i++) args[i]->reopen();
        reinit_consumer_table();
        need_reopen = false;
    }

    body_fcr->next(t);

    if (t.is_eos()) need_reopen = true;
}

PPIterator* PPFunCall::copy(dynamic_context *_cxt_)
{
    PPFunCall *res = se_new PPFunCall(_cxt_, ch_arr, fn_id);

    res->__xquery_line = __xquery_line;
    
    for (int i = 0; i < args_num; i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);

    return res;
}

var_c_id PPFunCall::register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(new_cxt->var_cxt.producers[dsc].cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPFunCall::next(tuple &t, var_dsc dsc, var_c_id id)
{
    args[dsc]->next(t, new_cxt->var_cxt.producers[dsc].cvc->at(id));
}

void PPFunCall::reopen(var_dsc dsc, var_c_id id)
{
    new_cxt->var_cxt.producers[dsc].cvc->at(id) = 0;
}

void PPFunCall::close(var_dsc dsc, var_c_id id)
{
}

inline void PPFunCall::reinit_consumer_table()
{
    for (int i = 0; i < args_num; i++)
    {
        complex_var_consumption *cvc = new_cxt->var_cxt.producers[i].cvc;
        for (int j = 0; j < cvc->size(); j++) cvc->at(j) = 0;
    }
}

bool PPFunCall::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    function_declaration &fd = tr_globals::qp.fun_decls[((PPFunCall*)cur)->fn_id];

    arr_of_PPOpIn ch_arr;
    ((PPFunCall*)cur)->children(ch_arr);

    vector<void*> ch_r(fd.num);
    vector<bool>  ch_s(fd.num);

    bool is_everything_strict = true;
    int i = 0;
    for (i = 0; i < fd.num; i++)
    {
        ch_s[i] = (ch_arr[i].op->res_fun())(ch_arr[i].op, cxt, ch_r[i]);
        is_everything_strict = is_everything_strict && ch_s[i];
    }

    if (!is_everything_strict)
    {
        for (i = 0; i < fd.num; i++)
        {
            if (ch_s[i])
            { // result is strict
                ch_arr[i].op = se_new PPSLStub(cxt, 
                                            ch_arr[i].op->copy(cxt), 
                                            (sequence*)(ch_r[i]));
            }
            else
            { // result is NON strict
                ch_arr[i].op = (PPIterator*)(ch_r[i]);
            }
        }

        r = se_new PPFunCall(cxt, ch_arr, ((PPFunCall*)cur)->fn_id);
        return false;
    }


    variable_context *new_cxt = se_new variable_context(fd.cxt_size);

    for (i = 0; i < fd.num; i++)
    {
        new_cxt->producers[i].type = pt_seq;
        new_cxt->producers[i].s = (sequence*)(ch_r[i]);
    }

    void *fun_r;
    bool fun_s = (fd.op->res_fun())(fd.op, new_cxt, fun_r);

    if (!fun_s)
    {
        for (i = 0; i < fd.num; i++)
        {
            ch_arr[i].op = se_new PPSLStub(cxt, ch_arr[i].op->copy(cxt), (sequence*)(ch_r[i]));
        }

        // !!! надо еще как-то передавать контекст и рюхать как он должен сохраняться

        r = se_new PPFunCall(cxt, ch_arr, ((PPFunCall*)cur)->fn_id);
        return false;
    }

    return strict_op_result(cur, (sequence*)fun_r, cxt, r);
*/
    return true;
}
