/*
 * File: PPAggrFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAggrFuncs.h"
#include "tr/executor/fo/op_map.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/PPUtils.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnMaxMin
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

static bin_op_tuple_cell_tuple_cell_collation PPFnMaxMin_fun_arr[] = {op_gt, op_lt};
static const char* PPFnMaxMin_fun_name[] = {"fn:max()", "fn:min()"};

using namespace std;

PPFnMaxMin::PPFnMaxMin(dynamic_context *_cxt_,
                       operation_info _info_,
                       int _i_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnMaxMin"),
                                         child(_child_),
                                         i(_i_)
{
    U_ASSERT(i == 0 || i == 1);
    function_name = PPFnMaxMin_fun_name[i];
}

PPFnMaxMin::PPFnMaxMin(dynamic_context *_cxt_,
                       operation_info _info_,
                       int _i_,
                       PPOpIn _child_,
                       PPOpIn _collation_) : PPIterator(_cxt_, _info_, "PPFnMaxMin"),
                                             child(_child_),
                                             collation(_collation_),
                                             i(_i_)
{
    U_ASSERT(i == 0 || i == 1);
    function_name = PPFnMaxMin_fun_name[i];
}

PPFnMaxMin::~PPFnMaxMin()
{
    delete child.op;
    child.op = NULL;
    if (collation.op)
    {
        delete collation.op;
        collation.op = NULL;
    }
}

void PPFnMaxMin::do_open ()
{
    child.op->open();
    if (collation.op)
        collation.op->open();

    handler = NULL;
}

void PPFnMaxMin::do_reopen()
{
    child.op->reopen();
    if (collation.op)
        collation.op->reopen();

    handler = NULL;
}         

void PPFnMaxMin::do_close()
{
    child.op->close();
    if (collation.op)
        collation.op->close();

    handler = NULL;
}

void PPFnMaxMin::do_next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation.op)
        {
            collation.op->next(t);
            if(t.is_eos()) 
                throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid arity of the second argument. Argument contains zero items in ") + function_name).c_str());

            tuple_cell col = atomize(collation.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type of the second argument in ") + function_name + " (xs_string/derived/promotable is expected)").c_str());

            collation.op->next(t);
            if (!t.is_eos()) 
                throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid arity of the second argument in ") + function_name + ". Argument contains more than one item").c_str());
            
            col = tuple_cell::make_sure_light_atomic(col);

            int res = cxt->get_static_context()->get_collation(col.get_str_mem(), &handler);
            if(res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + " in" + function_name + ".").c_str()); 
        }

        tuple_cell res;
        xmlscm_type least_common_type = xs_untypedAtomic;
        bool has_NaN = false;

        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) break;

            tuple_cell tca = atomize(child.get(t));
            xmlscm_type type = tca.get_atomic_type();

            if (type == xs_untypedAtomic)
            {
                tca = cast(tca, xs_double);
                type = xs_double;
            }
            else if (type == xs_anyURI)
            {
                tca = cast(tca, xs_string);
                type = xs_string;
            }

            if ((type == xs_float  && u_is_nan((double)(tca.get_xs_float()))) || 
                (type == xs_double && u_is_nan(tca.get_xs_double())))
                    has_NaN = true;

            if (!(is_numeric_type(type) || type == xs_boolean || type == xs_string || type == xs_date || type == xs_time || type == xs_dateTime || type == xs_yearMonthDuration || type == xs_dayTimeDuration))
                throw XQUERY_EXCEPTION2(FORG0006, (string("Bad argument type in ") + function_name).c_str());

            if (res.is_eos()) {
                res = tca;
                least_common_type = tca.get_atomic_type();
                continue;
            }

            try {
                bool cond = (PPFnMaxMin_fun_arr[i])(tca, res, handler).get_xs_boolean();
                if (cond) 
                    res = tca;
            } catch (SednaUserException) {
                throw XQUERY_EXCEPTION2(FORG0006, (string("Can't compare values in ") + function_name).c_str());
            }

            if (!(least_common_type == xs_double || least_common_type == xs_string))
                least_common_type = evaluate_common_type(tca.get_atomic_type(), least_common_type);
        }

        if (!res.is_eos()) {
            t.copy(cast((has_NaN ? tuple_cell::atomic(float_NaN) : res), least_common_type));
            return;
        }
    }
    //Not first time or 'res' was eos even after full walk through input sequence
    t.set_eos();
    handler = NULL;
}

PPIterator* PPFnMaxMin::do_copy(dynamic_context *_cxt_)
{
    PPFnMaxMin *res = se_new PPFnMaxMin(_cxt_, info, i, child, collation);
    res->child.op = child.op->copy(_cxt_);
    if (collation.op)
        res->collation.op = collation.op->copy(_cxt_);
    return res;
}

void PPFnMaxMin::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    if(collation.op) collation.op->accept(v);
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSumAvg
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

static const char* PPFnSumAvg_fun_name[] = {"fn:sum()", "fn:avg()"};

PPFnSumAvg::PPFnSumAvg(dynamic_context *_cxt_,
                       operation_info _info_,
                       int _i_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnSumAvg"),
                                         child(_child_),
                                         i(_i_)
{
    U_ASSERT(i == 0 || i == 1);
    function_name = PPFnSumAvg_fun_name[i];
}

PPFnSumAvg::PPFnSumAvg(dynamic_context *_cxt_,
                       operation_info _info_,
                       int _i_,
                       PPOpIn _child_,
                       PPOpIn _zero_) : PPIterator(_cxt_, _info_, "PPFnSumAvg"),
                                        child(_child_),
                                        zero(_zero_),
                                        i(_i_)
{
    U_ASSERT(i == 0 || i == 1);
    function_name = PPFnSumAvg_fun_name[i];
}

PPFnSumAvg::~PPFnSumAvg()
{
    delete child.op;
    child.op = NULL;
    if (zero.op)
    {
        delete zero.op;
        zero.op = NULL;
    }
}

void PPFnSumAvg::do_open ()
{
    child.op->open();
    if (zero.op)
        zero.op->open();

    first_time = true;
}

void PPFnSumAvg::do_reopen()
{
    child.op->reopen();
    if (zero.op)
        zero.op->reopen();

    first_time = true;
}         

void PPFnSumAvg::do_close()
{
    child.op->close();
    if (zero.op)
        zero.op->close();

    first_time = true;
}

void PPFnSumAvg::do_next(tuple &t)
{
    
    if (first_time)
    {
        first_time = false;
        tuple_cell res;
        int64_t n = 0;

        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) 
                break;

            ++n;

            tuple_cell tca = atomize(child.get(t));
            xmlscm_type type = tca.get_atomic_type();

            if (type == xs_untypedAtomic)
            {
                tca = cast(tca, xs_double);
                type = xs_double;
            }

            if (!is_numeric_type(type) && type != xs_yearMonthDuration && type != xs_dayTimeDuration)
                throw XQUERY_EXCEPTION2(FORG0006, (string("Numeric or duration type is expected in: ") + function_name + string("Given type is: ") + tca.type2string()).c_str());

            if (res.is_eos())
                res = tca;
            else
            {
                try {
                    res = op_add(tca, res);
                } catch (SednaUserException) {
                    throw XQUERY_EXCEPTION2(FORG0006, (string("Cannot sum values in ") + function_name).c_str());
                }
            }
        }

        if (res.is_eos())
        {
            if (zero.op)
            {
                zero.op->next(t);
                if (!t.is_eos()) 
                {
                    tuple_cell z = atomize(zero.get(t));
                    zero.op->next(t);
                    if (!t.is_eos()) 
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid arity of the second argument in ") + function_name + ". Argument contains more than one item").c_str());

                    t.copy(z);
                } else first_time = true;
            }
            else 
            {
                if (i) t.set_eos();
                else t.copy(tuple_cell::atomic((int64_t)0));
            }
        }
        else
        {
            if (i) res = op_div(res, tuple_cell::atomic(n));
            t.copy(res);
        }
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnSumAvg::do_copy(dynamic_context *_cxt_)
{
    PPFnSumAvg *res = se_new PPFnSumAvg(_cxt_, info, i, child, zero);
    res->child.op = child.op->copy(_cxt_);
    if (zero.op)
        res->zero.op = zero.op->copy(_cxt_);
    return res;
}

void PPFnSumAvg::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    if(zero.op) zero.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnCount
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnCount::PPFnCount(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnCount"), 
                                       child(_child_)
{
}


PPFnCount::~PPFnCount()
{
    delete child.op;
    child.op = NULL;
}

void PPFnCount::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnCount::do_reopen()
{
    child.op->reopen();
    first_time = true;
}         

void PPFnCount::do_close()
{
    child.op->close();
    first_time = true;
}

void PPFnCount::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        int64_t n = 0;
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) 
                break;
            n++;
        }
        t.copy(tuple_cell::atomic(n));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnCount::do_copy(dynamic_context *_cxt_)
{
    PPFnCount *res = se_new PPFnCount(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnCount::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
