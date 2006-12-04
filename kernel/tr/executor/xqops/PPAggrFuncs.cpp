/*
 * File: PPAggrFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAggrFuncs.h"
#include "op_map.h"
#include "casting_operations.h"
#include "xs_helper.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnMaxMin
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

static bin_op_tuple_cell_tuple_cell_collation PPFnMaxMin_fun_arr[] = {op_gt, op_lt};
static const char* PPFnMaxMin_fun_name[] = {"fn:max()", "fn:min()"};

PPFnMaxMin::PPFnMaxMin(variable_context *_cxt_,
                       int _i_,
                       PPOpIn _child_) : PPIterator(_cxt_), 
                                         i(_i_),
                                         child(_child_)
{
}

PPFnMaxMin::PPFnMaxMin(variable_context *_cxt_,
                       int _i_,
                       PPOpIn _child_,
                       PPOpIn _collation_) : PPIterator(_cxt_), 
                                             i(_i_),
                                             child(_child_),
                                             collation(_collation_)
{
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

void PPFnMaxMin::open ()
{
    child.op->open();
    if (collation.op)
        collation.op->open();

    handler = NULL;
}

void PPFnMaxMin::reopen ()
{
    child.op->reopen();
    if (collation.op)
        collation.op->reopen();

    handler = NULL;
}         

void PPFnMaxMin::close ()
{
    child.op->close();
    if (collation.op)
        collation.op->close();

    handler = NULL;
}

void PPFnMaxMin::next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation.op)
        {
            collation.op->next(t);
            if(t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, (std::string("Invalid arity of the second argument. Argument contains zero items in ") + PPFnMaxMin_fun_name[i]).c_str());

            tuple_cell col = atomize(collation.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, (std::string("Invalid type of the second argument in ") + PPFnMaxMin_fun_name[i] + " (xs_string/derived/promotable is expected)").c_str());

            collation.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, (std::string("Invalid arity of the second argument in ") + PPFnMaxMin_fun_name[i] + ". Argument contains more than one item").c_str());
            
            col = tuple_cell::make_sure_light_atomic(col);
            handler = tr_globals::st_ct.get_collation(col.get_str_mem());
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

            if (is_numeric_type(type) || type == xs_boolean || type == xs_string || type == xs_date || type == xs_time || type == xs_dateTime || type == xs_yearMonthDuration || type == xs_dayTimeDuration)
                ;
            else
                throw USER_EXCEPTION2(FORG0006, (std::string("Error in evaluation of ") + PPFnMaxMin_fun_name[i]).c_str());

            if (res.is_eos()) 
            {
                res = tca;
                least_common_type = tca.get_atomic_type();
                continue;
            }

            try {
                bool cond = (PPFnMaxMin_fun_arr[i])(tca, res, handler).get_xs_boolean();
                if (cond) 
                    res = tca;
            } catch (SednaUserException &e) {
                throw USER_EXCEPTION2(FORG0006, (std::string("Error in evaluation of ") + PPFnMaxMin_fun_name[i]).c_str());
            }

            if (!(least_common_type == xs_double || least_common_type == xs_string))
                least_common_type = evaluate_common_type(tca.get_atomic_type(), least_common_type);
        }

        if (res.is_eos())
            t.set_eos();
        else
            t.copy(cast((has_NaN ? tuple_cell::atomic(float_NaN) : res), least_common_type));
    }
    else
    {
        t.set_eos();
        handler = NULL;
    }
}

PPIterator* PPFnMaxMin::copy(variable_context *_cxt_)
{
    PPFnMaxMin *res = new PPFnMaxMin(_cxt_, i, child, collation);
    res->child.op = child.op->copy(_cxt_);
    if (collation.op)
        res->collation.op = collation.op->copy(_cxt_);

    return res;
}

bool PPFnMaxMin::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnMaxMin::result");
}
