/*
 * File:  PPError.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPError.h"
#include "tr/crmutils/crmutils.h"

///////////////////////////////////////////////////////////////////////////////
/// fn:error
///////////////////////////////////////////////////////////////////////////////
PPFnError::PPFnError(dynamic_context *_cxt_, 
                     PPOpIn &_child_err_,
                     PPOpIn &_child_descr_,
                     PPOpIn &_child_obj_) : PPIterator(_cxt_),
                                            child_err(_child_err_),
                                            child_descr(_child_descr_),
                                            child_obj(_child_obj_)
{
}

PPFnError::~PPFnError()
{
    if (child_err.op)
    {
        delete child_err.op;
        child_err.op = NULL;
    }
    if (child_descr.op)
    {
        delete child_descr.op;
        child_descr.op = NULL;
    }
    if (child_obj.op)
    {
        delete child_obj.op;
        child_obj.op = NULL;
    }
}

void PPFnError::open  ()
{
    if (child_err.op) child_err.op->open();
    if (child_descr.op) child_descr.op->open();
    if (child_obj.op) child_obj.op->open();
}

void PPFnError::reopen()
{
    if (child_err.op) child_err.op->reopen();
    if (child_descr.op) child_descr.op->reopen();
    if (child_obj.op) child_obj.op->reopen();
}

void PPFnError::close ()
{
    if (child_err.op) child_err.op->close();
    if (child_descr.op) child_descr.op->close();
    if (child_obj.op) child_obj.op->close();
}

void PPFnError::next  (tuple &t)
{
    SET_CURRENT_PP(this);
    
    tuple_cell err_name_tc; // eos by default
    tuple_cell err_descr_tc; // eos by default

    if (child_err.op)
    {
        child_err.op->next(t);
        if (t.is_eos() && !(child_descr.op)) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

        if (!t.is_eos())
        {
            err_name_tc = child_err.get(t);
            if (!err_name_tc.is_atomic() || err_name_tc.get_atomic_type() != xs_QName)
                throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

            err_name_tc = tuple_cell::make_sure_light_atomic(err_name_tc);
            child_err.op->next(t);

            if (!t.is_eos())
                throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");
        }
    }

    if (child_descr.op)
    {
        child_descr.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

        err_descr_tc = child_descr.get(t);
        if (!err_descr_tc.is_atomic() || err_descr_tc.get_atomic_type() != xs_string)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

        err_descr_tc = tuple_cell::make_sure_light_atomic(err_descr_tc);
        child_descr.op->next(t);

        if (!t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");
    }


    const char *err_name = err_name_tc.is_eos() ? 
                           "FOER0000" : 
                           xs_QName_get_local_name(err_name_tc.get_str_mem());
    const char *err_descr = err_descr_tc.is_eos() ? NULL : err_descr_tc.get_str_mem();

    throw USER_EXCEPTION_FNERROR(err_name, err_descr);

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnError::copy(dynamic_context *_cxt_)
{
    PPFnError *res = se_new PPFnError(_cxt_, child_err, child_descr, child_obj);
    if (child_err.op)   res->child_err.op   = child_err.op->copy(_cxt_);
    if (child_descr.op) res->child_descr.op = child_descr.op->copy(_cxt_);
    if (child_obj.op)   res->child_obj.op   = child_obj.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnError::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnError::result");
}


///////////////////////////////////////////////////////////////////////////////
/// fn:trace
///////////////////////////////////////////////////////////////////////////////
PPFnTrace::PPFnTrace(dynamic_context *_cxt_,
                     PPOpIn _value_child_,
                     PPOpIn _label_child_) : PPIterator(_cxt_),
                                             value_child(_value_child_),
                                             label_child(_label_child_),
                                             dostr(dynamic_context::dostr()),
                                             first_time(true)
{
}

PPFnTrace::~PPFnTrace()
{
    delete value_child.op;
    value_child.op = NULL;
    delete label_child.op;
    label_child.op = NULL;
}

void PPFnTrace::open  ()
{
    value_child.op->open();
    label_child.op->open();
    first_time = true;
}

void PPFnTrace::reopen()
{
    value_child.op->reopen();
    label_child.op->reopen();
    first_time = true;
}

void PPFnTrace::close ()
{
    value_child.op->close();
    label_child.op->close();
}

void PPFnTrace::next(tuple &t)
{
    SET_CURRENT_PP(this);

    bool is_first = false;
    if (first_time)
    {
        first_time = false;
        is_first = true;

        label_child.op->next(t);    
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
    
        tc = label_child.get(t);
        if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
    
        label_child.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
            
        tc = tuple_cell::make_sure_light_atomic(tc);
    }

    value_child.op->next(t);
    if (t.is_eos()) 
        first_time = true;
    else 
    {
        dostr.set_debug_info_type(se_QueryTrace);
        dostr << "\nSEDNA TRACE " << tc.get_str_mem() << "\n";
        print_tuple_indent(t, dostr, xml, is_first, cxt);
        dostr.flush();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnTrace::copy(dynamic_context *_cxt_)
{
    PPFnTrace *res = se_new PPFnTrace(_cxt_, value_child, label_child);
    res->value_child.op = value_child.op->copy(_cxt_);
    res->label_child.op = label_child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnTrace::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnTrace::result");
}
