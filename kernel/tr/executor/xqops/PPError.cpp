/*
 * File:  PPError.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPError.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/crmutils/serialization.h"
#include "tr/executor/base/PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
/// fn:error
///////////////////////////////////////////////////////////////////////////////
PPFnError::PPFnError(dynamic_context *_cxt_,
                     operation_info _info_, 
                     PPOpIn &_child_err_,
                     PPOpIn &_child_descr_,
                     PPOpIn &_child_obj_) : PPIterator(_cxt_, _info_, "PPFnError"),
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

void PPFnError::do_open ()
{
    if (child_err.op) child_err.op->open();
    if (child_descr.op) child_descr.op->open();
    if (child_obj.op) child_obj.op->open();
}

void PPFnError::do_reopen()
{
    if (child_err.op) child_err.op->reopen();
    if (child_descr.op) child_descr.op->reopen();
    if (child_obj.op) child_obj.op->reopen();
}

void PPFnError::do_close()
{
    if (child_err.op) child_err.op->close();
    if (child_descr.op) child_descr.op->close();
    if (child_obj.op) child_obj.op->close();
}



void PPFnError::do_next (tuple &t)
{
        
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


    const char *err_name = err_name_tc.is_eos() ? "FOER0000" : err_name_tc.get_xs_qname().getLocalName();
    const char *err_descr = err_descr_tc.is_eos() ? NULL : err_descr_tc.get_str_mem();

    throw USER_EXCEPTION_FNERROR(err_name, err_descr);
}

PPIterator* PPFnError::do_copy(dynamic_context *_cxt_)
{
    PPFnError *res = new PPFnError(_cxt_, info, child_err, child_descr, child_obj);
    if (child_err.op)   res->child_err.op   = child_err.op->copy(_cxt_);
    if (child_descr.op) res->child_descr.op = child_descr.op->copy(_cxt_);
    if (child_obj.op)   res->child_obj.op   = child_obj.op->copy(_cxt_);
    return res;
}

void PPFnError::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (child_err.op) child_err.op->accept(v);
    if (child_descr.op) child_descr.op->accept(v);
    if (child_obj.op) child_obj.op->accept(v);
    v.pop();
}



///////////////////////////////////////////////////////////////////////////////
/// fn:trace
///////////////////////////////////////////////////////////////////////////////
PPFnTrace::PPFnTrace(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _value_child_,
                     PPOpIn _label_child_) : PPIterator(_cxt_, _info_, "PPFnTrace"),
                                             value_child(_value_child_),
                                             label_child(_label_child_),
                                             first_time(true),
                                             dostr(tr_globals::client->get_debug_ostream())
{
}

PPFnTrace::~PPFnTrace()
{
    delete value_child.op;
    value_child.op = NULL;
    delete label_child.op;
    label_child.op = NULL;
}

void PPFnTrace::do_open ()
{
    value_child.op->open();
    label_child.op->open();
    first_time = true;
}

void PPFnTrace::do_reopen()
{
    value_child.op->reopen();
    label_child.op->reopen();
    first_time = true;
}

void PPFnTrace::do_close()
{
    value_child.op->close();
    label_child.op->close();
}

void PPFnTrace::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        label_child.op->next(t);    
        if (t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace. Label argument cannot be empty sequence.");
    
        tc = atomize(label_child.get(t));
        if (!is_string_type(tc.get_atomic_type()))
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace. Invalid label type (xs_string/derived/promotable is expected).");
    
        label_child.op->next(t);
        if (!t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace. Argument contains more than one item.");
            
        tc = tuple_cell::make_sure_light_atomic(tc);
        
        if (tc.get_strlen_mem() > 500) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Too long trace prefix is given in fn:trace function");

        tr_globals::create_serializer(tr_globals::client->get_result_type());
    }

    value_child.op->next(t);
    
    if (t.is_eos())
    {
        first_time = true;
    }
    else 
    {
        dostr->set_debug_info_type(se_QueryTrace);
        (*dostr) << tc.get_str_mem() << " ";

        tr_globals::serializer->prepare(
            dostr, cxt->get_static_context()->get_serialization_options()
          );

        tr_globals::serializer->serialize(t);
        dostr->flush();
    }
}

PPIterator* PPFnTrace::do_copy(dynamic_context *_cxt_)
{
    PPFnTrace *res = new PPFnTrace(_cxt_, info, value_child, label_child);
    res->value_child.op = value_child.op->copy(_cxt_);
    res->label_child.op = label_child.op->copy(_cxt_);
    return res;
}

void PPFnTrace::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    value_child.op->accept(v);
    label_child.op->accept(v);
    v.pop();
}
