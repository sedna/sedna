/*
 * File:  PPFnDocAvailable.cpp
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnDocAvailable.h"
#include "tr/structures/metadata.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/PPUtils.h"

PPFnDocAvailable::PPFnDocAvailable(dynamic_context *_cxt_, 
                                   PPOpIn _doc_name_op_) : PPIterator(_cxt_),
                                                           doc_name_op(_doc_name_op_)
{
}

PPFnDocAvailable::~PPFnDocAvailable()
{
    delete doc_name_op.op;
    doc_name_op.op = NULL;
}

void PPFnDocAvailable::open ()
{
    doc_name_op.op->open();
    first_time = true;
}


void PPFnDocAvailable::reopen()
{
    doc_name_op.op->reopen();
    first_time = true;
}


void PPFnDocAvailable::close ()
{
    doc_name_op.op->close();
}

void PPFnDocAvailable::next(tuple &t)
{
    SET_XQUERY_LINE(__xquery_line);

    if (first_time)
    {
        doc_name_op.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}    //if $uri is the empty sequence, the result is an empty sequence.

        tuple_cell tc_doc= atomize(doc_name_op.get(t));
        if(!is_string_type(tc_doc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:doc-available (xs_string/derived/promotable is expected).");
        doc_name_op.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument in fn:doc-available. Argument contains more than one item.");

        first_time = false;

        bool valid;
        Uri::check_constraints(&tc_doc, &valid, NULL);

        if(!valid) throw XQUERY_EXCEPTION2(FODC0005, "Invalid uri in fn:doc-available.");

        tc_doc = tuple_cell::make_sure_light_atomic(tc_doc);
        
        bool res = (find_document((const char*)tc_doc.get_str_mem()) != NULL);

        t.copy(tuple_cell::atomic(res));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }

    UNDO_XQUERY_LINE;
}

PPIterator* PPFnDocAvailable::copy(dynamic_context *_cxt_)
{
    PPFnDocAvailable *res = se_new PPFnDocAvailable(_cxt_, doc_name_op);
    res->doc_name_op.op = doc_name_op.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnDocAvailable::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDocAvailable::result");
}



