/*
 * File:  PPFnDocAvailable.cpp
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnDocAvailable.h"
#include "tr/structures/metadata.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFnDocAvailable::PPFnDocAvailable(dynamic_context *_cxt_,
                                   operation_info _info_, 
                                   PPOpIn _doc_name_op_) : PPIterator(_cxt_, _info_, "PPFnDocAvailable"),
                                                           doc_name_op(_doc_name_op_)
{
}

PPFnDocAvailable::~PPFnDocAvailable()
{
    delete doc_name_op.op;
    doc_name_op.op = NULL;
}

void PPFnDocAvailable::do_open ()
{
    doc_name_op.op->open();
    first_time = true;
}


void PPFnDocAvailable::do_reopen()
{
    doc_name_op.op->reopen();
    first_time = true;
}


void PPFnDocAvailable::do_close()
{
    doc_name_op.op->close();
}

void PPFnDocAvailable::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        doc_name_op.op->next(t);
        if (t.is_eos()) 
        {
            t.copy(tuple_cell::atomic(false));
            return;
        }

        tuple_cell tc_doc= atomize(doc_name_op.get(t));
        if(!is_string_type(tc_doc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:doc-available (xs_string/derived/promotable is expected).");
        doc_name_op.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument in fn:doc-available. Argument contains more than one item.");

        bool valid;
        Uri::check_constraints(&tc_doc, &valid, NULL);

        if(!valid) throw XQUERY_EXCEPTION2(FODC0005, "Invalid uri in fn:doc-available.");

        tc_doc = tuple_cell::make_sure_light_atomic(tc_doc);
        
        const char* name = (const char*)tc_doc.get_str_mem();
        
        bool res = true;

        /// suppose any system document is available ///
        if(get_document_type(name, dbe_document) == DT_NON_SYSTEM)
            res = (find_document(name) != XNULL);

        t.copy(tuple_cell::atomic(res));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnDocAvailable::do_copy(dynamic_context *_cxt_)
{
    PPFnDocAvailable *res = se_new PPFnDocAvailable(_cxt_, info, doc_name_op);
    res->doc_name_op.op = doc_name_op.op->copy(_cxt_);
    return res;
}

void PPFnDocAvailable::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    doc_name_op.op->accept(v);
    v.pop();
}
