/*
 * File:  PPFnDocAvailable.cpp
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnDocAvailable.h"
#include "tr/structures/metadata.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFnDocAvailable::PPFnDocAvailable(dynamic_context *_cxt_,
                                   operation_info _info_, 
                                   PPOpIn _doc_name_op_) : PPIterator(_cxt_, _info_, "PPFnDocAvailable"),
                                                           doc_name_op(_doc_name_op_)
{
}

PPFnDocAvailable::PPFnDocAvailable(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _col_name_op_,
                                   PPOpIn _doc_name_op_) : PPIterator(_cxt_, _info_, "PPFnDocAvailable"),
                                                           col_name_op(_col_name_op_),
                                                           doc_name_op(_doc_name_op_)
{
}

PPFnDocAvailable::~PPFnDocAvailable()
{
    delete doc_name_op.op;
    doc_name_op.op = NULL;
    if (col_name_op.op) {
        delete col_name_op.op;
        col_name_op.op = NULL;
    }
}

void PPFnDocAvailable::do_open ()
{
    doc_name_op.op->open();
    if (col_name_op.op) {
        col_name_op.op->open();
    }
    first_time = true;
}


void PPFnDocAvailable::do_reopen()
{
    doc_name_op.op->reopen();
    if (col_name_op.op) {
        col_name_op.op->reopen();
    }
    first_time = true;
}


void PPFnDocAvailable::do_close()
{
    doc_name_op.op->close();
    if (col_name_op.op) {
        col_name_op.op->close();
    }
}

void PPFnDocAvailable::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        tuple_cell col_name;
        if (col_name_op.op) {
            col_name = get_name_from_PPOpIn(col_name_op, "collection", "doc-available", true, true);
        }
        
        tuple_cell doc_name = get_name_from_PPOpIn(doc_name_op, "document", "doc-available", true, true);
        
        bool res = false;

        if (!doc_name.is_eos() && !(col_name_op.op && col_name.is_eos())) {

            const char* doc_name_str = doc_name.get_str_mem();

            if(!col_name_op.op && get_document_type(doc_name_str, dbe_document) == DT_NON_SYSTEM) {
                res = (find_document(doc_name_str) != XNULL);
            } else if (col_name_op.op) {
                //access_collection checks auth (we still need permission to read it to check if doc is available),
                //finds collection and locks it; if collection not found it throws exception
                const char* col_name_str = col_name.get_str_mem();
                access_collection(col_name_str);
                res = (find_document_in_collection(col_name_str, doc_name_str) != XNULL);
            } else {
                //suppose any of the system documents is available
                res = true;
            }
        }

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
    PPFnDocAvailable *res = col_name_op.op ?
        new PPFnDocAvailable(_cxt_, info, col_name_op, doc_name_op) :
        new PPFnDocAvailable(_cxt_, info, doc_name_op);

    res->doc_name_op.op = doc_name_op.op->copy(_cxt_);
    if (col_name_op.op) {
        res->col_name_op.op = col_name_op.op->copy(_cxt_);
    }

    return res;
}

void PPFnDocAvailable::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    doc_name_op.op->accept(v);
    if (col_name_op.op) {
        col_name_op.op->accept(v);
    }
    v.pop();
}
