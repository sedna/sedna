/*
 * File: PPFnColAvailable.cpp
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnColAvailable.h"
#include "tr/structures/metadata.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFnColAvailable::PPFnColAvailable(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _col_name_op_) : PPIterator(_cxt_, _info_, "PPFnColAvailable"),
                                                           col_name_op(_col_name_op_)
{
}

PPFnColAvailable::~PPFnColAvailable()
{
    delete col_name_op.op;
    col_name_op.op = NULL;
}

void PPFnColAvailable::do_open ()
{
    col_name_op.op->open();
    first_time = true;
}


void PPFnColAvailable::do_reopen()
{
    col_name_op.op->reopen();
    first_time = true;
}


void PPFnColAvailable::do_close()
{
    col_name_op.op->close();
}

void PPFnColAvailable::do_next(xqp_tuple &t)
{
    if (first_time)
    {
        first_time = false;

        tuple_cell col_name = get_name_from_PPOpIn(col_name_op, "collection", "col-available", true, true);

        bool res = false;

        if (!col_name.is_eos()) {

            const char* col_name_str = col_name.get_str_mem();

            if(get_document_type(col_name_str, dbe_collection) == DT_NON_SYSTEM) {
                res = (find_collection(col_name_str) != XNULL);
            } else {
                //suppose any of the system collections is available
                res = true;
            }
        }

        t.copy(tuple_cell::atomic(res));
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnColAvailable::do_copy(dynamic_context *_cxt_)
{
    PPFnColAvailable *res = new PPFnColAvailable(_cxt_, info, col_name_op);
    res->col_name_op.op = col_name_op.op->copy(_cxt_);
    return res;
}

void PPFnColAvailable::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    col_name_op.op->accept(v);
    v.pop();
}
