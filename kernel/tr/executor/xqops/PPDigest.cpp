/*
 * File: PPDigest.cpp
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPDigest.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/crypto/sha1.h"

PPDigest::PPDigest(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDigest"),
                                     child(_child_)
{
}

PPDigest::~PPDigest()
{
    delete child.op;
    child.op = NULL;
}

void PPDigest::do_open ()
{
    child.op->open();
    first_time = true;
}


void PPDigest::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDigest::do_close()
{
    child.op->close();
}

void PPDigest::do_next(tuple &t)
{
    if (first_time)
    {
        child.op->next(t);

        if (!t.is_eos())
        {
            first_time = false;
            tuple_cell in_str = atomize(child.get(t));

            if(!is_string_type(in_str.get_atomic_type())) {
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in {http://sedna.org/crypto}::sha1 (xs_string/derived/promotable is expected).");
            }

            child.op->next(t);
            if (!t.is_eos()) {
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument. Argument contains more than one item in {http://sedna.org/crypto}::sha1.");
            }

            Sha1 digest = Sha1();
            t.copy(digest.get(&in_str));
        }
    } else {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDigest::do_copy(dynamic_context *_cxt_)
{
    PPDigest *res = new PPDigest(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPDigest::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
