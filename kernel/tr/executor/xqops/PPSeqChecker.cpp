/*
 * File:  PPSeqChecker.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPSeqChecker.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "common/errdbg/exceptions.h"

using namespace std;

PPSeqChecker::PPSeqChecker(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_,
                 PPSeqChecker::CheckMode _mode_) : PPIterator(_cxt_, _info_, "PPSeqChecker"),
                                                   child(_child_),
                                                   mode(_mode_)
{
}

PPSeqChecker::~PPSeqChecker()
{
    delete (child.op);
    child.op = NULL;
}

void PPSeqChecker::do_open ()
{
    child.op->open();
    pos = 0;
}

void PPSeqChecker::do_reopen()
{
    child.op->reopen();
    pos = 0;
}

void PPSeqChecker::do_close()
{
    child.op->close();
}

void PPSeqChecker::do_next (tuple &t)
{
    child.op->next(t);
    tuple_cell tc = child.get(t);

    if (t.is_eos())
    {
        pos = 0;
        return;
    }

    // first time
    if (!pos && mode == CHECK_MIX)
    {
        expect_nodes = tc.is_node();
        pos++;
        return;
    }

    pos++;

    if (mode == CHECK_NODE)
    {
        if (!tc.is_node())
        {
            std::string err = "at (" + cast_to_string<int>(this->info.query_line) + ":" +  cast_to_string<int>(this->info.query_col) +
                "), filter step contains atomic in position " + int2string(pos);
            throw XQUERY_EXCEPTION2(XPTY0019, err.c_str());
        }
    }
    else
    {
        if (expect_nodes && !tc.is_node())
        {
            std::string err = "at (" + cast_to_string<int>(this->info.query_line) + ":" +  cast_to_string<int>(this->info.query_col) + "), "
                    "last step contains atomic in position " + cast_to_string<unsigned int>(pos) + ", but has started with nodes";

            throw XQUERY_EXCEPTION2(XPTY0018, err.c_str());
        }
        else if (!expect_nodes && tc.is_node())
        {
            std::string err = "at (" + cast_to_string<int>(this->info.query_line) + ":" +  cast_to_string<int>(this->info.query_col)+
                "), last step contains node in position " + cast_to_string<unsigned int>(pos) + ", but has started with atomics";

            throw XQUERY_EXCEPTION2(XPTY0018, err.c_str());
        }
    }
}

PPIterator* PPSeqChecker::do_copy(dynamic_context *_cxt_)
{
    PPSeqChecker *res = new PPSeqChecker(_cxt_, info, child, mode);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPSeqChecker::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
