/*
 * File:  PPSeqChecker.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPSeqChecker.h"
#include "common/errdbg/exceptions.h"
#include <string>

using namespace std;

PPSeqChecker::PPSeqChecker(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_,
                 unsigned int _error_code_,
                 const char *_error_msg_) : PPIterator(_cxt_, _info_),
                                   child(_child_),
                                   error_code(_error_code_),
                                   error_msg(_error_msg_)
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

    if (error_code == XPTY0018)
        mode = CHECK_MIX;
    else
        mode = CHECK_NODE;

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
            std::string err = std::string(error_msg) + " filter step contains atomic in position " + int2string(pos);
            throw XQUERY_EXCEPTION2(error_code, err.c_str());
        }
    }
    else
    {
        if (expect_nodes && !tc.is_node())
        {
            std::string err = std::string(error_msg) + " last step contains atomic in position " + int2string(pos) +
                ", but has started with nodes";

            throw XQUERY_EXCEPTION2(error_code, err.c_str());
        }
        else if (!expect_nodes && tc.is_node())
        {
            std::string err = std::string(error_msg) + " last step contains node in position " + int2string(pos) +
                ", but has started with atomics";

            throw XQUERY_EXCEPTION2(error_code, err.c_str());
        }
    }
}

PPIterator* PPSeqChecker::do_copy(dynamic_context *_cxt_)
{
    PPSeqChecker *res = se_new PPSeqChecker(_cxt_, info, child, error_code, error_msg);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
