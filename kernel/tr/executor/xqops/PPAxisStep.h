/*
 * File:  PPAxisStep.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISSTEP_H
#define _PPAXISSTEP_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"

struct AxisHints;

typedef Node (*EvaluateAxisProc)(Node node, AxisHints * hint);
typedef Node (*NextNodeProc)(Node node, AxisHints * hint);
typedef bool (*TestNodeProc)(Node node, AxisHints * hint);

class PPAxisStep : public PPIterator {
protected:
    PPOpIn child;
    xpath::NodeTest nt;
    xptr currentNodeIndir;
    AxisHints * hint;

    EvaluateAxisProc evaluateAxisProc;
    NextNodeProc nextNodeProc;
    TestNodeProc testNodeProc;
private:
    virtual void do_open();
    virtual void do_reopen();
    virtual void do_close();
    virtual void do_next(tuple &t);
    virtual void do_accept(PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
public:
    PPAxisStep(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _child_,
                xpath::NodeTest _nt_);

    virtual ~PPAxisStep();

    inline const xpath::NodeTest& getNodeTest() { return nt; }
};

#endif
