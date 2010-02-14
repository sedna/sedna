/*
 * File:  PPReplace.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPReplace.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/updates/updates.h"
#include "tr/locks/locks.h"

PPReplace::PPReplace(PPOpIn _child_, 
                     dynamic_context *_cxt_) : PPUpdate(),
                                               child(_child_),
                                               cxt(_cxt_)
{
}

PPReplace::~PPReplace()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPReplace::do_open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
}

void PPReplace::do_close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

void PPReplace::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

void PPReplace::do_execute()
{
    replace(child);
}

