/*
 * File:  PPDeleteUndeep.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDeleteUndeep.h"
#include "tr/executor/base/PPVisitor.h"
#include "tr/updates/updates.h"
#include "tr/locks/locks.h"

PPDeleteUndeep::PPDeleteUndeep(PPOpIn _child_, 
                               dynamic_context *_cxt_) : PPUpdate(),
                                                         child(_child_),
                                                         cxt(_cxt_)
{
}

PPDeleteUndeep::~PPDeleteUndeep()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPDeleteUndeep::open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
}

void PPDeleteUndeep::close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

void PPDeleteUndeep::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);    
    v.pop();
}

void PPDeleteUndeep::execute()
{
    delete_undeep(child);
}

