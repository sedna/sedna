/*
 * File:  PPDeleteDeep.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDeleteDeep.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/updates/updates.h"
#include "tr/locks/locks.h"

PPDeleteDeep::PPDeleteDeep(PPOpIn _child_, 
                           dynamic_context *_cxt_) : PPUpdate(),
                                                     child(_child_),
                                                     cxt(_cxt_)
{
}

PPDeleteDeep::~PPDeleteDeep()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPDeleteDeep::open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
}

void PPDeleteDeep::close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

void PPDeleteDeep::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);    
    v.pop();
}

void PPDeleteDeep::execute()
{
    delete_deep(child);
}

