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

void PPDeleteDeep::do_open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
}

void PPDeleteDeep::do_close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

void PPDeleteDeep::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);    
    v.pop();
}

void PPDeleteDeep::do_execute()
{
    delete_deep(child);
}

