/*
 * File:  PPDeleteDeep.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPDeleteDeep.h"
#include "updates.h"
#include "locks.h"

PPDeleteDeep::PPDeleteDeep(PPOpIn _child_, 
                           variable_context *_cxt_) : PPUpdate(),
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
    child.op->open();
}

void PPDeleteDeep::close()
{
    child.op->close();
}

void PPDeleteDeep::execute()
{
    delete_deep(child);
}

