/*
 * File:  PPReplace.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPReplace.h"
#include "updates.h"
#include "locks.h"

PPReplace::PPReplace(PPOpIn _child_, 
                               variable_context *_cxt_) : PPUpdate(),
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

void PPReplace::open()
{
    local_lock_mrg->lock(lm_x);
    child.op->open();
}

void PPReplace::close()
{
    child.op->close();
}

void PPReplace::execute()
{
    replace(child);
}

