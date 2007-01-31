/*
 * File:  PPInsertTo.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPInsertTo.h"
#include "tr/updates/updates.h"
#include "tr/locks/locks.h"

PPInsertTo::PPInsertTo(PPOpIn _child1_, 
                       dynamic_context *_cxt1_,
                       PPOpIn _child2_,
                       dynamic_context *_cxt2_) : PPUpdate(),
                                                  child1(_child1_),
                                                  cxt1(_cxt1_),
                                                  child2(_child2_),
                                                  cxt2(_cxt2_)
{
}

PPInsertTo::~PPInsertTo()
{
    delete child1.op;
    child1.op = NULL;
    delete cxt1;
    cxt1 = NULL;
    delete child2.op;
    child2.op = NULL;
    delete cxt2;
    cxt2 = NULL;
}

void PPInsertTo::open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child1.op->open();
    child2.op->open();
}

void PPInsertTo::close()
{
    child1.op->close();
    child2.op->close();
    dynamic_context::global_variables_close();
}

void PPInsertTo::execute()
{
    insert_to(child1, child2);
}

