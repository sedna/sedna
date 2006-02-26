/*
 * File:  PPInsertFollowing.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPInsertFollowing.h"
#include "updates.h"
#include "locks.h"

PPInsertFollowing::PPInsertFollowing(PPOpIn _child1_, 
                                     variable_context *_cxt1_,
                                     PPOpIn _child2_,
                                     variable_context *_cxt2_) : PPUpdate(),
                                                                 child1(_child1_),
                                                                 cxt1(_cxt1_),
                                                                 child2(_child2_),
                                                                 cxt2(_cxt2_)
{
}

PPInsertFollowing::~PPInsertFollowing()
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

void PPInsertFollowing::open()
{
    local_lock_mrg->lock(lm_x);
    child1.op->open();
    child2.op->open();
}

void PPInsertFollowing::close()
{
    child1.op->close();
    child2.op->close();
}

void PPInsertFollowing::execute()
{
    insert_following(child1, child2);
}

