/*
 * File:  PPRename.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPRename.h"
#include "updates.h"
#include "locks.h"

PPRename::PPRename(PPOpIn _child_, 
                   variable_context *_cxt_,
                   QName _name_) : PPUpdate(),
                                   child(_child_),
                                   cxt(_cxt_),
                                   name(_name_)
{
}

PPRename::~PPRename()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPRename::open()
{
    local_lock_mrg->lock(lm_x);
    child.op->open();
}

void PPRename::close()
{
    child.op->close();
}

void PPRename::execute()
{
    rename(child, name.LocalPart.n);
}

