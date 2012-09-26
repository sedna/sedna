/*
 * File:  PPCheckpoint.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPCheckpoint.h"
#include "tr/log/log.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "common/llcommon/llMain.h"

PPCheckpoint::PPCheckpoint(dynamic_context *_cxt_,
                           operation_info _info_) : PPIterator(_cxt_, _info_, "PPCheckpoint")
{
}

PPCheckpoint::~PPCheckpoint()
{
    // nothing to do
}

void PPCheckpoint::do_open ()
{
    // nothing to do
}

void PPCheckpoint::do_reopen()
{
    // nothing to do
}

void PPCheckpoint::do_close()
{
    // nothing to do
}

void PPCheckpoint::do_next (tuple &t)
{
     t.set_eos();

     /* Activate checkpoint without waiting for it.
      * Due to our architecture it'll probably start after this transaction ended.
      */
     llActivateCheckpoint();
}

void PPCheckpoint::do_accept(PPVisitor &v)
{
    v.visit (this);
}


PPIterator* PPCheckpoint::do_copy(dynamic_context *_cxt_)
{
    PPCheckpoint *res = new PPCheckpoint(_cxt_, info);
    return res;
}
