/*
 * File:  PPCheckpoint.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPCheckpoint.h"
#include "tr/log/log.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/PPVisitor.h"

PPCheckpoint::PPCheckpoint(dynamic_context *_cxt_,
                           operation_info _info_) : PPIterator(_cxt_, _info_)
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
    //activate_and_wait_for_end_checkpoint();
    // change of semantics -- se:checkpoint() makes checkpoint on transaction commit now
    tr_globals::is_need_checkpoint_on_transaction_commit = true;
    // call checkpoint here
}

void PPCheckpoint::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    v.pop();
}


PPIterator* PPCheckpoint::do_copy(dynamic_context *_cxt_)
{
    PPCheckpoint *res = se_new PPCheckpoint(_cxt_, info);
    return res;
}
