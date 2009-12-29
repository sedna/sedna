/*
 * File:  PPScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPScan.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPVisitor.h"


PPScan::PPScan(dynamic_context *_cxt_,
               operation_info _info_,
               schema_node_xptr _scm_node_,
               counted_ptr<db_entity> _db_ent_) : PPIterator(_cxt_, _info_),
                                                  db_ent(_db_ent_),
                                                  scm_node(_scm_node_)
{
}

PPScan::~PPScan()
{
}

void PPScan::do_open ()
{
    // Put lock on documents and check security
    schema_node_cptr root = get_schema_node(db_ent, "Unknown entity passed to PPScan");

    res = XNULL;
}


void PPScan::do_reopen()
{
    res = XNULL;
}


void PPScan::do_close()
{
}

void PPScan::do_next(tuple &t)
{
    if (res == XNULL)
    {
        res = getNonemptyBlockLookBack(scm_node->bblk);

        if (res == XNULL)
        {
            t.set_eos();
            return;
        }

        CHECKP(res);
        res = GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(res));

        if (res == XNULL)
            t.set_eos();
        else
            t.copy(tuple_cell::node(res));
        return;
    }

    res = getNextDescriptorOfSameSortXptr(res);
    if (res == XNULL)
        t.set_eos();
    else
        t.copy(tuple_cell::node(res));
}

PPIterator* PPScan::do_copy(dynamic_context *_cxt_)
{
    PPScan *res = se_new PPScan(_cxt_, info, scm_node, db_ent);
    return res;
}

void PPScan::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    v.pop();
}
