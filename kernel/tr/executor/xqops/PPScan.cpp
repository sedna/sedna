/*
 * File:  PPScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPScan.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"



PPScan::PPScan(dynamic_context *_cxt_, 
               schema_node_xptr _scm_node_,
               counted_ptr<db_entity> _db_ent_) : PPIterator(_cxt_),
                                                  db_ent(_db_ent_),
                                                  scm_node(_scm_node_)
{
}

PPScan::~PPScan()
{
}

void PPScan::open ()
{
    // Put lock on documents and check security
    schema_node_cptr root = get_schema_node(db_ent, "Unknown entity passed to PPScan");

    res = XNULL;
}


void PPScan::reopen()
{
    res = XNULL;
}


void PPScan::close ()
{
}

void PPScan::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (res == NULL)
    {
        res = getUnemptyBlockFore(scm_node->bblk);

        if (res == NULL)
        {
            t.set_eos();
            {RESTORE_CURRENT_PP; return;}
        }

        CHECKP(res);
        res = GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(res));

        if (res == NULL)
            t.set_eos();
        else
            t.copy(tuple_cell::node(res));

        {RESTORE_CURRENT_PP; return;}
    }

    res = getNextDescriptorOfSameSortXptr(res);
    if (res == NULL)
        t.set_eos();
    else
        t.copy(tuple_cell::node(res));

    RESTORE_CURRENT_PP;
}

PPIterator* PPScan::copy(dynamic_context *_cxt_)
{
    PPScan *res = se_new PPScan(_cxt_, scm_node, db_ent);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPScan::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPScan::result");
}

