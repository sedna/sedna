/*
 * File:  PPScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPSCAN_H
#define __PPSCAN_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/structures/system_tables.h"

class PPScan : public PPIterator
{
protected:
    counted_ptr<db_entity> db_ent;
    schema_node_xptr scm_node;

    xptr res;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPScan(dynamic_context *_cxt_, 
           operation_info _info_,
           schema_node_xptr _scm_node_,
           counted_ptr<db_entity> _db_ent_);
    virtual ~PPScan();
};


#endif
