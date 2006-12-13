/*
 * File:  PPScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPSCAN_H
#define __PPSCAN_H

#include "sedna.h"
#include "PPBase.h"

class PPScan : public PPIterator
{
protected:
    // given parameters
    counted_ptr<db_entity> db_ent;
    schema_node *scm_node;

    xptr res;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPScan(dynamic_context *_cxt_, 
           schema_node *_scm_node_,
           counted_ptr<db_entity> _db_ent_);
    virtual ~PPScan();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif
