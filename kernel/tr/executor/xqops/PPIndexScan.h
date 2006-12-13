/*
 * File:  PPIndexScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINDEXSCAN_H
#define _PPINDEXSCAN_H

#include <string>

#include "sedna.h"

#include "PPBase.h"
#include "index_data.h"
#include "btree.h"
#include "XPathOnSchema.h"

enum index_scan_condition
{
    isc_eq,
    isc_lt,
    isc_le,
    isc_gt,
    isc_ge,
    isc_gt_lt,
    isc_gt_le,
    isc_ge_lt,
    isc_ge_le
};

class PPIndexScan : public PPIterator
{
protected:
    typedef void (PPIndexScan::*t_next_fun)(tuple &t);

    // given parameters
    std::string index_name;
    tuple_cell tc, tc2;
    PPOpIn child, child2;
    index_scan_condition isc;

    // obtained parameters and local data
    xptr btree;
    xmlscm_type idx_type;
    bt_cursor cursor;
    bt_key key, key2;
    xptr res;
    t_next_fun next_fun;
    bool first_time;

    void next_eq     (tuple &t);
    void next_lt_le  (tuple &t);
    void next_gt_ge  (tuple &t);
    void next_between(tuple &t);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) { (this->*next_fun)(t); }

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPIndexScan(dynamic_context *_cxt_,
                const std::string &_index_name_,
                const tuple_cell& _tc_,
                const tuple_cell& _tc2_,
                index_scan_condition _isc_);
    PPIndexScan(dynamic_context *_cxt_,
                const std::string &_index_name_,
                PPOpIn _child_,
                const tuple_cell& _tc2_,
                index_scan_condition _isc_);
    PPIndexScan(dynamic_context *_cxt_,
                const std::string &_index_name_,
                const tuple_cell& _tc_,
                PPOpIn _child2_,
                index_scan_condition _isc_);
    PPIndexScan(dynamic_context *_cxt_,
                const std::string &_index_name_,
                PPOpIn _child_,
                PPOpIn _child2_,
                index_scan_condition _isc_);
    virtual ~PPIndexScan();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif
