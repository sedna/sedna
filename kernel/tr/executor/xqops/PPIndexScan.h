/*
 * File:  PPIndexScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINDEXSCAN_H
#define _PPINDEXSCAN_H

#include <string>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/idx/indeximpl.h"
#include "tr/executor/base/XPathOnSchema.h"

enum index_scan_condition
{
    isc_eq = 0,
    isc_lt = 1,
    isc_le = 2,
    isc_left_right = 9, // DUMMY, used in comparison
    isc_gt = 4,
    isc_ge = 5,
    isc_greater_beetween = 9, // DUMMY, used in comparison
    isc_gt_lt = 10,
    isc_gt_le = 11,
    isc_ge_lt = 12,
    isc_ge_le = 13
};

inline const char* 
index_scan_condition2string(index_scan_condition isc)
{
    switch(isc)
    {
    case isc_eq: return "EQ";
    case isc_lt: return "LT";
    case isc_le: return "LE";
    case isc_gt: return "GT";
    case isc_ge: return "GE";
    case isc_gt_lt: return "INT";
    case isc_gt_le: return "HINTR";
    case isc_ge_lt: return "HINTL";
    case isc_ge_le: return "SEG";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in index scan condition to string conversion");
    }
}

class PPIndexScan : public PPIterator
{
protected:
    typedef void (PPIndexScan::*t_next_fun)(tuple &t);

    PPOpIn index_name, child, child2;
    index_scan_condition isc;

    tuple_cell left_bound, right_bound;
    bool left_bound_exclusive, right_bound_exclusive;

    xmlscm_type idx_type;

    scoped_ptr<idx::KeyValueIterator> cursor;
    idx::KeyValueMultimap * index;

    xptr res;
    t_next_fun next_fun;
    bool first_time;

    void next_eq     (tuple &t);
    void next_lt_le  (tuple &t);
    void next_gt_ge  (tuple &t);
    void next_between(tuple &t);

    /* This is the common initialization function for all next interators */
    void initialize   ();

private:
    CollationHandler* collation;

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPIndexScan(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _index_name_,
                PPOpIn _child_,
                PPOpIn _child2_,
                index_scan_condition _isc_);

    virtual ~PPIndexScan();
    inline index_scan_condition get_index_scan_condition() { return isc; }
};


#endif
