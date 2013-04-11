/*
 * File:  PPIndexScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPIndexScan.h"
#include "tr/locks/locks.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/idx/indecies.h"

#include "tr/bstrie/sednabtrie.h"
#include "tr/executor/fo/op_map.h"

PPIndexScan::PPIndexScan(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _index_name_,
                         PPOpIn _child_,
                         PPOpIn _child2_,
                         index_scan_condition _isc_) : PPIterator(_cxt_, _info_, "PPIndexScan"),
                                                       index_name(_index_name_),
                                                       child(_child_),
                                                       child2(_child2_),
                                                       isc(_isc_)
{
    collation = cxt->get_static_context()->get_default_collation();
}

PPIndexScan::~PPIndexScan()
{
    if (child.op)
    {
        delete child.op;
        child.op = NULL;
    }
    if (child2.op)
    {
        delete child2.op;
        child2.op = NULL;
    }
    if(index_name.op)
    {
        delete index_name.op;
        index_name.op = NULL;
    }
}

void PPIndexScan::do_open ()
{
    right_bound_exclusive = false;
    left_bound_exclusive = false;

    switch (isc)
    {
        case isc_eq     : next_fun = &PPIndexScan::next_eq; break;
        case isc_lt     : right_bound_exclusive = true;
        case isc_le     : next_fun = &PPIndexScan::next_lt_le; break;
        case isc_gt     : left_bound_exclusive = true;
        case isc_ge     : next_fun = &PPIndexScan::next_eq; break;
        case isc_gt_lt  :
        case isc_gt_le  : left_bound_exclusive = true;
        case isc_ge_lt  : if (isc != isc_gt_le) { right_bound_exclusive = true; }
        case isc_ge_le  : next_fun = &PPIndexScan::next_between; break;
        default         : throw USER_EXCEPTION2(SE1003, "Unexpected index scan condition (internal error, please report a bug)");
    }

    if (index_name.op) index_name.op->open();
    if (child.op) child.op->open();
    if (child2.op) child2.op->open();

    first_time = true;
}


void PPIndexScan::do_reopen()
{
    if (index_name.op) index_name.op->reopen();
    if (child.op) child.op->reopen();
    if (child2.op) child2.op->reopen();

    first_time = true;
}


void PPIndexScan::do_close()
{
    if (index_name.op) index_name.op->close();
    if (child.op) child.op->close();
    if (child2.op) child2.op->close();
}


#define SET_EOS_AND_EXIT		    {								\
                                        first_time = true;			\
                                        t.set_eos();				\
                                        return;	                    \
                                    }

#define DEREF_AND_SET				CHECKP(res);					\
                                    t.copy(tuple_cell::node(*(xptr*)(XADDR(res)))); \
                                    res = *(xptr*)(XADDR(res));


void get_casted_value(tuple_cell /*out*/ &tc, PPOpIn /*out*/ &child, xmlscm_type idx_type)
{
    if (child.op)
    {
        tuple t(1);
        child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Value argument of index-scan is not a single sequence.");

        tc = child.get(t);
        tc = atomize(tc);

        child.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Value argument of index-scan is not a single sequence.");
    }

	tc = cast(tc, idx_type);
}


void PPIndexScan::initialize()
{
    U_ASSERT(first_time);

    tuple_cell tc_name = get_name_from_PPOpIn(index_name, "index", "index scan", false, false);

    // Put lock on documents under index scan and check security for document
    get_schema_node(find_db_entity_for_object(catobj_indicies, tc_name.get_str_mem()), "Unknown entity passed to index scan");

    // we don't need to check auth privilege for using index, because
    // read access to index is allowed to everyone

    index_cell_cptr idc(tc_name.get_str_mem());
    if (!idc.found()) {
        throw XQUERY_EXCEPTION2(SE1061, tc_name.get_str_mem());
    }

    index = idc->get_backend();
    idx_type = idc->get_keytype();
}

void PPIndexScan::do_next(tuple& t)
{
    (this->*next_fun)(t);
}

void PPIndexScan::next_eq(tuple &t)
{
    if (first_time) {
        tuple_cell current_key;
        bool key_equal;

        initialize();

        get_casted_value(left_bound, child, idx_type);
        cursor = index->find(left_bound);
        U_ASSERT(!cursor.isnull());

        current_key = cursor->getKey();

        key_equal = op_eq(current_key, left_bound, collation).is_boolean_true();

        if ((isc != isc_eq) && left_bound_exclusive && key_equal) {
            cursor->nextKey();
        }

        if (current_key.is_eos() || ((isc == isc_eq) && !key_equal)) {
            t.set_eos();
            return;
        }

        first_time = false;
    }

    t.copy(cursor->getValue());

    if (t.is_eos()) {
        first_time = true;
        cursor.clear();
    } else if (isc == isc_eq) {
        cursor->nextValue();
    } else {
        cursor->nextPair();
    }
}

void PPIndexScan::next_lt_le(tuple &t)
{
    bool is_left_bound = false;
    tuple_cell current_key;

    if (first_time) {
        initialize();

        get_casted_value(right_bound, child, idx_type);
        cursor = index->begin();
        U_ASSERT(!cursor.isnull());
        
        first_time = false;
    }

    current_key = cursor->getKey();

    if (current_key.is_eos() || !(right_bound_exclusive ?
          op_lt(current_key, right_bound, collation).is_boolean_true() :
          op_le(current_key, right_bound, collation).is_boolean_true())) {
        t.set_eos();
        first_time = true;
        cursor.clear();
    } else {
        t.copy(cursor->getValue());
        cursor->nextPair();
    }
}

void PPIndexScan::next_between(tuple &t)
{
    tuple_cell current_key;
    if (first_time) {

        initialize();

        get_casted_value(left_bound, child, idx_type);
        get_casted_value(right_bound, child2, idx_type);

        cursor = index->find(left_bound);
        U_ASSERT(!cursor.isnull());

        current_key = cursor->getKey();

        if (left_bound_exclusive && op_eq(current_key, left_bound, collation).is_boolean_true()) {
            cursor->nextKey();
        }

        if (current_key.is_eos()) {
            t.set_eos();
            return;
        }

        first_time = false;
    }

    current_key = cursor->getKey();

    if (current_key.is_eos() || !(right_bound_exclusive ?
          op_lt(current_key, right_bound, collation).is_boolean_true() :
          op_le(current_key, right_bound, collation).is_boolean_true())) {
        t.set_eos();
        first_time = true;
        cursor.clear();
    } else {
        t.copy(cursor->getValue());
        cursor->nextPair();
    }
}

PPIterator* PPIndexScan::do_copy(dynamic_context *_cxt_)
{
    PPIndexScan *res = NULL;
    res = se_new PPIndexScan(_cxt_, info, index_name, child, child2, isc);
    if(index_name.op) res->index_name.op = index_name.op->copy(_cxt_);
    if(child.op) res->child.op = child.op->copy(_cxt_);
    if(child2.op) res->child2.op = child2.op->copy(_cxt_);
    return res;
}

void PPIndexScan::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (index_name.op) index_name.op->accept(v);
    if (child.op) child.op->accept(v);
    if (child2.op) child2.op->accept(v);
    v.pop();
}
