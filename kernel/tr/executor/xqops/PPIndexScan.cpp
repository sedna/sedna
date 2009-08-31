/*
 * File:  PPIndexScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPIndexScan.h"
#include "tr/locks/locks.h"
#include "tr/idx/indexes.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"

PPIndexScan::PPIndexScan(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _index_name_,
                         PPOpIn _child_,
                         PPOpIn _child2_,
                         index_scan_condition _isc_) : PPIterator(_cxt_, _info_),
                                                       index_name(_index_name_),
                                                       child(_child_),
                                                       child2(_child2_),
                                                       isc(_isc_)
{
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
}

void PPIndexScan::do_open ()
{
    switch (isc)
    {
        case isc_eq		: next_fun = &PPIndexScan::next_eq; break;
        case isc_lt		:
        case isc_le		: next_fun = &PPIndexScan::next_lt_le; break;
        case isc_gt		:
        case isc_ge		: next_fun = &PPIndexScan::next_gt_ge; break;
        case isc_gt_lt	:
        case isc_gt_le	:
        case isc_ge_lt	:
        case isc_ge_le	: next_fun = &PPIndexScan::next_between; break;
        default			: throw USER_EXCEPTION2(SE1003, "Unexpected index scan condition (internal error, please report a bug)");
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


void obtain_tuple_cell(tuple_cell /*out*/ &tc, PPOpIn /*out*/ &child, xmlscm_type idx_type)
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
	
	tuple_cell index_name_tuple;
    obtain_tuple_cell(index_name_tuple, index_name, xs_string);
	char * index_name_string = index_name_tuple.get_str_mem();
	
    // Put lock on documents under index scan and check security for document
    get_schema_node(find_db_entity_for_index(index_name_string), "Unknown entity passed to PPIndexScan");
    // we don't need to check auth privilege for using index, because
    // read access to index is allowed to everyone

    // Find B-Tree root
    btree = find_btree(index_name_string);
    if (btree == XNULL) throw XQUERY_EXCEPTION2(SE1061, index_name_string);

    idx_type = get_index_xmlscm_type(index_name_string);
    if (idx_type == -1) throw XQUERY_EXCEPTION2(SE1061, index_name_string);
}


void PPIndexScan::next_eq(tuple &t)
{
    if (first_time)
    {
		initialize();
		
        obtain_tuple_cell(tc, child, idx_type);

        tuple_cell2bt_key(tc, key);
        cursor = bt_find(btree, key);

        first_time = false;
    }

    res = cursor.bt_next_obj();

    if (res == XNULL) SET_EOS_AND_EXIT

    DEREF_AND_SET
}

void PPIndexScan::next_lt_le(tuple &t)
{
    if (first_time)
    {
		initialize();
		
        obtain_tuple_cell(tc, child, idx_type);

        tuple_cell2bt_key(tc, key);
        cursor = bt_lm(btree);

        //if (!cursor.bt_next_key()) SET_EOS_AND_EXIT

        if (!(isc == isc_lt ? cursor.get_key() <  key
                            : cursor.get_key() <= key)) SET_EOS_AND_EXIT
        else first_time = false;
    }

    while (true)
    {
        res = cursor.bt_next_obj();
        if (res != XNULL) break;

        if (cursor.bt_next_key())
            if (isc == isc_lt ? cursor.get_key() <  key
                              : cursor.get_key() <= key) continue;
        
        SET_EOS_AND_EXIT
    }

    DEREF_AND_SET
}

void PPIndexScan::next_gt_ge(tuple &t)
{
    if (first_time)
    {
		initialize();

		obtain_tuple_cell(tc, child, idx_type);

        tuple_cell2bt_key(tc, key);
        cursor = isc == isc_gt ? bt_find_gt(btree, key)
                               : bt_find_ge(btree, key);

        first_time = false;
    }

    while (true)
    {
        res = cursor.bt_next_obj();
        if (res != XNULL) break;

        if (!cursor.bt_next_key()) SET_EOS_AND_EXIT
    }

    DEREF_AND_SET
}

void PPIndexScan::next_between(tuple &t)
{
    if (first_time)
    {
		initialize();

		obtain_tuple_cell(tc, child, idx_type);
        obtain_tuple_cell(tc2, child2, idx_type);

        tuple_cell2bt_key(tc, key);
        tuple_cell2bt_key(tc2, key2);
        cursor = (isc == isc_gt_lt || isc == isc_gt_le) ? bt_find_gt(btree, key)
                                                        : bt_find_ge(btree, key);

        if (cursor.is_null()) SET_EOS_AND_EXIT

        if (!((isc == isc_gt_lt || isc == isc_ge_lt) ? cursor.get_key() <  key2
                                                     : cursor.get_key() <= key2)) SET_EOS_AND_EXIT

        first_time = false;
    }

    while (true)
    {
        res = cursor.bt_next_obj();
        if (res != XNULL) break;

        if (cursor.bt_next_key())
            if ((isc == isc_gt_lt || isc == isc_ge_lt) ? cursor.get_key() <  key2
                                                       : cursor.get_key() <= key2) continue;
        
        SET_EOS_AND_EXIT
    }

    DEREF_AND_SET
}

PPIterator* PPIndexScan::do_copy(dynamic_context *_cxt_)
{
    PPIndexScan *res = NULL;

    if (child.op && child2.op)
    {
        res = se_new PPIndexScan(_cxt_, info, index_name, child, child2, isc);
		res->index_name.op = index_name.op->copy(_cxt_);
        res->child.op = child.op->copy(_cxt_);
        res->child2.op = child2.op->copy(_cxt_);
    }

    return res;
}
