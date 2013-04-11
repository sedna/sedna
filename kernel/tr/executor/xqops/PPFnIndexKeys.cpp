/*
 * File: PPFnIndexKeys.cpp
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnIndexKeys.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/PPUtils.h"

#include "tr/cat/catalog.h"
#include "tr/idx/indecies.h"


PPFnIndexKeys::PPFnIndexKeys(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _index_name_) : PPIterator(_cxt_, _info_, "PPFnIndexKeys"),
                                                index_name(_index_name_)
{
}

PPFnIndexKeys::~PPFnIndexKeys()
{
    if(index_name.op)
    {
        delete index_name.op;
        index_name.op = NULL;
    }
}

void PPFnIndexKeys::do_open ()
{
    if (index_name.op) index_name.op->open();
    first_time = true;
}


void PPFnIndexKeys::do_reopen()
{
    if (index_name.op) index_name.op->reopen();
    first_time = true;
}


void PPFnIndexKeys::do_close()
{
    if (index_name.op) index_name.op->close();
}

void PPFnIndexKeys::initialize()
{
    U_ASSERT(first_time);

    tuple_cell tc_name = get_name_from_PPOpIn(index_name, "index", "index scan", false, false);

    // Put lock on documents under index scan and check security for document
    get_schema_node(find_db_entity_for_object(catobj_indicies, tc_name.get_str_mem()), "Unknown entity passed to index scan");

    index_cell_cptr idc(tc_name.get_str_mem());
    if (!idc.found()) {
        throw XQUERY_EXCEPTION2(SE1061, tc_name.get_str_mem());
    }

    index = idc->get_backend();
}

void PPFnIndexKeys::do_next(tuple& t)
{
    if (first_time) {
        tuple_cell current_key;
        initialize();

        cursor = index->begin();
        U_ASSERT(!cursor.isnull());

        first_time = false;
    }

    t.copy(cursor->getKey());

    if (t.is_eos()) {
        first_time = true;
        cursor.clear();
    } else {
        cursor->nextKey();
    }
}

PPIterator* PPFnIndexKeys::do_copy(dynamic_context *_cxt_)
{
    PPFnIndexKeys *res = NULL;
    res = new PPFnIndexKeys(_cxt_, info, index_name);
    if(index_name.op) res->index_name.op = index_name.op->copy(_cxt_);
    return res;
}

void PPFnIndexKeys::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (index_name.op) index_name.op->accept(v);
    v.pop();
}
