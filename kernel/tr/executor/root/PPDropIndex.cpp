/*
 * File:  PPDropIndex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPDropIndex.h"
#include "indexes.h"
#include "PPUtils.h"
#include "locks.h"


PPDropIndex::PPDropIndex(PPOpIn _index_name_) : index_name(_index_name_)
{
}

PPDropIndex::~PPDropIndex()
{
    delete index_name.op;
    index_name.op = NULL;
}

void PPDropIndex::open()
{
    local_lock_mrg->lock(lm_x); // because Leon changes the descriptive schema of the document/collection
    index_name.op->open();
}

void PPDropIndex::close()
{
    index_name.op->close();
}

void PPDropIndex::execute()
{
    tuple_cell tc;
    tuple t(1);
    index_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = index_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    index_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        
    tc = tuple_cell::make_sure_light_atomic(tc);

    schema_node *root = get_schema_node(find_entity(tc.get_str_mem()), "Unknown entity passed to PPDropIndex");
    local_lock_mrg->put_lock_on_index(tc.get_str_mem());
    delete_index(tc.get_str_mem());
}

