/*
 * File:  PPDropIndex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropIndex.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/idx/indecies.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"


PPDropIndex::PPDropIndex(PPOpIn _index_name_, 
                         dynamic_context *_cxt_) : PPUpdate("PPDropIndex"),
                                                   index_name(_index_name_), 
                                                   cxt(_cxt_)
{
}

PPDropIndex::~PPDropIndex()
{
    delete index_name.op;
    index_name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropIndex::do_open()
{
    local_lock_mrg->lock(lm_x); // because Leon changes the descriptive schema of the document/collection
    cxt->global_variables_open();
    index_name.op->open();
}

void PPDropIndex::do_close()
{
    index_name.op->close();
    cxt->global_variables_close();
}

void PPDropIndex::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    index_name.op->accept(v);    
    v.pop();
}

void PPDropIndex::do_execute()
{
    tuple_cell tc = get_name_from_PPOpIn(index_name, "index", "drop index");
    get_schema_node(find_db_entity_for_object(catobj_indicies, tc.get_str_mem()), 
                    "Unknown database entity passed to drop index");
    local_lock_mrg->put_lock_on_index(tc.get_str_mem());
    auth_for_drop_object(tc.get_str_mem(), "index", false);
    drop_index(tc.get_str_mem());
}

