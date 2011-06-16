/*
 * File:  PPDropFtIndex.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropFtIndex.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/auth/auc.h"
#include "tr/ft/ft_index_data.h"

PPDropFtIndex::PPDropFtIndex(PPOpIn _index_name_,
                             dynamic_context *_cxt_) : PPUpdate("PPDropFtIndex"),
                                                       index_name(_index_name_),
                                                       cxt(_cxt_)
{
}

PPDropFtIndex::~PPDropFtIndex()
{
    delete index_name.op;
    index_name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropFtIndex::do_open()
{
    index_name.op->open();
    cxt->global_variables_open();
}

void PPDropFtIndex::do_close()
{
    index_name.op->close();
    cxt->global_variables_close();
}

void PPDropFtIndex::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    index_name.op->accept(v);    
    v.pop();
}

void PPDropFtIndex::do_execute()
{

    tuple_cell tc = get_name_from_PPOpIn(index_name, "full-text index", "drop full-text index");
    get_schema_node(find_db_entity_for_object(catobj_ft_indicies, tc.get_str_mem()),
                    "Unknown database entity passed to drop index");
    auth_for_drop_object(tc.get_str_mem(), "ft-index", false);
    delete_ft_index(tc.get_str_mem());
}
