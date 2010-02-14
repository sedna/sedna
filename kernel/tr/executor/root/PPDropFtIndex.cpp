/*
 * File:  PPDropFtIndex.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropFtIndex.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPDropFtIndex::PPDropFtIndex(PPOpIn _index_name_, dynamic_context *_cxt_) : index_name(_index_name_), cxt(_cxt_)
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
    dynamic_context::global_variables_open();
}

void PPDropFtIndex::do_close()
{
    index_name.op->close();
    dynamic_context::global_variables_close();
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

	delete_ft_index(tc.get_str_mem());
}

