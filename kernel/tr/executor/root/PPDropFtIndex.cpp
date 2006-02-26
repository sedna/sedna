/*
 * File:  PPDropFtIndex.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPDropFTIndex.h"
#include "PPUtils.h"

PPDropFtIndex::PPDropFtIndex(PPOpIn _index_name_) :	index_name(_index_name_)
{
}


PPDropFtIndex::~PPDropFtIndex()
{
    delete index_name.op;
    index_name.op = NULL;
}

void PPDropFtIndex::open()
{
    index_name.op->open();
}

void PPDropFtIndex::close()
{
    index_name.op->close();
}

void PPDropFtIndex::execute()
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

	ft_index_cell::delete_index(tc.get_str_mem());
}

