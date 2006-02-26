/*
 * File:  PPBase.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPBase.h"
#include "PPSLStub.h"
#include "PPFunCall.h"



bool strict_op_result(PPIterator* cur, sequence *res_seq, variable_context *cxt, void*& r)
{
    if (res_seq->size() > STRICT_FUNS_BOUND)
    {
        PPIterator *new_tree = cur->copy(cxt);
        PPSLStub *new_node = new PPSLStub(cxt, new_tree, res_seq);
        new_node->open();
        r = new_node;
        return false;
    }
    else
    {
        r = res_seq;
        return true;
    }
}


namespace tr_globals
{

query_prolog qp;
static_context st_ct;

char mem_str_buf[MAX_MEM_STR_SIZE + 1];

char e_string_buf[PAGE_SIZE];
}