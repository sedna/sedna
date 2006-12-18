/*
 * File:  PPBase.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "PPBase.h"
#include "PPSLStub.h"
#include "PPFunCall.h"

global_variable_context dynamic_context::glb_var_cxt;
function_context dynamic_context::funct_cxt;
dynamic_context_info **dynamic_context::infos = NULL;
int dynamic_context::infos_num = 0;
int dynamic_context::infos_pos = 0;



global_producer::~global_producer() { delete op; op = NULL; }
void global_producer::open() { op->open(); }
void global_producer::close() { ((PPIterator*)op)->close(); }




bool strict_op_result(PPIterator* cur, sequence *res_seq, dynamic_context *cxt, void*& r)
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

static_context st_ct;

char mem_str_buf[MAX_MEM_STR_SIZE + 1];
char mem_str_buf2[MAX_MEM_STR_SIZE + 1];

char e_string_buf[PAGE_SIZE];
}