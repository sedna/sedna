/*
 * File:  PPAxisParent.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAxisParent.h"
#include "node_utils.h"
#include "PPUtils.h"
#include "dm_accessors.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisParent
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPAxisParent::PPAxisParent(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_)
{
    switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisParent::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisParent::next_comment; break;
        case node_test_text						: next_fun = &PPAxisParent::next_text; break;
        case node_test_node						: next_fun = &PPAxisParent::next_node; break;
        case node_test_string					: next_fun = &PPAxisParent::next_string; break;
        case node_test_qname					: next_fun = &PPAxisParent::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisParent::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisParent::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisParent::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisParent::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisParent::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }
}

PPAxisParent::~PPAxisParent()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisParent::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisParent::reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisParent::close ()
{
    child.op->close();
}


void PPAxisParent::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisParent::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisParent::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisParent::next_node(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
		cur = get_parent_node(cur);
		if (cur!=XNULL)
		{
			CHECKP(cur);
			if (GETSCHEMENODEX(cur)->type==virtual_root)
				cur=XNULL;
		}
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_string");
}

void PPAxisParent::next_qname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
		if (cur==XNULL) continue;
        CHECKP(cur);
        if (!comp_qname_type(GETSCHEMENODEX(cur),
                              nt_data.uri,
                              nt_data.ncname_local, 
                              element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
		cur = get_parent_node(cur);
		if (cur==XNULL) continue;
        CHECKP(cur);
        if (!comp_type(GETSCHEMENODEX(cur), 
                        NULL,
                        NULL, 
                        element)) cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_ncname_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
		if (cur==XNULL) continue;
        CHECKP(cur);
        if (!
              comp_uri_type(GETSCHEMENODEX(cur),
                            nt_data.uri,
                            NULL, 
                            element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_wildcard_star_ncname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        CHECKP(cur);
		if (cur==XNULL) continue;
        if (!
              comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname_local, 
                              element))
            cur = XNULL;
    }

    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}

void PPAxisParent::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_function_call");
}

void PPAxisParent::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_var_name");
}

sequence *PPAxisParent::next_processing_instruction_s(sequence *data_seq, PPAxisParent* cur_op)
{
    delete data_seq;
    return new sequence(1);
}

sequence *PPAxisParent::next_comment_s(sequence *data_seq, PPAxisParent* cur_op)
{
    delete data_seq;
    return new sequence(1);
}

sequence *PPAxisParent::next_text_s(sequence *data_seq, PPAxisParent* cur_op)
{
    delete data_seq;
    return new sequence(1);
}

sequence *PPAxisParent::next_node_s(sequence *data_seq, PPAxisParent* cur_op)
{
    sequence *res_seq = new sequence(1);
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = tc.get_node();
        CHECKP(cur);
        cur = GETPARENTPOINTER(cur);

        if (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
        }
    }
    delete data_seq;

    return res_seq;
}

sequence *PPAxisParent::next_string_s(sequence *data_seq, PPAxisParent* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_string_s");
}

sequence *PPAxisParent::next_qname_s(sequence *data_seq, PPAxisParent* cur_op)
{
    sequence *res_seq = new sequence(1);
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = tc.get_node();
        CHECKP(cur);
        cur = GETPARENTPOINTER(cur);
        CHECKP(cur);

        if (cur != NULL &&
            GETSCHEMENODEX(cur)->type == element && 
            strcmp(GETSCHEMENODEX(cur)->name, cur_op->nt_data.ncname_local) == 0)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
        }
    }
    delete data_seq;

    return res_seq;
}

sequence *PPAxisParent::next_wildcard_star_s(sequence *data_seq, PPAxisParent* cur_op)
{
    sequence *res_seq = new sequence(1);
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = tc.get_node();
        CHECKP(cur);
        cur = GETPARENTPOINTER(cur);
        CHECKP(cur);

        if (cur != NULL &&
            GETSCHEMENODEX(cur)->type == element)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
        }
    }
    delete data_seq;

    return res_seq;
}

sequence *PPAxisParent::next_wildcard_ncname_star_s(sequence *data_seq, PPAxisParent* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_wildcard_ncname_star_s");
}

sequence *PPAxisParent::next_wildcard_star_ncname_s(sequence *data_seq, PPAxisParent* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_wildcard_star_ncname_s");
}

sequence *PPAxisParent::next_function_call_s(sequence *data_seq, PPAxisParent* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_function_call_s");
}

sequence *PPAxisParent::next_var_name_s(sequence *data_seq, PPAxisParent* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisParent::next_var_name_s");
}

PPIterator* PPAxisParent::copy(dynamic_context *_cxt_)
{
    PPAxisParent *res = new PPAxisParent(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPAxisParent::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPAxisParent*)cur)->children(child);

    void *ac_r;
    bool ac_s = (child.op->res_fun())(child.op, cxt, ac_r);

    if (!ac_s) // if expression is not strict
    { // create PPAxisParent and transmit state
        child.op = (PPIterator*)ac_r;
        r = new PPAxisParent(cxt, child, ((PPAxisParent*)cur)->nt_type, ((PPAxisParent*)cur)->nt_data);
        return false;
    }

    sequence *res_seq;
    switch (((PPAxisParent*)cur)->nt_type)
    {
        case node_test_processing_instruction	: res_seq = next_processing_instruction_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_comment					: res_seq = next_comment_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_text						: res_seq = next_text_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_node						: res_seq = next_node_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_string					: res_seq = next_string_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_qname					: res_seq = next_qname_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_wildcard_star			: res_seq = next_wildcard_star_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_wildcard_ncname_star		: res_seq = next_wildcard_ncname_star_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_wildcard_star_ncname		: res_seq = next_wildcard_star_ncname_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_function_call			: res_seq = next_function_call_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        case node_test_var_name					: res_seq = next_var_name_s((sequence*)ac_r, (PPAxisParent*)cur); break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }

    return strict_op_result(cur, res_seq, cxt, r);
}
