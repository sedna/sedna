/*
 * File:  PPAxisChild.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAxisChild.h"
#include "node_utils.h"
#include "PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisChild
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisChild::PPAxisChild(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_)
{
    switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisChild::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisChild::next_comment; break;
        case node_test_text						: next_fun = &PPAxisChild::next_text; break;
        case node_test_node						: next_fun = &PPAxisChild::next_node; break;
        case node_test_string					: next_fun = &PPAxisChild::next_string; break;
        case node_test_qname					: next_fun = &PPAxisChild::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisChild::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisChild::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisChild::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisChild::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisChild::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }
}

PPAxisChild::~PPAxisChild()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisChild::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisChild::reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisChild::close ()
{
    child.op->close();
}


void PPAxisChild::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisChild::next_comment(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getChildPointerXptr(child.get(t).get_node(), NULL, comment, NULL);
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextSiblingOfSameSortXptr(cur);
}

void PPAxisChild::next_text(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getChildPointerXptr(child.get(t).get_node(), NULL, text, NULL);
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextSiblingOfSameSortXptr(cur);
}

void PPAxisChild::next_node(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getFirstByOrderNoneAttributeChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderNoneAttribute(cur);
}

void PPAxisChild::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_string");
}

void PPAxisChild::next_qname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         cxt->st_cxt->get_uri_by_prefix(nt_data.ncname_prefix, element),
                         nt_data.ncname_local,
                         element,
                         comp_qname_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_wildcard_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getFirstByOrderElementChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderElement(cur);
}

void PPAxisChild::next_wildcard_ncname_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         cxt->st_cxt->get_uri_by_prefix(nt_data.ncname_prefix, element),
                         NULL,
                         element,
                         comp_uri_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_wildcard_star_ncname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         NULL,
                         nt_data.ncname_local,
                         element,
                         comp_local_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next(cur);
}

void PPAxisChild::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_function_call");
}

void PPAxisChild::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_var_name");
}

sequence *PPAxisChild::next_processing_instruction_s(sequence *data_seq, PPAxisChild* cur_op)
{
    delete data_seq;
    return new sequence(1);
}

sequence *PPAxisChild::next_comment_s(sequence *data_seq, PPAxisChild* cur_op)
{
    delete data_seq;
    return new sequence(1);
}

sequence *PPAxisChild::next_text_s(sequence *data_seq, PPAxisChild* cur_op)
{
    sequence *res_seq = new sequence(1);
/*
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getChildPointerXptr(tc.get_node(), NULL, text,NULL);

        while (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
            cur = getNextSiblingOfSameSortXptr(cur);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisChild::next_node_s(sequence *data_seq, PPAxisChild* cur_op)
{
    sequence *res_seq = new sequence(1);
/*
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getFirstByOrderNoneAttributeChild(tc.get_node());

        while (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
            cur = getNextByOrderNoneAttribute(cur);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisChild::next_string_s(sequence *data_seq, PPAxisChild* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_string_s");
}

sequence *PPAxisChild::next_qname_s(sequence *data_seq, PPAxisChild* cur_op)
{
    sequence *res_seq = new sequence(1);
/*
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getChildPointerXptr(tc.get_node(), 
                                  cur_op->nt_data.qname.LocalPart.c_str(), 
                                  element,NULL);

        while (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
            cur = getNextSiblingOfSameSortXptr(cur);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisChild::next_wildcard_star_s(sequence *data_seq, PPAxisChild* cur_op)
{
    sequence *res_seq = new sequence(1);
/*
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getFirstByOrderElementChild(tc.get_node());

        while (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
            cur = getNextByOrderElement(cur);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisChild::next_wildcard_ncname_star_s(sequence *data_seq, PPAxisChild* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_wildcard_ncname_star_s");
}

sequence *PPAxisChild::next_wildcard_star_ncname_s(sequence *data_seq, PPAxisChild* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_wildcard_star_ncname_s");
}

sequence *PPAxisChild::next_function_call_s(sequence *data_seq, PPAxisChild* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_function_call_s");
}

sequence *PPAxisChild::next_var_name_s(sequence *data_seq, PPAxisChild* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_var_name_s");
}

PPIterator* PPAxisChild::copy(dynamic_context *_cxt_)
{
    PPAxisChild *res = new PPAxisChild(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPAxisChild::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPAxisChild*)cur)->children(child);

    void *ac_r;
    bool ac_s = (child.op->res_fun())(child.op, cxt, ac_r);

    if (!ac_s) // if expression is not strict
    { // create PPAxisChild and transmit state
        child.op = (PPIterator*)ac_r;
        r = new PPAxisChild(cxt, child, ((PPAxisChild*)cur)->nt_type, ((PPAxisChild*)cur)->nt_data);
        return false;
    }

    sequence *res_seq;
    switch (((PPAxisChild*)cur)->nt_type)
    {
        case node_test_processing_instruction	: res_seq = next_processing_instruction_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_comment					: res_seq = next_comment_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_text						: res_seq = next_text_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_node						: res_seq = next_node_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_string					: res_seq = next_string_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_qname					: res_seq = next_qname_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_wildcard_star			: res_seq = next_wildcard_star_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_wildcard_ncname_star		: res_seq = next_wildcard_ncname_star_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_wildcard_star_ncname		: res_seq = next_wildcard_star_ncname_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_function_call			: res_seq = next_function_call_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        case node_test_var_name					: res_seq = next_var_name_s((sequence*)ac_r, (PPAxisChild*)cur); break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }

    return strict_op_result(cur, res_seq, cxt, r);
}
