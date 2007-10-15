/*
 * File:  PPAxisAttribute.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisAttribute.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisAttribute
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisAttribute::PPAxisAttribute(dynamic_context *_cxt_, 
                                 PPOpIn _child_,
                                 NodeTestType _nt_type_,
                                 NodeTestData _nt_data_) : PPIterator(_cxt_),
                                                           child(_child_),
                                                           nt_type(_nt_type_),
                                                           nt_data(_nt_data_)
{
    switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisAttribute::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisAttribute::next_comment; break;
        case node_test_text						: next_fun = &PPAxisAttribute::next_text; break;
        case node_test_node						: next_fun = &PPAxisAttribute::next_node; break;
        case node_test_string					: next_fun = &PPAxisAttribute::next_string; break;
        case node_test_qname					: next_fun = &PPAxisAttribute::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisAttribute::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisAttribute::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisAttribute::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisAttribute::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisAttribute::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }
}

PPAxisAttribute::~PPAxisAttribute()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisAttribute::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisAttribute::reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisAttribute::close ()
{
    child.op->close();
}


void PPAxisAttribute::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAttribute::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAttribute::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAttribute::next_node(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstByOrderAttributeChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderAttribute(cur);
}

void PPAxisAttribute::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_string");
}

void PPAxisAttribute::next_qname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         nt_data.uri,
                         nt_data.ncname_local,
                         attribute,
                         comp_qname_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

void PPAxisAttribute::next_wildcard_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = getFirstByOrderAttributeChild(child.get(t).get_node());
    }

    t.copy(tuple_cell::node(cur));
    cur = getNextByOrderAttribute(cur);
}

void PPAxisAttribute::next_wildcard_ncname_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         nt_data.uri,
                         NULL,
                         attribute,
                         comp_uri_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

void PPAxisAttribute::next_wildcard_star_ncname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) {UNDO_XQUERY_LINE; return;}

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = merge.init(child.get(t).get_node(),
                         NULL,
                         nt_data.ncname_local,
                         attribute,
                         comp_local_type);
    }

    t.copy(tuple_cell::node(cur));
    cur = merge.next();
}

void PPAxisAttribute::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_function_call");
}

void PPAxisAttribute::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_var_name");
}

sequence *PPAxisAttribute::next_processing_instruction_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    delete data_seq;
    return se_new sequence(1);
}

sequence *PPAxisAttribute::next_comment_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    delete data_seq;
    return se_new sequence(1);
}

sequence *PPAxisAttribute::next_text_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    delete data_seq;
    return se_new sequence(1);
}

sequence *PPAxisAttribute::next_node_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    delete data_seq;
    return se_new sequence(1);
}

sequence *PPAxisAttribute::next_string_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_string_s");
}

sequence *PPAxisAttribute::next_qname_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    sequence *res_seq = se_new sequence(1);
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
                                  attribute,NULL);

        if (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisAttribute::next_wildcard_star_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    sequence *res_seq = se_new sequence(1);
/*
    tuple t(1);
    xptr cur;
    for (int i = 0; i < data_seq->size(); i++)
    {
        data_seq->get(t, i);
        tuple_cell tc = t.cells[0];
        if (!(tc.is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = getFirstByOrderAttributeChild(tc.get_node());

        while (cur != NULL)
        {
            t.cells[0] = tuple_cell::node(cur);
            res_seq->add(t);
            cur = getNextByOrderAttribute(cur);
        }
    }
    delete data_seq;
*/
    return res_seq;
}

sequence *PPAxisAttribute::next_wildcard_ncname_star_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_wildcard_ncname_star_s");
}

sequence *PPAxisAttribute::next_wildcard_star_ncname_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_wildcard_star_ncname_s");
}

sequence *PPAxisAttribute::next_function_call_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_function_call_s");
}

sequence *PPAxisAttribute::next_var_name_s(sequence *data_seq, PPAxisAttribute* cur_op)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAttribute::next_var_name_s");
}

PPIterator* PPAxisAttribute::copy(dynamic_context *_cxt_)
{
    PPAxisAttribute *res = se_new PPAxisAttribute(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPAxisAttribute::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPAxisAttribute*)cur)->children(child);

    void *ac_r;
    bool ac_s = (child.op->res_fun())(child.op, cxt, ac_r);

    if (!ac_s) // if expression is not strict
    { // create PPAxisAttribute and transmit state
        child.op = (PPIterator*)ac_r;
        r = se_new PPAxisAttribute(cxt, child, ((PPAxisAttribute*)cur)->nt_type, ((PPAxisAttribute*)cur)->nt_data);
        return false;
    }

    sequence *res_seq;
    switch (((PPAxisAttribute*)cur)->nt_type)
    {
        case node_test_processing_instruction	: res_seq = next_processing_instruction_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_comment					: res_seq = next_comment_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_text						: res_seq = next_text_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_node						: res_seq = next_node_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_string					: res_seq = next_string_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_qname					: res_seq = next_qname_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_wildcard_star			: res_seq = next_wildcard_star_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_wildcard_ncname_star		: res_seq = next_wildcard_ncname_star_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_wildcard_star_ncname		: res_seq = next_wildcard_star_ncname_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_function_call			: res_seq = next_function_call_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        case node_test_var_name					: res_seq = next_var_name_s((sequence*)ac_r, (PPAxisAttribute*)cur); break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }

    return strict_op_result(cur, res_seq, cxt, r);
}

