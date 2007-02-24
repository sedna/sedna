/*
 * File:  PPAxisAncestor.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisAncestor.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/dm_accessors.h"

void PPAxisAncestor::init_function()
{
	switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisAncestor::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisAncestor::next_comment; break;
        case node_test_text						: next_fun = &PPAxisAncestor::next_text; break;
        case node_test_node						: next_fun = &PPAxisAncestor::next_node; break;
        case node_test_string					: next_fun = &PPAxisAncestor::next_string; break;
        case node_test_qname					: next_fun = &PPAxisAncestor::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisAncestor::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisAncestor::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisAncestor::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisAncestor::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisAncestor::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
    }
}
PPAxisAncestor::PPAxisAncestor(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
						 NodeTestData _nt_data_):PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_)
{
	self=false; 
	init_function();
}
PPAxisAncestor::PPAxisAncestor(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_,bool _self_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_),
												   self(_self_)
{
    init_function();
}

PPAxisAncestor::~PPAxisAncestor()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisAncestor::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisAncestor::reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisAncestor::close ()
{
    child.op->close();
}
PPIterator* PPAxisAncestor::copy(dynamic_context *_cxt_)
{
    PPAxisAncestor *res = se_new PPAxisAncestor(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPAxisAncestor::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	return true;
}

PPAxisAncestorOrSelf::PPAxisAncestorOrSelf(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
						 NodeTestData _nt_data_):PPAxisAncestor(_cxt_, _child_, _nt_type_, _nt_data_,true)
{
 
}

void PPAxisAncestor::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
		if (!self) cur = get_parent_node(cur);
		if (cur!=XNULL)
		{
			CHECKP(cur);
			if (GETSCHEMENODEX(cur)->type==virtual_root)
				cur=XNULL;
		}
    }

    t.copy(tuple_cell::node(cur));
    cur = get_parent_node(cur);
}
void PPAxisAncestor::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAncestor::next_string");
}
void PPAxisAncestor::next_qname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (!self) cur = get_parent_node(cur);		
        while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_qname_type(GETSCHEMENODEX(cur),
                                nt_data.uri,
                                nt_data.ncname_local, 
                                element))
                break;
			cur = get_parent_node(cur);
		}
    }
    t.copy(tuple_cell::node(cur));
	cur = get_parent_node(cur);
	while (cur!=XNULL)
	{
		CHECKP(cur);		
		if (comp_qname_type(GETSCHEMENODEX(cur),
                              nt_data.uri,
                              nt_data.ncname_local, 
                              element))
							  return;
		cur = get_parent_node(cur);
	}
    
}

void PPAxisAncestor::next_wildcard_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
		if (!self) cur = get_parent_node(cur);		
		while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_type(GETSCHEMENODEX(cur), 
                        NULL,
                        NULL, 
                        element))
							  break;
			cur = get_parent_node(cur);
		}        
    }

    t.copy(tuple_cell::node(cur));
	cur = get_parent_node(cur);
    while (cur!=XNULL)
	{
		CHECKP(cur);		
		if ( comp_type(GETSCHEMENODEX(cur), 
                        NULL,
                        NULL, 
                        element))
							  return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_wildcard_ncname_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (!self) cur = get_parent_node(cur);
		
        while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_type(GETSCHEMENODEX(cur), 
                        nt_data.uri,
                        NULL, 
                        element))
							  break;
			cur = get_parent_node(cur);
		}     
    }

    t.copy(tuple_cell::node(cur));
	cur = get_parent_node(cur);
    while (cur!=XNULL)
	{
		CHECKP(cur);		
		if (comp_type(GETSCHEMENODEX(cur), 
                        nt_data.uri,
                        NULL, 
                        element))
							  return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_wildcard_star_ncname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (!self) cur = get_parent_node(cur);        
		while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname_local, 
                              element))
							  break;
			cur = get_parent_node(cur);
		}             
    }

    t.copy(tuple_cell::node(cur));
	cur = get_parent_node(cur);
    while (true)
	{
		CHECKP(cur);		
		if (comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname_local, 
                              element))
							  return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAncestor::next_function_call");
}

void PPAxisAncestor::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisAncestor::next_var_name");
}
