#include "PPAxisAncestor.h"
#include "node_utils.h"
#include "PPUtils.h"
#include "dm_accessors.h"
using namespace tr_globals;
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
PPAxisAncestor::PPAxisAncestor(variable_context *_cxt_, 
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
PPAxisAncestor::PPAxisAncestor(variable_context *_cxt_, 
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
PPIterator* PPAxisAncestor::copy(variable_context *_cxt_)
{
    PPAxisAncestor *res = new PPAxisAncestor(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPAxisAncestor::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	return true;
}

PPAxisAncestorOrSelf::PPAxisAncestorOrSelf(variable_context *_cxt_, 
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
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
    }
}

void PPAxisAncestor::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
    }
}

void PPAxisAncestor::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
    }
}

void PPAxisAncestor::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);

        cur = child.get(t).get_node();
		cur = get_parent_node(cur);
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

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        CHECKP(cur);
        while (cur!=XNULL)
		{
			if (comp_qname_type(GETSCHEMENODEX(cur),
                              tr_globals::st_ct.get_uri_by_prefix(nt_data.qname.Prefix, element),
                              nt_data.qname.LocalPart.n, 
                              element))
							  break;
			cur = get_parent_node(cur);
		}
    }
    t.copy(tuple_cell::node(cur));
	while (true)
	{
		cur = get_parent_node(cur);
		if (cur == XNULL ||
              comp_qname_type(GETSCHEMENODEX(cur),
                              tr_globals::st_ct.get_uri_by_prefix(nt_data.qname.Prefix, element),
                              nt_data.qname.LocalPart.n, 
                              element))
							  return;
	}
    
}

void PPAxisAncestor::next_wildcard_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);

        cur = child.get(t).get_node();
		cur = get_parent_node(cur);
        CHECKP(cur);
		while (cur!=XNULL)
		{
			if (comp_type(GETSCHEMENODEX(cur), 
                        NULL,
                        NULL, 
                        element))
							  break;
			cur = get_parent_node(cur);
		}        
    }

    t.copy(tuple_cell::node(cur));
    while (true)
	{
		cur = get_parent_node(cur);
		if (cur == XNULL ||
              comp_type(GETSCHEMENODEX(cur), 
                        NULL,
                        NULL, 
                        element))
							  return;
	}
}

void PPAxisAncestor::next_wildcard_ncname_star(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        CHECKP(cur);
		char* uri=st_ct.get_uri_by_prefix(nt_data.ncname,element);
        while (cur!=XNULL)
		{
			if (comp_type(GETSCHEMENODEX(cur), 
                        uri,
                        NULL, 
                        element))
							  break;
			cur = get_parent_node(cur);
		}     
    }

    t.copy(tuple_cell::node(cur));
	char* uri=st_ct.get_uri_by_prefix(nt_data.ncname,element);
    while (true)
	{
		cur = get_parent_node(cur);
		if (cur == XNULL ||
              comp_type(GETSCHEMENODEX(cur), 
                        uri,
                        NULL, 
                        element))
							  return;
	}
}

void PPAxisAncestor::next_wildcard_star_ncname(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);

        cur = child.get(t).get_node();
        cur = get_parent_node(cur);
        CHECKP(cur);
		while (cur!=XNULL)
		{
			if (comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname.n, 
                              element))
							  break;
			cur = get_parent_node(cur);
		}             
    }

    t.copy(tuple_cell::node(cur));
    while (true)
	{
		cur = get_parent_node(cur);
		if (cur == XNULL ||
              comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname.n, 
                              element))
							  return;
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
