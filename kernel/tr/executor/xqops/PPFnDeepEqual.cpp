/*
 * File:  PPFnDeepEqual.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "PPFnDeepEqual.h"
#include "comparison_operations.h"
#include "casting_operations.h"
#include "dm_accessors.h"
#include "node_utils.h"

PPFnDeepEqual::PPFnDeepEqual(variable_context *_cxt_,
                   PPOpIn _child1_,
                   PPOpIn _child2_) : PPIterator(_cxt_),
                                      child1(_child1_),
                                      child2(_child2_)
{
}
bool PPFnDeepEqual::are_nodes_deep_equal(xptr& node1,xptr& node2)
{
	CHECKP(node1);
	schema_node* scm1=GETSCHEMENODEX(node1);
	CHECKP(node2);
	schema_node* scm2=GETSCHEMENODEX(node2);
	if (scm1->type!=scm2->type)
		return false;
	switch(scm1->type)
	{
	case element: return are_elements_deep_equal(node1,node2);
	case document: return are_documents_deep_equal(node1,node2);
	case attribute: return are_attributes_equal(node1,node2,scm1,scm2);
	case comment:case text: return are_text_nodes_equal(node1,node2);
	case pr_ins: return are_pi_equal(node1,node2);	 
	default: return false;
	}
}
bool PPFnDeepEqual::are_elements_deep_equal(xptr& node1,xptr& node2)
{
	//1.Attributes
	xptr at1=getFirstByOrderAttributeChild(node1);
	int at_cnt=0;
	while (at1!=XNULL)
	{
		schema_node * ats=GETSCHEMENODEX(at1);
		CHECKP(node2);
		if (!isAttributePointerSet((n_dsc*)XADDR(node2),ats->name,(ats->xmlns==NULL)?NULL:ats->xmlns->uri))return false;
		at_cnt++;
		at1=getNextByOrderAttribute(at1);
	}
	at1=getFirstByOrderAttributeChild(node2);
	int at_cnt2=0;
	while (at1!=XNULL)
	{
		at_cnt2++;
		at1=getNextByOrderAttribute(at1);
	}
	if (at_cnt!=at_cnt2)return false;
	return are_documents_deep_equal(node1,node2);
}
bool PPFnDeepEqual::are_documents_deep_equal(xptr& node1,xptr& node2)
{
	xptr c1=getFirstByOrderChildNode(node1);
	xptr c2=getFirstByOrderChildNode(node2);
	while (c1!=XNULL && c2!=XNULL)
	{		
		if (!are_nodes_deep_equal(c1,c2))
			return false;
		CHECKP(c1);
		c1=((n_dsc*)XADDR(c1))->rdsc;
		CHECKP(c2);
		c2=((n_dsc*)XADDR(c2))->rdsc;
	}
	if (c1!=XNULL || c2!=XNULL)
		return false;
	else
		return true;
}
bool PPFnDeepEqual::are_attributes_equal(xptr& node1,xptr& node2,schema_node* scm1,schema_node* scm2)
{
	if ( my_strcmp(scm2->name,scm1->name)==0 &&  (((char*)scm1->xmlns)==((char*)scm2->xmlns) ||
		(scm1->xmlns!=NULL&& scm2->xmlns!=NULL && my_strcmp(scm2->xmlns->uri,scm1->xmlns->uri)==0)))
	return are_text_nodes_equal(node1,node2);
	else
		return false;
}

bool PPFnDeepEqual::are_text_nodes_equal(xptr& node1,xptr& node2)
{
	tuple_cell n1=cast_to_xs_string(dm_typed_value(node1));
	tuple_cell n2=cast_to_xs_string(dm_typed_value(node2));
	if (value_comp_eq(n1,n2).get_xs_boolean())	
		return true;
	else
		return false;
}


bool PPFnDeepEqual::are_pi_equal(xptr& node1,xptr& node2)
{
	char* n1=dm_node_name(node1).get_str_mem();
	char* n2=dm_node_name(node2).get_str_mem();
	if (my_strcmp(n1,n2)==0)	
		return are_text_nodes_equal(node1,node2);
	else
		return false;	
}


PPFnDeepEqual::~PPFnDeepEqual()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPFnDeepEqual::open  ()
{
    child1.op->open();
    child2.op->open();

    eos_reached1 = true;
	eos_reached2 = true;
}

void PPFnDeepEqual::reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    eos_reached1 = true;
	eos_reached2 = true;
}

void PPFnDeepEqual::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPFnDeepEqual::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		if (!eos_reached1) child1.op->reopen();
		if (!eos_reached2) child2.op->reopen();
		eos_reached2 = false;
		eos_reached1 = false;
		tuple cont1(child1.ts);
		tuple cont2(child2.ts);
		child1.op->next(cont1);
		child2.op->next(cont2);
		while (!cont1.is_eos() && !cont2.is_eos())
		{
			if (cont1.cells[0].is_node() && cont2.cells[0].is_node() )
			{
				xptr node1=cont1.cells[0].get_node();
				xptr node2=cont2.cells[0].get_node();
				if (!are_nodes_deep_equal(node1,node2))
				{
					t.copy(tuple_cell::atomic(false));
					return;
				}
			}
			else if (!cont1.cells[0].is_node() && !cont2.cells[0].is_node() )
			{
				if (!value_comp_eq(cont1.cells[0],cont2.cells[0]).get_xs_boolean())
				{
					t.copy(tuple_cell::atomic(false));
					return;
				}
			}
			else
			{
				t.copy(tuple_cell::atomic(false));
				return;
			}

			child1.op->next(cont1);
			child2.op->next(cont2);
		}
		if (cont1.is_eos() && cont2.is_eos())
		{
			eos_reached1 = true;
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(true));
			return;
		}
		else
		{
			eos_reached1 = cont1.is_eos();
			eos_reached2 = cont2.is_eos();
			t.copy(tuple_cell::atomic(false));
			return;
		}
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnDeepEqual::copy(variable_context *_cxt_)
{
    PPFnDeepEqual *res = new PPFnDeepEqual(_cxt_, child1, child2);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);

    return res;
}

bool PPFnDeepEqual::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDeepEqual::result");
}

