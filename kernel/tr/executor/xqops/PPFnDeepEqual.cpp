/*
 * File:  PPFnDeepEqual.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/xqops/PPFnDeepEqual.h"
#include "tr/executor/fo/comparison_operations.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"

PPFnDeepEqual::PPFnDeepEqual(dynamic_context *_cxt_,
                   PPOpIn _child1_,
                   PPOpIn _child2_) : PPIterator(_cxt_),
                                      child1(_child1_),
                                      child2(_child2_)
{
}
PPFnDeepEqual::PPFnDeepEqual(dynamic_context *_cxt_,
                             PPOpIn _child1_,
                             PPOpIn _child2_,
							 PPOpIn _collation_) : PPIterator(_cxt_),
                                                   child1(_child1_),
                                                   child2(_child2_),
									               collation(_collation_)
{
}
bool PPFnDeepEqual::are_nodes_deep_equal(xptr& node1,xptr& node2)
{
	CHECKP(node1);
	schema_node_cptr scm1=GETSCHEMENODEX(node1);
	CHECKP(node2);
	schema_node_cptr scm2=GETSCHEMENODEX(node2);
	if (scm1->type!=scm2->type)
		return false;
	switch(scm1->type)
	{
        case element: return are_elements_deep_equal(node1,node2,scm1,scm2);
        case document: return are_documents_deep_equal(node1,node2);
        case attribute: return are_attributes_equal(node1,node2,scm1,scm2);
        case comment:case text: return are_text_nodes_equal(node1,node2);
        case pr_ins: return are_pi_equal(node1,node2);	 
        default: return false;
	}
}

static inline bool compare_full_schema_names(schema_node_cptr scm1, schema_node_cptr scm2)
{
    if ( my_strcmp(scm2->name,scm1->name) == 0 && 
		((scm1->get_xmlns()) == (scm2->get_xmlns()) ||
		(scm1->get_xmlns() != NULL && 
         scm2->get_xmlns() != NULL && 
         my_strcmp(scm2->get_xmlns()->uri, scm1->get_xmlns()->uri)==0)))
        return true;
	else 
		return false;
}

bool PPFnDeepEqual::are_elements_deep_equal(xptr& node1,xptr& node2,schema_node_cptr scm1,schema_node_cptr scm2)
{
	//1. Compare names
	if(!compare_full_schema_names(scm1, scm2)) return false;
			
	//2. Compare attributes
	xptr at1=getFirstByOrderAttributeChild(node1);
	int at_cnt=0;
	while (at1!=XNULL)
	{
		schema_node_cptr ats=GETSCHEMENODEX(at1);
		CHECKP(node2);
		xptr nd=isAttributePointerSet((n_dsc*)XADDR(node2),ats->name,(ats->get_xmlns()==NULL)?NULL:ats->get_xmlns()->uri);
		if (nd==XNULL)return false;
		CHECKP(nd);
		if (!are_attributes_equal(at1,nd,ats,GETSCHEMENODEX(nd))) return false;
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
	xptr c1 = getFirstByOrderChildNode(node1);
	xptr c2 = getFirstByOrderChildNode(node2);
	schema_node_cptr scm = XNULL;

	while (true)
	{		
		while (c1 != XNULL) 
		{ 
			CHECKP(c1);
	        scm = GETSCHEMENODEX(c1);
		    if(scm->type == pr_ins || scm->type == comment) c1 = ((n_dsc*)XADDR(c1))->rdsc;
			else break; 
		}
		
		while (c2 != XNULL) 
		{ 
			CHECKP(c2);
			scm = GETSCHEMENODEX(c2);
		    if(scm->type == pr_ins || scm->type == comment) c2 = ((n_dsc*)XADDR(c2))->rdsc;
			else break; 
		}
		
		if(c1 == XNULL || c2 == XNULL) 	  return c1 == c2;
		if (!are_nodes_deep_equal(c1,c2)) return false;
		
		CHECKP(c1);
		c1 = ((n_dsc*)XADDR(c1))->rdsc;
		CHECKP(c2);
		c2 = ((n_dsc*)XADDR(c2))->rdsc; 
	}
}

bool PPFnDeepEqual::are_attributes_equal(xptr& node1,xptr& node2,schema_node_cptr scm1,schema_node_cptr scm2)
{
	if (compare_full_schema_names(scm1, scm2))
	    return are_text_nodes_equal(node1,node2);
	else
		return false;
}


bool PPFnDeepEqual::are_text_nodes_equal(xptr& node1,xptr& node2)
{
	tuple_cell n1=cast_primitive_to_xs_string(dm_typed_value(node1));
	tuple_cell n2=cast_primitive_to_xs_string(dm_typed_value(node2));
	if (op_eq(n1,n2,handler).get_xs_boolean())	
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
	if (collation.op)
    {
        delete collation.op;
        collation.op = NULL;
    }
}

void PPFnDeepEqual::open  ()
{
    child1.op->open();
    child2.op->open();
	if (collation.op)
        collation.op->reopen();
    eos_reached1 = true;
	eos_reached2 = true;
	handler = NULL;
}

void PPFnDeepEqual::reopen()
{
    child1.op->reopen();
    child2.op->reopen();
	if (collation.op)
        collation.op->reopen();
    eos_reached1 = true;
	eos_reached2 = true;
	handler = NULL;
}

void PPFnDeepEqual::close ()
{
    child1.op->close();
    child2.op->close();
	if (collation.op)
        collation.op->close();
	handler = NULL;
}

void PPFnDeepEqual::next  (tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (!handler)
    {
        handler = charset_handler->get_unicode_codepoint_collation();
		if (collation.op)
        {
            collation.op->next(t);
            if(t.is_eos()) 
				throw XQUERY_EXCEPTION2(XPTY0004, "Empty third argument is not allowed in fn:deep-equal." );

            tuple_cell col = atomize(collation.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Wrong type of the third argument value in fn:deep-equal (xs_string/derived/promotable is expected).");

            collation.op->next(t);
            if (!t.is_eos()) 
				throw XQUERY_EXCEPTION2(XPTY0004, "Wrong arity of the third argument in fn:deep-equal. Argument contains more than one item." );
            
            col = tuple_cell::make_sure_light_atomic(col);

            int res = cxt->st_cxt->get_collation(col.get_str_mem(), &handler);
            if(res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + " in fn:deep-equal.").c_str()); 
        }

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
			tuple_cell& tc1=cont1.cells[0];
			tuple_cell& tc2=cont2.cells[0];
			if (tc1.is_node() && tc2.is_node() )
			{
				xptr node1=tc1.get_node();
				xptr node2=tc2.get_node();
				if (!are_nodes_deep_equal(node1,node2))
				{
					t.copy(tuple_cell::atomic(false));
					{RESTORE_CURRENT_PP; return;}
				}
			}
			else if (!tc1.is_node() && !tc2.is_node() )
			{
				if (((tc1.get_atomic_type()==xs_double && u_is_nan(tc1.get_xs_double()))||(tc1.get_atomic_type()==xs_float && u_is_nan(tc1.get_xs_float())))&&((tc2.get_atomic_type()==xs_double && u_is_nan(tc2.get_xs_double()))||(tc2.get_atomic_type()==xs_float && u_is_nan(tc2.get_xs_float()))))
				{}
				else
				{
					try
					{
						if (!op_eq(cont1.cells[0],cont2.cells[0],handler).get_xs_boolean())
						{
							t.copy(tuple_cell::atomic(false));
							{RESTORE_CURRENT_PP; return;}
						}
					}
					catch (SednaUserException &e)
					{
						t.copy(tuple_cell::atomic(false));
						{RESTORE_CURRENT_PP; return;}
					}
				}
			}
			else
			{
				t.copy(tuple_cell::atomic(false));
				{RESTORE_CURRENT_PP; return;}
			}

			child1.op->next(cont1);
			child2.op->next(cont2);
		}
		if (cont1.is_eos() && cont2.is_eos())
		{
			eos_reached1 = true;
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(true));
			{RESTORE_CURRENT_PP; return;}
		}
		else
		{
			eos_reached1 = cont1.is_eos();
			eos_reached2 = cont2.is_eos();
			t.copy(tuple_cell::atomic(false));
			{RESTORE_CURRENT_PP; return;}
		}
    }
    else 
    {
        handler=NULL;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnDeepEqual::copy(dynamic_context *_cxt_)
{
    PPFnDeepEqual *res = collation.op ? 
		                 se_new PPFnDeepEqual(_cxt_, child1, child2, collation) : 
	                     se_new PPFnDeepEqual(_cxt_, child1, child2);
    
	res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);
    
	if(collation.op) 
		res->collation.op = collation.op->copy(_cxt_);
	
	res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnDeepEqual::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDeepEqual::result");
}
