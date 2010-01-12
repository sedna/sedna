/*
 * File:  PPAxisAncestor.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisAncestor.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/merge.h"
#include "tr/executor/base/visitor/PPVisitor.h"

void PPAxisAncestor::init_function()
{
    NodeTestType type = nt_type;
    
	if (type == node_test_element) 
        type = (nt_data.ncname_local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
        case node_test_processing_instruction   : next_fun = &PPAxisAncestor::next_processing_instruction; break;
        case node_test_comment                  : next_fun = &PPAxisAncestor::next_comment; break;
        case node_test_text                     : next_fun = &PPAxisAncestor::next_text; break;
        case node_test_node                     : next_fun = &PPAxisAncestor::next_node; break;
        case node_test_qname                    : next_fun = &PPAxisAncestor::next_qname; break;
        case node_test_wildcard_star            : next_fun = &PPAxisAncestor::next_wildcard_star; break;
        case node_test_wildcard_ncname_star     : next_fun = &PPAxisAncestor::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname     : next_fun = &PPAxisAncestor::next_wildcard_star_ncname; break;
        case node_test_attribute                : next_fun = &PPAxisAncestor::next_attribute; break;
        case node_test_document                 : next_fun = &PPAxisAncestor::next_document; break;
        default                                 : throw USER_EXCEPTION2(SE1003, "PPAxisAncestor: unexpected node test");
    }
}
PPAxisAncestor::PPAxisAncestor(dynamic_context *_cxt_,
                               operation_info _info_, 
                               PPOpIn _child_,
                               NodeTestType _nt_type_,
                               NodeTestData _nt_data_) : PPIterator(_cxt_, _info_),
                                                         child(_child_),
                                                         nt_type(_nt_type_),
                                                         nt_data(_nt_data_)
{
	self=false; 
	init_function();
}
PPAxisAncestor::PPAxisAncestor(dynamic_context *_cxt_,
                               operation_info _info_, 
                               PPOpIn _child_,
                               NodeTestType _nt_type_,
                               NodeTestData _nt_data_,
                               bool _self_) : PPIterator(_cxt_, _info_),
                                              child(_child_),
                                              nt_type(_nt_type_),
                                              nt_data(_nt_data_),
                                              self(_self_)
{
    init_function();
}

PPAxisAncestorOrSelf::PPAxisAncestorOrSelf(dynamic_context *_cxt_,
                                           operation_info _info_, 
                                           PPOpIn _child_,
                                           NodeTestType _nt_type_,
                                           NodeTestData _nt_data_) : 
    PPAxisAncestor(_cxt_, _info_, _child_, _nt_type_, _nt_data_, true)
{
}



PPAxisAncestor::~PPAxisAncestor()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisAncestor::do_open ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisAncestor::do_reopen()
{
    child.op->reopen();

    cur = XNULL;
}

void PPAxisAncestor::do_close()
{
    child.op->close();
}

PPIterator* PPAxisAncestor::do_copy(dynamic_context *_cxt_)
{
    PPAxisAncestor *res = self ? se_new PPAxisAncestor(_cxt_, info, child, nt_type, nt_data, true) :
                                 se_new PPAxisAncestor(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPAxisAncestor::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


void PPAxisAncestorOrSelf::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

void PPAxisAncestor::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisAncestor::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

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
	if (cur!=XNULL)
	{
		CHECKP(cur);
		if (GETSCHEMENODEX(cur)->type==virtual_root)
			cur=XNULL;
	}
}

void PPAxisAncestor::next_qname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

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
                              element)) return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

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
                        element)) return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_wildcard_ncname_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (!self) cur = get_parent_node(cur);
		
        while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_type(GETSCHEMENODEX(cur), 
                        nt_data.uri,
                        NULL, 
                        element)) break;
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
                        element)) return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_wildcard_star_ncname(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (!self) cur = get_parent_node(cur);        
		while (cur!=XNULL)
		{
			CHECKP(cur);
			if (comp_local_type(GETSCHEMENODEX(cur),
                              NULL,
                              nt_data.ncname_local, 
                              element)) break;
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
                              element)) return;
		cur = get_parent_node(cur);
	}
}

void PPAxisAncestor::next_attribute(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        if(self) 
        {
            /* Works just like self::attribute() in this case! */
            xptr node = child.get(t).get_node();
            if (node!=XNULL)
            {
                CHECKP(node);
                schema_node_cptr scm = GETSCHEMENODEX(node);
                t_item type = scm->type;

                if (type != attribute) continue;

                if (nt_data.ncname_local == NULL || 
                    comp_qname_type(scm, nt_data.uri, nt_data.ncname_local, attribute)) return;
            }
        }
    }
}

void PPAxisAncestor::next_document(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        
        xptr node = child.get(t).get_node();
        cur = getRoot(node);

        U_ASSERT(cur != XNULL);
        CHECKP(cur);
        
        if(GETSCHEMENODEX(cur)->type != document ||
           (!self && cur == node)) 
        {
            cur = XNULL;
        }
        else if(nt_data.ncname_local != NULL) 
        {
            RelChildAxisMerge merge;
            xptr desc = merge.init(cur,
                                   nt_data.uri,
                                   nt_data.ncname_local,
                                   element,
                                   comp_qname_type);

            if(desc == XNULL || merge.next(desc) != XNULL) cur = XNULL;
        }
    }
    
    t.copy(tuple_cell::node(cur));
    cur = XNULL;
}
