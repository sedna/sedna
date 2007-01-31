/*
 * File:  other_updates.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/mo/micro.h"
#include "tr/auth/auc.h"
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif
// Rename operation
void rename(PPOpIn arg,const char* name)
{
	// Creating the first sequence (different validity tests+ indirection deref)
	tuple t(arg.ts);
	xptr_sequence argseq;
	arg.op->next(t);
	while (!t.is_eos())
	{
		if (t.cells[0].is_node())
		{
			xptr node=t.cells[0].get_node();
			CHECKP(node);
			if (is_node_persistent(node)&& (is_node_element(node)||is_node_attribute(node)) ) 
			{
				//xptr indir=((n_dsc*)XADDR(node))->indir;
				argseq.add(node);	
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2020);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2021);
			}
#endif
		arg.op->next(t);
	}
	
	if (argseq.size()<=0) return;
	// Checking authorization
	if (is_auth_check_needed(RENAME_STATEMENT)) 
		auth_for_update(&argseq, RENAME_STATEMENT, false);
	//Sort in document order
	argseq.sort();
	//changing to indirection
	xptr_sequence::iterator it=argseq.begin();
    xptr res;
	while (it!=argseq.end())
	{
		xptr node=*it;
		CHECKP(node);
		argseq.set(((n_dsc*)XADDR(node))->indir,it);
		++it;
	}
	it=argseq.end();
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
	do
	{
		--it;
		xptr indir=*it;
		xptr node=removeIndirection(indir);
		CHECKP(node);
		t_item type=GETTYPE((GETBLOCKBYNODE(node))->snode);
		n_dsc* desc=(n_dsc*)XADDR(node);
		xptr left=node;
		xptr parent=removeIndirection(desc->pdsc);
		CHECKP(node);
#ifdef SE_ENABLE_TRIGGERS
        if (apply_per_node_triggers(XNULL, XNULL, parent, TRIGGER_BEFORE, TRIGGER_INSERT_EVENT, name, type) == XNULL)
    		return;
        if (apply_per_node_triggers(XNULL, node, parent, TRIGGER_BEFORE, TRIGGER_DELETE_EVENT) == XNULL)
    		return;
#endif
        
		switch(type)
		{
		case attribute:
			{
				//1. insert
				int size=((a_dsc*)desc)->size;
				if (size>0)
				{
					char *z=new char[size];
					xptr ind_ptr=((a_dsc*)desc)->data;
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					memcpy(z,data,size);
					insert_attribute(left, XNULL, parent,name, ((a_dsc*)desc)->type,z,size,NULL);
					delete z;
				}
				else
					insert_attribute(left, XNULL, parent,name, ((a_dsc*)desc)->type,NULL,0,NULL);
				//2. delete
				CHECKP(indir);
				delete_node(*((xptr*)XADDR(indir)));
				break;
			}
		case element:
			{
				//1.INSERT
				res=insert_element(left, XNULL, parent,name,((e_dsc*)desc)->type,NULL);
				CHECKP(indir);
				copy_content(res,*((xptr*)XADDR(indir)),XNULL);
				//2.DELETE
				CHECKP(indir);
				delete_node(*((xptr*)XADDR(indir)));
				break;
			}
		}
#ifdef SE_ENABLE_TRIGGERS
        apply_per_node_triggers(res, XNULL, parent, TRIGGER_AFTER, TRIGGER_INSERT_EVENT);
#endif
		if (it==argseq.begin()) break;
	}
	while (true);
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
}