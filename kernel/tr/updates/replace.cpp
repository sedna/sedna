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

void replace(PPOpIn arg)
{
	//1.seq-replaced nodes
	//2.replacements
	//3. persistent replacements+their position in 2 seq
//	xptr addr(0,(void*)0x4acc0000);
//	check_blk_consistency(addr);
	xptr node, parent, tmp_node, del_node, node_child;

	tuple t(arg.ts);
	descript_sequence arg3seq(2);
	xptr_sequence arg1seq;
	xptr_sequence arg1seq_tmp;
	xptr_sequence arg2seq;
	upd_ns_map* ins_swiz=NULL;
	bool is_node_updated=true;
	arg.op->next(t);
	while (!t.is_eos())
	{
		if (t.cells[0].is_node())
		{
			node=t.cells[0].get_node();
			CHECKP(node);
			if ((!is_node_updated || is_node_persistent(node))&& !is_node_document(node)) 
			{
				xptr indir=((n_dsc*)XADDR(node))->indir;
				if (is_node_updated) 
				{
					is_node_updated=false;
					arg1seq.add(indir);	
					arg1seq_tmp.add(node);
				}
				else
				{
						
					if (is_node_persistent(node))
					{
						tuple tup(2);
						tup.copy(tuple_cell::node(node),tuple_cell((__int64)(arg2seq.size())));
						arg3seq.add(tup);
					}
					arg2seq.add(indir);
				}
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2020);
			}
#endif
		}
		else
		{
			if (t.cells[0].get_atomic_type()==se_separator)
			{
				arg2seq.add(XNULL);
				is_node_updated=true;
			}
#ifndef IGNORE_UPDATE_ERRORS
			else	throw USER_EXCEPTION(SE2021);
#endif
		}

		arg.op->next(t);
	}
	
	if (arg1seq.size()<=0) return;
	//merge 1'st and 3'rd sequences create copy of merged items
	// Checking authorization
	if (is_auth_check_needed(REPLACE_STATEMENT)) 
		auth_for_update(&arg1seq, REPLACE_STATEMENT, false);
	arg1seq_tmp.sort();
	arg3seq.sort();
	descript_sequence::iterator it3=arg3seq.begin();
	xptr_sequence::iterator it1=arg1seq_tmp.begin();
	while(it3!=arg3seq.end()&& it1!=arg1seq_tmp.end())
	{
		switch(nid_cmp_effective((*it3).cells[0].get_node(), *it1))
		{
		case 0:
			{
				node=copy_to_temp((*it3).cells[0].get_node());
				arg2seq[(*it3).cells[1].get_xs_integer()]=((n_dsc*)XADDR(node))->indir;
				arg2seq.set(((n_dsc*)XADDR(node))->indir,(*it3).cells[1].get_xs_integer());
				++it3;//++it1;
			}
			break;
		case 1:
			++it1;
			break;
		case 2:
			++it1;
			break;
		case -1:
			++it3;
			break;
		case -2:
			{
				node=copy_to_temp((*it3).cells[0].get_node());
				arg2seq.set(((n_dsc*)XADDR(node))->indir,(*it3).cells[1].get_xs_integer());
				++it3;
			}
			break;
		}
	}
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
#ifdef SE_ENABLE_TRIGGERS
	apply_per_statement_triggers(&arg1seq, false, NULL, false, TRIGGER_BEFORE, TRIGGER_REPLACE_EVENT);
#endif    
	//sorting arg1seq
	arg3seq.clear();
	xptr_sequence::iterator it=arg1seq.begin();
	xptr_sequence::iterator sit=arg2seq.begin();
	int ctr=0;
	do
	{
		tuple tup(2);
		tup.copy(tuple_cell::node(removeIndirection(*it)),tuple_cell((__int64)ctr));
		arg3seq.add(tup);
		while(*sit!=XNULL)
		{
			sit++;
			ctr++;			
		}
		sit++;
		ctr++;
		it++;
	}
	while (it!=arg1seq.end());
	arg3seq.sort();
	it3=arg3seq.begin();
	descript_sequence arg4seq(2);
	do
	{
		node=(*it3).cells[0].get_node();
		CHECKP(node);
		tuple t=(*it3);
		t.cells[0].set_node(((n_dsc*)XADDR(node))->indir);
		++it3;
		arg4seq.add(t);
		
	}
	while (it3!=arg3seq.end());
	// inserting new nodes
	it3=arg4seq.end();
	do
	{
		--it3;
		node=removeIndirection((*it3).cells[0].get_node());
		
		//1.insert
		int pos=(*it3).cells[1].get_xs_integer();
		sit=arg2seq.begin()+pos;
		while(*sit!=XNULL)
		{
			node_child=*sit;
            CHECKP(node);
#ifdef SE_ENABLE_TRIGGERS
            if(apply_per_node_triggers(removeIndirection(node_child), node, XNULL, NULL, TRIGGER_BEFORE, TRIGGER_REPLACE_EVENT) == XNULL)
    			goto next_replacement;
    		CHECKP(node);
#endif
			if (is_node_attribute(node)|| is_node_attribute(removeIndirection(node_child)))
			{
				parent=removeIndirection(GETPARENTPOINTER(node));
				if (is_node_persistent(node_child)) 
					node=deep_pers_copy(XNULL, XNULL, parent, removeIndirection(node_child),true);
				else
					node=deep_temp_copy(XNULL, XNULL, parent, removeIndirection(node_child),ins_swiz);
			}
			else
			{
				if (is_node_persistent(node_child)) 
					node=deep_pers_copy(node, XNULL, XNULL, removeIndirection(node_child),true);
				else
					node=deep_temp_copy(node, XNULL, XNULL, removeIndirection(node_child),ins_swiz);
			}
			sit++;
		}

		//delete node
        del_node = removeIndirection((*it3).cells[0].get_node());
#ifdef SE_ENABLE_TRIGGERS
        tmp_node = copy_to_temp(del_node);
        parent=removeIndirection(((n_dsc*)XADDR(del_node))->pdsc);
        CHECKP(del_node);
#endif        

        delete_node(del_node);

#ifdef SE_ENABLE_TRIGGERS
        apply_per_node_triggers(XNULL, tmp_node, parent, NULL, TRIGGER_AFTER, TRIGGER_REPLACE_EVENT);
#endif
next_replacement:;    
	}
	while (it3!=arg4seq.begin());
	//3.delete
/*	arg2seq.clear();
	it=arg1seq.begin();
	while (it!=arg1seq.end())
	{
		arg2seq.add(removeIndirection(*it));
		++it;
	}
	arg2seq.sort();
	it=arg2seq.end();
	do
	{
		--it;
		delete_node(*it);
		if (it==arg2seq.begin()) break;
	}
	while (true);
*/
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);
		delete ins_swiz;
	}
	clear_temp();
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
#ifdef SE_ENABLE_TRIGGERS
	apply_per_statement_triggers(NULL, false, NULL, false, TRIGGER_AFTER, TRIGGER_REPLACE_EVENT);
#endif 
}