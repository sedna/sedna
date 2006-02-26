/*
 * File:  other_updates.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "updates.h"
#include "xptr_sequence.h"
#include "micro.h"
#include "auc.h"

void replace(PPOpIn arg)
{
	//1.seq-replaced nodes
	//2.replacements
	//3. persistent replacements+their position in 2 seq
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
			xptr node=t.cells[0].get_node();
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
						tup.copy(tuple_cell::node(node),tuple_cell(arg2seq.size()));
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
				xptr node=copy_to_temp((*it3).cells[0].get_node());
				arg2seq[(*it3).cells[1].get_xs_integer()]=((n_dsc*)XADDR(node))->indir;
				++it3;++it1;
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
				xptr node=copy_to_temp((*it3).cells[0].get_node());
				arg2seq.set(((n_dsc*)XADDR(node))->indir,(*it3).cells[1].get_xs_integer());
				++it3;
			}
			break;
		}
	}
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
	// inserting new nodes
	xptr_sequence::iterator sit=arg2seq.begin();
	xptr_sequence::iterator it=arg1seq.begin();
	do
	{
		xptr node=removeIndirection(*it);
		//1.insert
		while(*sit!=XNULL)
		{
			xptr node_child=*sit;
			if (is_node_persistent(node_child)) 
				node=deep_pers_copy(node, XNULL, XNULL, removeIndirection(node_child),true);
			else
				node=deep_temp_copy(node, XNULL, XNULL, removeIndirection(node_child),ins_swiz);
			
			sit++;
		}
		sit++;
		++it;
	}
	while (it!=arg1seq.end());
	//3.delete
	arg2seq.clear();
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
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);
		delete ins_swiz;
	}
	clear_temp();
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
	
}