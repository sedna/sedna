                                                                         /*
 * File:  insertions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "updates.h"
#include "xptr_sequence.h"
#include "micro.h"
#include "auc.h"
/* third insert procedure: insert before
* arg1- is operation that returns a sequence of the updated nodes
* arg2- is operation that returns a sequence of the inserted nodes
*/
void insert_before(PPOpIn arg2, PPOpIn arg1)
{
// Creating the first sequence (different validity tests+ indirection deref)
	tuple t(arg1.ts);
	xptr_sequence arg1seq;
	tuple t2(arg2.ts);
	xptr_sequence arg2seq;
	arg1.op->next(t);
	while (!t.is_eos())
	{
		if (t.cells[0].is_node())
		{
			xptr node=t.cells[0].get_node();
			CHECKP(node);
			if (is_node_persistent(node) )
			{
				xptr indir=((n_dsc*)XADDR(node))->indir;
				arg1seq.add(indir);	
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2013);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2013);
			}
#endif
	  arg1.op->next(t);
	}
	if (arg1seq.size()<=0) return;
	// Checking authorization
	if (is_auth_check_needed(INSERT_STATEMENT)) 
		auth_for_update(&arg1seq, INSERT_STATEMENT, false);
	// Creating the second sequence (different validity tests+ indirection deref)
	t_item prev_item=attribute;
	arg2.op->next(t2);
	while (!t2.is_eos())
	{
		if (t2.cells[0].is_node())
		{
			xptr node=t2.cells[0].get_node();
			CHECKP(node);
			if (!(is_node_document(node) || (is_node_attribute(node) && (prev_item!=attribute && prev_item!=xml_namespace))))
			{
				prev_item=GETTYPE(GETSCHEMENODEX(node));
				if (is_node_persistent(node))
				{
					xptr indir=((n_dsc*)XADDR(node))->indir;
					arg2seq.add(indir);	
				}
				else
				{
					//deep constructing of node
					arg2seq.add(node);
				}
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2014) ;
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2015);
			}
#endif
	arg2.op->next(t2);
	}

	if (arg2seq.size()<=0) return;
	// outer cycle on first sequence
	xptr_sequence::iterator it1=arg1seq.begin();
	upd_ns_map* ins_swiz=NULL;
	xptr_sequence::iterator it2;
	xptr node_child=XNULL;
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
	do
	{
		bool mark;
		xptr node_par=*it1;	
		it2=arg2seq.begin();
		node_child=*it2;
		//Check of the following is right
		xptr tmp=removeIndirection(node_par);
		xptr right=tmp;
		mark= is_node_persistent(node_child); 
		if (mark) node_child=removeIndirection(node_child);
		CHECKP(tmp);
		tmp=GETLEFTPOINTER(tmp);
		xptr child=tmp;
		if (tmp!=XNULL)
		{
			CHECKP(tmp);
			mark=!is_node_attribute(tmp);
			CHECKP(node_child);
			mark=mark && !is_node_attribute(node_child);
			if (!mark)
			{ 
				CHECKP(tmp);
				mark= is_node_attribute(tmp);
				xptr rght=GETRIGHTPOINTER(tmp);
				if (rght!=XNULL)
				{
					CHECKP(rght);
					mark=mark && !is_node_attribute(rght);
				}
				if (!mark)
				{
					CHECKP(tmp);
					mark= is_node_attribute(tmp);
					xptr last=*(arg2seq.end()-1);
					if (is_node_persistent(last))last=removeIndirection(last); 
					CHECKP(last);
					mark=mark && is_node_attribute(last);
					if (!mark)
					{
#ifndef IGNORE_UPDATE_ERRORS
						throw USER_EXCEPTION(SE2016);
#endif		
						goto cycle1;
					}
				}
			}
		}
		do
		{
			node_child=*it2;
			mark= is_node_persistent(node_child); 
			if (mark) 
			{
				node_child=removeIndirection(node_child);
				// checking that item from the second sequence is not a parent of the item from the first sequence
				if (nid_ancestor(node_child,removeIndirection(node_par)))
				{
#ifndef IGNORE_UPDATE_ERRORS
					throw USER_EXCEPTION(SE2017);
#endif				
					goto cycle2;
				}
			}
			CHECKP(node_child);
			if (child==XNULL)
			{
				if (mark) 
					node_child=deep_pers_copy( XNULL,right, XNULL, node_child,true);
				else
					node_child=deep_temp_copy( XNULL,right, XNULL, node_child,ins_swiz);
			}
			else
			{
				if (mark) 
					node_child=deep_pers_copy(child, XNULL, XNULL, node_child,true);
				else
					node_child=deep_temp_copy(child, XNULL, XNULL, node_child,ins_swiz);
			}
			
			child=node_child;
			// inner cycle on second sequence
cycle2:	
			it2++;
		}
		while (it2!=arg2seq.end());
cycle1:
		it1++;
	}
	while (it1!=arg1seq.end());
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);
		delete ins_swiz;
	}
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
}
/* second insert procedure: insert following
* arg1- is operation that returns a sequence of the updated nodes
* arg2- is operation that returns a sequence of the inserted nodes
*/
void insert_following(PPOpIn arg2, PPOpIn arg1)
{
// Creating the first sequence (different validity tests+ indirection deref)
	tuple t(arg1.ts);
	xptr_sequence arg1seq;
	tuple t2(arg2.ts);
	xptr_sequence arg2seq;
	arg1.op->next(t);
	while (!t.is_eos())
	{
		if (t.cells[0].is_node())
		{
			xptr node=t.cells[0].get_node();
			CHECKP(node);
			if (is_node_persistent(node) )
			{
				xptr indir=((n_dsc*)XADDR(node))->indir;
				arg1seq.add(indir);	
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				USER_EXCEPTION(SE2013);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				USER_EXCEPTION(SE2013);
			}
#endif
	  arg1.op->next(t);
	}
	if (arg1seq.size()<=0) return;
	// Checking authorization
	if (is_auth_check_needed(INSERT_STATEMENT)) 
		auth_for_update(&arg1seq, INSERT_STATEMENT, false);
	
	// Creating the second sequence (different validity tests+ indirection deref)
	t_item prev_item=attribute;
	arg2.op->next(t2);
	while (!t2.is_eos())
	{
		if (t2.cells[0].is_node())
		{
			xptr node=t2.cells[0].get_node();
			CHECKP(node);
			if (!(is_node_document(node)|| (is_node_attribute(node) && prev_item!=attribute)))
			{
				prev_item=GETTYPE(GETSCHEMENODEX(node));
				if (is_node_persistent(node))
				{
					xptr indir=((n_dsc*)XADDR(node))->indir;
					arg2seq.add(indir);	
				}
				else
				{
					//deep constructing of node
					arg2seq.add(node);
				}
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				USER_EXCEPTION(SE2014);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				USER_EXCEPTION(SE2013);
			}
#endif
	arg2.op->next(t2);
	}

	if (arg2seq.size()<=0) return;
	// outer cycle on first sequence
	xptr_sequence::iterator it1=arg1seq.begin();
	xptr_sequence::iterator it2;
	upd_ns_map* ins_swiz=NULL;
	xptr node_child=XNULL;
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
	do
	{
		bool mark;
		xptr node_par=*it1;	
		it2=arg2seq.begin();
		node_child=*it2;
		//Check of the following is right
		xptr tmp=removeIndirection(node_par);
		xptr child=tmp;
		mark= is_node_persistent(node_child); 
		if (mark) node_child=removeIndirection(node_child);
		CHECKP(tmp);
		mark=!is_node_attribute(tmp);
		CHECKP(node_child);
		mark=mark && !is_node_attribute(node_child);
		if (!mark)
		{ 
			CHECKP(tmp);
			mark= is_node_attribute(tmp);
			xptr rght=GETRIGHTPOINTER(tmp);
			if (rght!=XNULL)
			{
				CHECKP(rght);
				mark=mark && !is_node_attribute(rght);
			}
			if (!mark)
			{
				CHECKP(tmp);
				mark= is_node_attribute(tmp);
				xptr last=*(arg2seq.end()-1);
				if (is_node_persistent(last))last=removeIndirection(last); 
				CHECKP(last);
				mark=mark && is_node_attribute(last);
				if (!mark)
				{
#ifndef IGNORE_UPDATE_ERRORS
					USER_EXCEPTION(SE2013);
#endif		
					goto cycle1;
				}
			}
		}
		do
		{
			node_child=*it2;
			mark= is_node_persistent(node_child); 
			if (mark) 
			{
				node_child=removeIndirection(node_child);
				// checking that item from the second sequence is not a parent of the item from the first sequence
				if (nid_ancestor(node_child,removeIndirection(node_par)))
				{
#ifndef IGNORE_UPDATE_ERRORS
					throw USER_EXCEPTION(SE2017);
#endif				
					goto cycle2;
				}
			}
			CHECKP(node_child);
			if (mark) 
				node_child=deep_pers_copy(child, XNULL, XNULL, node_child,true);
			else
				node_child=deep_temp_copy(child, XNULL, XNULL, node_child,ins_swiz);
			
			child=node_child;
			// inner cycle on second sequence
cycle2:	
			it2++;
		}
		while (it2!=arg2seq.end());
cycle1:
		it1++;
	}
	while (it1!=arg1seq.end());
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);
		delete ins_swiz;
	}
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
}
/* first insert procedure: insert to
* arg1- is operation that returns a sequence of the updated nodes
* arg2- is operation that returns a sequence of the inserted nodes
*/
void insert_to(PPOpIn arg2, PPOpIn arg1)
{
	// Creating the first sequence (different validity tests+ indirection deref)
	tuple t(arg1.ts);
	xptr_sequence arg1seq;
	tuple t2(arg2.ts);
	xptr_sequence arg2seq;
	//bool ch_auth=is_auth_check_needed(INSERT_STATEMENT);
	arg1.op->next(t);
	while (!t.is_eos())
	{
		if (t.cells[0].is_node())
		{
			xptr node=t.cells[0].get_node();
			CHECKP(node);
			if (is_node_persistent(node) && (is_node_element(node)||is_node_document(node)))
			{
				/*if (ch_auth)
					auth_for_update( node, INSERT_STATEMENT);*/
				xptr indir=((n_dsc*)XADDR(node))->indir;
				arg1seq.add(indir);	
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2018);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2019);
			}
#endif
		arg1.op->next(t);
	}
	if (arg1seq.size()<=0) return;
	
	// Checking authorization
	if (is_auth_check_needed(INSERT_STATEMENT)) 
		auth_for_update(&arg1seq, INSERT_STATEMENT, false);
	
	// Creating the second sequence (different validity tests+ indirection deref)
	t_item prev_item=attribute;
	arg2.op->next(t2);
	while (!t2.is_eos())
	{
		if (t2.cells[0].is_node())
		{
			xptr node=t2.cells[0].get_node();
			CHECKP(node);
			if (!(is_node_document(node)||( is_node_attribute(node) && prev_item!=attribute)))
			{
				prev_item=GETTYPE(GETSCHEMENODEX(node));
				if (is_node_persistent(node))
				{
					xptr indir=((n_dsc*)XADDR(node))->indir;
					arg2seq.add(indir);	
				}
				else
				{
					//deep constructing of node
					arg2seq.add(node);
				}
			}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2014);
			}
#endif
		}
#ifndef IGNORE_UPDATE_ERRORS
			else
			{
				throw USER_EXCEPTION(SE2015);
			}
#endif
		arg2.op->next(t2);
	}
	
	// outer cycle on first sequence
	if (arg2seq.size()<=0 ) return;
	xptr_sequence::iterator it1=arg1seq.begin();
	xptr_sequence::iterator it2;
	upd_ns_map* ins_swiz=NULL;
	xptr node_child=XNULL;
#ifdef SE_ENABLE_FTSEARCH
	clear_ft_sequences();
#endif
	do
	{
		bool mark;
		xptr node_par=*it1;	
		xptr prev_child=XNULL;
		t_item prev_item=xml_namespace;
		it2=arg2seq.begin();
		do
		{
			node_child=*it2;
			mark= is_node_persistent(node_child); 
			if (mark) 
			{
				node_child=removeIndirection(node_child);
				// checking that item from the second sequence is not a parent of the item from the first sequence
				if (nid_ancestor(node_child,removeIndirection(node_par)))
				{
#ifndef IGNORE_UPDATE_ERRORS
					throw USER_EXCEPTION(SE2017);
#endif
					goto cycle2;
				}
			}
			CHECKP(node_child);
			if (prev_item==xml_namespace && ! is_node_xml_namespace(node_child)) 
			{
				prev_child=XNULL;
				prev_item=get_node_type(node_child);
			}
			else
			if (prev_item==attribute && ! is_node_attribute(node_child)) 
			{
				prev_child=XNULL;
				prev_item=element;
			}
			
			if (mark) 
				prev_child=deep_pers_copy(prev_child, XNULL, removeIndirection(node_par), node_child,true);
			else
				prev_child=deep_temp_copy(prev_child, XNULL, removeIndirection(node_par), node_child,ins_swiz);

			// inner cycle on second sequence
	cycle2:		
			it2++;
		}
		while (it2!=arg2seq.end());
	cycle1:
		it1++;
	}
while (it1!=arg1seq.end());
	if (ins_swiz!=NULL) 
	{
//		checkSwiizleTab(ins_swiz);	
		delete ins_swiz;
	}
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
}
