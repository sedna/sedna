/*
 * File:  xptrChanneledMerge.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/base/xptrChanneledMerge.h"

node_cell* node_cell::init(xptr node)
{
	node_cell* nc=(node_cell*)scm_malloc(sizeof(node_cell),false);
	nc->node=node;
	return nc;
}
xptr xptrChanneledMerge::getNextNode()
{
	if (!top)
	{
		if (forward)
			top=merge_tree->rb_minimum(merge_tree->root);
		else
			top=merge_tree->rb_maximum(merge_tree->root);
		if(!top) return XNULL;
	}
	xptr tmp=(nodeFN)(top->obj->node);
	xptr res=top->obj->node;
	if (tmp!=XNULL)
	{
		pers_sset<node_cell,unsigned short>::pers_sset_entry* nxt=(forward)?merge_tree->rb_successor(top):merge_tree->rb_predecessor(top);
		if (nxt)
		{
			tmp_cell->node=tmp;
			if ((forward && tmp_cell->less(nxt->obj))||(!forward&&nxt->obj->less(tmp_cell)))
			{
				top->obj->node=tmp;	
			}
			else
			{
				node_cell* nc=top->obj;
				nc->node=tmp;
				merge_tree->rb_delete(top);
				merge_tree->put(nc);
				top=nxt;//(following)?merge_tree->rb_minimum(merge_tree->root):merge_tree->rb_maximum(merge_tree->root);				
			}
		}
		else
		{			
			top->obj->node=tmp;		
		}			
	}
	else
	{			
		pers_sset<node_cell,unsigned short>::pers_sset_entry* nxt=(forward)?merge_tree->rb_successor(top):merge_tree->rb_predecessor(top);
		merge_tree->rb_delete(top);
		top=nxt;
		//top=(following)?merge_tree->rb_minimum(merge_tree->root):merge_tree->rb_maximum(merge_tree->root);
	}
	return res;
}
xptrChanneledMerge::xptrChanneledMerge(next_node_fn _nodeFN_, bool _forward_):nodeFN(_nodeFN_), forward(_forward_)
{
	merge_tree=pers_sset<node_cell,unsigned short>::init(false);
	tmp_cell=node_cell::init(XNULL);
	top=NULL;
}
xptrChanneledMerge::~xptrChanneledMerge()
{
	clear_merge ();
	pers_sset<node_cell,unsigned short>::free(merge_tree);
	scm_free(tmp_cell,false);
}
void xptrChanneledMerge::clear_merge  ()
{
	pers_sset<node_cell,unsigned short>::pers_sset_entry* tmp=merge_tree->rb_minimum(merge_tree->root);
	while (tmp!=NULL)
	{		
		node_cell* mdc=tmp->obj;
		scm_free(tmp->obj,false);
		tmp->obj=NULL;
		tmp=merge_tree->rb_successor(tmp);
	}	
	top=NULL;
	merge_tree->clear();
}
void xptrChanneledMerge::addChannel(xptr node)
{
	if (node!=XNULL) merge_tree->put(node_cell::init(node));
}