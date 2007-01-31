/*
 * File:  xptrChanneledMerge.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _XPTRCHANELLEDMERGE_H
#define _XPTRCHANELLEDMERGE_H

#include "common/sedna.h"

#include "tr/nid/numb_scheme.h"
#include "tr/structures/pers_map.h"

struct node_cell
{
	xptr node;
	static node_cell* init(xptr node );
	inline bool less( node_cell *p1) 
	{
		return nid_cmp_effective(node,p1->node)<0;
	}
	inline bool equals( node_cell *p1) 
	{
		return node==p1->node;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		
		return (node<*(xptr*)p1);		
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return (node==*(xptr*)p1);
	}
};

typedef xptr (*next_node_fn)(xptr t);
class xptrChanneledMerge
{
private:
	next_node_fn nodeFN;
	bool forward;
	pers_sset<node_cell,unsigned short>* merge_tree;
	pers_sset<node_cell,unsigned short>::pers_sset_entry* top;
	node_cell* tmp_cell;
public:	
    xptr getNextNode();  
    xptrChanneledMerge(next_node_fn _nodeFN_, bool _forward_);
	void addChannel(xptr node);
    virtual ~xptrChanneledMerge();
    void clear_merge  ();	
};
#endif

