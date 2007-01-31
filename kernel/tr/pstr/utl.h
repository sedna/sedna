/*
 * File:  utl.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UTL_H
#define _UTL_H

#include "common/sedna.h"
#include "tr/structures/nodes.h"

/* types of items */
#define	ITEM_BAD	0
#define ITEM_PSTR	1
#define	ITEM_HOLE	2

struct sort_item {
	int					item_type;
	shft				item_shft;
	shft				sort_value;
	struct sort_item*	next_item;
	struct sort_item*	prev_item;

	sort_item() {
		item_type=ITEM_BAD;
		item_shft=0;
		sort_value=0;
		next_item=NULL;
		prev_item=NULL;
	};
};

typedef struct sort_item sort_item;

sort_item*	utl_sort_sit(xptr blk);
sort_item*	utl_sort_hh(xptr blk);
sort_item*	utl_merge(sort_item* one, sort_item* two);
void		utl_print(sort_item* head);
int			utl_cmp(const void* item1, const void* item2);

#endif