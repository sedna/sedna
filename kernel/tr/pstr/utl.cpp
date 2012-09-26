/*
 * File:  utl.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "tr/pstr/utl.h"
#include "tr/pstr/pstrblk.h"
#include "common/errdbg/d_printf.h"

using namespace std;

/* sort SIT array by values in slots (PSTR shfts) returning a sorted double-linked list */
sort_item* utl_sort_sit(xptr blk) {
	int			sit_size=0;	/* number of pstr strings actually registered in SIT */
	int			sit_array_size=(PAGE_SIZE - SITB(blk) -1)/sizeof(shft); /* the whole size of SIT array */
	sort_item*	sit_array = new sort_item[sit_array_size];
	sort_item*	it;
	sort_item*	result=NULL;
	/* fill items of sit_array with non-empty PSTR slots. No links between items are set */
	for (shft slot_shft = SITB(blk); slot_shft != PAGE_SIZE-sizeof(shft); slot_shft+=sizeof(shft)) {
		shft slot_value = *(shft*)((char*)XADDR(blk) + slot_shft + sizeof(shft)); /* the first slot is not in the SIT */
		/* SIT contains empty slots, which are value-linked by the shft value of next empty slot in the block.
		   Here we use the fact that for empty slots the shift value stored in that slot points inside SIT,
		   to filter out empty slots of the SIT */
		if (slot_value == PSTR_EMPTY_SLOT || slot_value >= (SITB(blk)+ sizeof(shft)))
			continue;
		it = sit_array+sit_size;
		it->item_type=ITEM_PSTR;
		it->item_shft=slot_shft+sizeof(shft);
		it->sort_value=slot_value;
		sit_size++;
	}
	if (sit_size) {
		qsort((void*)sit_array, sit_size, sizeof(sort_item), utl_cmp);
		/* prepare linked list of the sorted array */
		sort_item*	prev=NULL;
		sort_item*	next=NULL;
		for(int i=0; i<sit_size; i++) {
			it = &sit_array[i];
			if (i+1<sit_size)
				next = &sit_array[i+1];
			else
				next = NULL;
			it->prev_item=prev;
			it->next_item=next;
			prev=it;
		}
		result=&sit_array[0];
	}
	/*sort_item* tmp3 = result;
	int k=0;
	while(tmp3) {
		cout << "sit:"<<tmp3->sort_value<<endl;
		tmp3=tmp3->next_item;
		k++;
	}
	cout << "sit sorted " << k << " elements" << endl; */
	return result;
}

/* sort HH array by shifts of holes returning a sorted double-linked list */
sort_item* utl_sort_hh(xptr blk){
	int			hh_size=HHSIZE(blk);	/* number of holes in HH */
	sort_item*	hh_array = new sort_item[hh_size];
	sort_item*	it;
	sort_item*	result=NULL;
	/* fil items of hh_array. No links between items are set */
	shft slot_shft = HH_ADDR(blk, 0) - (char*)XADDR(blk);
	for (int j=0; j<hh_size ; slot_shft+=sizeof(hh_slot)) {
		hh_slot* slot = (hh_slot*)((char*)XADDR(blk) + slot_shft);
		it = &hh_array[j];
		it->item_type=ITEM_HOLE;
		it->item_shft=slot_shft;
		it->sort_value=slot->hole_shft;
		j++;
	}
	if (hh_size) {
		qsort((void*)hh_array, hh_size, sizeof(sort_item), utl_cmp);
		/* prepare linked list of the sorted array */
		sort_item*	prev=NULL;
		sort_item*	next=NULL;
		for(int i=0; i<hh_size; i++) {
			it = &hh_array[i];
			if (i+1<hh_size)
				next = &hh_array[i+1];
			else
				next = NULL;
			it->prev_item=prev;
			it->next_item=next;
			prev=it;
		}
		result=&hh_array[0];
	}
	/*
	sort_item* tmp3 = result;
	int k=0;
	while(tmp3) {
		cout << "sit:"<<tmp3->sort_value<<endl;
		tmp3=tmp3->next_item;
		k++;
	}
	cout << "hh sorted " << k << " elements" << endl; */
	return result;
}

/* merge two sorted lists */
sort_item* utl_merge(sort_item* one, sort_item* two) {
	sort_item*	t1=one;
	sort_item*	t2=two;
	sort_item*	result=NULL;
	sort_item*	tmp;
	while(t1 && t2) {
		if (utl_cmp(t1,t2)<0) {
			/* t1 <= t2 */
			/* cut off header element from list "one" and shift ahead t1 pointer */
			tmp = t1;
			t1=t1->next_item;
		} 
		else if (utl_cmp(t1,t2)>0) {
			/* t1 > t2 */
			/* cut off header element from list "two" and shift ahead t2 pointer */
			tmp = t2;
			t2=t2->next_item;			
		}
		else
			throw SYSTEM_EXCEPTION("utl_cmp: bad sorted arrays");
		/* link cutted element to result array */
		if (result) {
				result->next_item=tmp;
				tmp->prev_item=result;
			}
			result=tmp;
	}
	tmp= NULL;
	if (t1)
		tmp=t1;
	if (t2)
		tmp=t2;
	if (tmp) {
		/* copy ramaining tail to result */
		if (result) {
			result->next_item=tmp;
			tmp->prev_item=result;
		}
		result = tmp;
	}
	/* rewind result to the begining */
	if (result)
		while (result->prev_item)
			result=result->prev_item;
	
	int k=0;
	sort_item*	tmp3=result;
	while(tmp3) {
	//	cout << "merged:"<<tmp3->sort_value<<endl;
		tmp3=tmp3->next_item;
		k++;
	}
	//cout << "merged " << k << " elements" << endl; 
	return result;
};

void utl_print(sort_item* head) {
	sort_item*	cur = head;
	while (cur) {
        d_printf4("item slot:%d type:%d item begining:%d\n", (int)cur->item_shft, cur->item_type, (int)cur->sort_value);
		cur=cur->next_item;
	}
}

int utl_cmp(const void* item1, const void* item2) {
	shft v1 = ((sort_item*)item1)->sort_value;
	shft v2 = ((sort_item*)item2)->sort_value;
	if ( v1<v2 ) return -1;
	else if (v1==v2) 
	{
		return 0;
	}
	else return 1;
}


