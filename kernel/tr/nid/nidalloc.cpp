/*
 * File:  nidalloc.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/nid/nidalloc.h"

nid_slot*	nid_head=NULL;
int			nid_size=1;

/* allocate new register of size PAGE_SIZE */
void*	nid_alloc() {
	if (nid_head==NULL) {
		nid_head = new nid_slot();
		nid_head->used=true;
		return (void*)nid_head->buf;
	} else {
		nid_slot* tmp = nid_head;
		while (true) {
			if (!tmp->used) {
				tmp->used=true;
				return (void*)tmp->buf;
			}
			if (!tmp->next) {
				nid_size++;
				nid_slot* tmp2 = new nid_slot();
				tmp->next=tmp2;
				tmp2->used=true;
				return (void*)tmp2->buf;
			} else 
				tmp=tmp->next;
		}
	}	
}

/* */
void	nid_free(void* buf) {
	nid_slot*	s = (nid_slot*)buf;
	s->used=false;
}

