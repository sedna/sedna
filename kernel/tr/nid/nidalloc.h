/*
 * File:  nidalloc.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _NIDALLOC_H
#define _NIDALLOC_H

#include "common/sedna.h"
#include "common/base.h"

struct	nid_slot {
    unsigned char       buf[PAGE_SIZE];
    struct nid_slot*    next;
    bool                used;
    nid_slot() {
        next=NULL;
        used=false;
	}
};

typedef struct nid_slot nid_slot;

void*   nid_alloc();
void    nid_free(void* buf);

#endif
