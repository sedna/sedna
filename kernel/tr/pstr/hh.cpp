/*
 * File:  hh.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/pstr/pstrblk.h"
#include "tr/pstr/hh.h"
/* for debug */
#include "tr/pstr/pstr.h"
#include <iostream>

/* note: numeration of holes starts from index 1 in "hh_" functions */
#define HH_PARENT(i)	(i/2)
#define HH_LEFT(i)		(2*i)
#define HH_RIGHT(i)		(2*i + 1)
/* returns pointer to i-th slot, i must be > 0 */

inline hh_slot* HH_SLOT(xptr p, int i) {
		return (hh_slot*)((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 6*sizeof(shft) + (i-1)*sizeof(hh_slot));
}

shft hh_maxhole_size(xptr blk) {
	if (HHSIZE(blk) > 0) {
		hh_slot h = *(hh_slot*)HH_SLOT(blk, 1);
		return h.hole_size;
	} else {
		return 0;
	}
}

void hh_insert(xptr blk, hh_slot s) {
	(*(shft*)HHSIZE_ADDR(blk))+=1;
	int			hh_size =HHSIZE(blk);
	int			i = hh_size;
	while (i>1 && HH_SLOT(blk, HH_PARENT(i))->hole_size < s.hole_size) {
		*HH_SLOT(blk, i)=*HH_SLOT(blk, HH_PARENT(i));
		i = HH_PARENT(i);
	}
	/* debug */
	/* hh_slot*	debug = HH_SLOT(blk, i); */
	*HH_SLOT(blk, i)=s;
}

/* remove the 1-st maximum slot and heapify HH */
void hh_remove_max(xptr blk) {
	int	hh_size = HHSIZE(blk);
	if (hh_size < 1)
		throw SYSTEM_EXCEPTION("[hh_remove_max()] HH is empty");
	if (hh_size != 1)
	{
		*HH_SLOT(blk, 1) = *HH_SLOT(blk, hh_size);
	}
	(*(shft*)HHSIZE_ADDR(blk))-=1;
	hh_heapify(blk, 1);
}

/* removes i-th slot and heapifies HH */
void hh_remove(xptr blk, int i) {
	int			hh_size = HHSIZE(blk);
	if (hh_size < i)
		throw  SYSTEM_EXCEPTION("[hh_remove()] trying to remove non-existing element");
	if (hh_size != i)
	{
		*HH_SLOT(blk, i) = *HH_SLOT(blk, hh_size);
	}
	(*(shft*)HHSIZE_ADDR(blk))-=1;
	hh_heapify(blk, i);
}


void hh_heapify(xptr blk, int i) {
	hh_slot		tmp;
	int			hh_size = HHSIZE(blk);
	if (i>hh_size)	return;
	int			l = HH_LEFT(i);
	int			r = HH_RIGHT(i);
	int			largest = i;
	if (l <= hh_size && (HH_SLOT(blk, l))->hole_size  > (HH_SLOT(blk, i))->hole_size)
		largest = l;
	if (r <= hh_size && (HH_SLOT(blk, r))->hole_size > (HH_SLOT(blk, largest))->hole_size)
		largest = r;
	if (largest != i) {
		tmp = *HH_SLOT(blk, i);
		*HH_SLOT(blk, i) =  *HH_SLOT(blk, largest);
		*HH_SLOT(blk, largest) = tmp;
		hh_heapify(blk, largest);
	}
	/*d_printf2("hh_heapify(%d):\n", i);
      d_printf2("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	  pstr_print_blk(blk);
      d_printf2("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
	*/
}
