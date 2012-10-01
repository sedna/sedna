/*
 * File:  pstrblk.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PSTRBLK_H
#define _PSTRBLK_H

#include "common/sedna.h"
#include "common/xptr/sm_vmm_data.h"

#define	HHMAXSIZE	124
struct hh_slot {
	shft	hole_shft;	/* shift from the begining of block */
	shft	hole_size;
};

typedef struct hh_slot hh_slot;
/*************************************************************************
 *************************************************************************
 In all the following macros p is (xptr) begining of block
 *************************************************************************/

//#ifdef labuda
inline	char*	PSTRBLK_BODY(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr));
}
inline	char*	PSTRNUM_ADDR(xptr p)	{
						/* same as PSTRBLK_BODY */
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr));
}
inline	shft	PSTRNUM(xptr p)	{
						return *(shft*)PSTRNUM_ADDR(p);
}
inline	char*	SITB_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + sizeof(shft));
}
inline	shft	SITB(xptr p)	{
						return *(shft*)SITB_ADDR(p);
}
inline	char*	SITH_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 2*sizeof(shft));
}
inline	shft	SITH(xptr p)	{
						return *(shft*)SITH_ADDR(p);
}
inline	char*	SSB_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 3*sizeof(shft));
}
inline	shft	SSB(xptr p)	{
						return *(shft*)SSB_ADDR(p);
}
inline	char*	BFS_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 4*sizeof(shft));
}
inline	shft	BFS(xptr p)	{
						return *(shft*)BFS_ADDR(p);
}
inline	char*	HHSIZE_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 5*sizeof(shft));
}
inline	shft	HHSIZE(xptr p)	{
						return *(shft*)HHSIZE_ADDR(p);
}
/* i-th slot in HH array. Numeration starts from index 0 */
inline	char*	HH_ADDR(xptr p, int i)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 6*sizeof(shft) + (i)*sizeof(hh_slot));
}
inline	char*	SS_ADDR(xptr p)	{
						return ((char*)XADDR(p) + sizeof(struct vmm_sm_blk_hdr) + 6*sizeof(shft) + HHMAXSIZE*sizeof(hh_slot));
}


#define	PSTR_EMPTY_SLOT	(shft)0

#endif
