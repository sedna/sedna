/*
 * File:  pstr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PSTR_H
#define _PSTR_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"

extern const shft	PSTRMAXSIZE;

/* functions for persistent string management */
xptr    pstr_create_blk(bool persistent);
xptr	pstr_modify(xptr node, char* s, int s_size);
xptr	pstr_allocate(xptr blk, xptr node, const char* s, int s_size);
void	pstr_deallocate(xptr node);
void	pstr_read_from_node(xptr node, char* s);
void	pstr_read(xptr ps, int ps_size, char* the_s);
bool	pstr_fit_into_blk(xptr blk, shft s_size);
/* dereference indirect xptr pointer to persistent string; returns direct xptr */
//#define	PSTRDEREF(p)	ADDR2XPTR((char*)XADDR(BLOCKXPTR(p)) + *(shft*)XADDR(p))
#define	PSTRDEREF(p)	(BLOCKXPTR(p) + *(shft*)XADDR(p))

inline
xptr pstrderef(const xptr p) {
    return (BLOCKXPTR(p) + *(shft*)XADDR(p));
}


/* internal functions */
void	pstr_blk_markup(xptr blk);
void    pstr_delete_blk(xptr blk);
xptr	pstr_do_allocate(xptr blk, const char* s, int s_size);
bool	pstr_do_deallocate(xptr blk, xptr ps, int s_size, bool drop_empty_block);
xptr	pstr_insert_into_tail(xptr blk, const char* s, int s_size);
xptr	pstr_insert_into_maxhole(xptr blk, const char* s, int s_size);
void	pstr_defragment(xptr blk);
xptr	pstr_migrate(xptr blk, xptr node, const char* s, int s_size);
void	pstr_print_blk(xptr blk);
bool is_last_shft_in_blk(xptr addr);
void check_blk_consistency(xptr addr);


#endif
