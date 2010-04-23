/*
* BTrie headers
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _BTRIE_UTILS_H
#define _BTRIE_UTILS_H

#include "btrie_internal.h"
#include "btrie_misc.h"

/***********************************************************************
  This utils are very short, so they are inlined in the header file
*/

inline
static char * get_root_state(struct st_page_header * ph) { return (char *) XADDR(ph->page) + ph->trie_offset; }

#endif /* _BTRIE_UTILS_H */