/*
* BTrie headers
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _BTRIE_READSTATE_H
#define _BTRIE_READSTATE_H

#include "btrie_internal.h"

/***********************************************************************
  This function is very time critical, so it is inlined in header file
  It reads state descriptor from byte sequence
*/

inline
static char * read_state(const char * p, struct state_descriptor * d)
{
    flags_t meta;
    uint8_t prefix_len;
    uint8_t edge_count;
    uint16_t object_len;
    char * s = (char *) p;

    d->p = (char *) p;

    CAST_AND_READ(meta, s);
    d->flags = meta;

    if ((meta & STATE_LONG_JUMP) > 0) {
        CAST_AND_READ(d->long_jump, s);
        d->len = (s - p);
        return s;
    } else {
        d->long_jump = XNULL;
    }

    if ((meta & STATE_NO_PREFIX) > 0) {
        prefix_len = 0;
    } else {
        CAST_AND_READ(prefix_len, s);
    }

    if ((meta & STATE_HAS_EDGES) > 0) {
        CAST_AND_READ(edge_count, s);
    } else {
        edge_count = 0;
    }

    if (((meta & STATE_FINAL) == 0) || ((meta & STATE_NO_OBJECT) > 0)) {
        object_len = 0;
    } else {
        CAST_AND_READ(object_len, s);
    }

    d->prefix_len = prefix_len;
    d->object_len = object_len;
    d->edge_count = edge_count;
    d->edge_len = d->edge_count * (sizeof(char) + sizeof(sptr_t));

    d->prefix = s;
    s += d->prefix_len;
    d->edges = s;
    s += d->edge_count * sizeof(char);
    d->pointers = (sptr_t *) s;
    s += d->edge_count * sizeof(sptr_t);
    d->object = s;
    s += d->object_len;

    d->len = (s - p);

    return s;
}

inline
static xptr_t st_remove_indirection(xptr_t p) {
    struct st_page_header page_header;
    sptr_t a;
    char * v;

    st_read_page_header(p, &page_header);
    v = (char *) XADDR(p);
    CAST_AND_READ(a, v);

    return page_header.page + page_header.trie_offset + a;
}

#endif /* _BTRIE_READSTATE_H */
