/*
* BTrie headers
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _BTRIE_INTERNAL_H
#define _BTRIE_INTERNAL_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>

#include "btrie_unify.h"
#include "btrie_misc.h"
#include "btrie_structures.h"

enum {
    st_trie_list_offset = ST_PAGE_HEADER_OFFSET + sizeof(uint16_t) + sizeof(sptr_t)
};

enum {
    MAX_STATE_SIZE = ((PAGE_SIZE - ST_PAGE_HEADER_OFFSET) / 3)
};

/** Page header handling */
struct st_page_header * st_read_page_header(xptr_t p, struct st_page_header * header);
struct st_page_header * st_write_page_header(struct st_page_header * header);
struct st_page_header * st_markup_page(struct st_page_header * new_page_header, int root_count, bool create_page);

/** Key path search */
struct st_path * st_find_state_path(const struct btrie * tree, const char * key, size_t key_len);

void st_sp_free(struct st_path * state_path);

/** Page split */
xptr_t st_split(struct btrie tree, struct st_path * sp, int cpage);

struct st_tmp_trie * st_state_delete_prepare(char * state);

/** Break the given <state> into new ones (two or three). If state is null, create it.
  * This function only returns INFORMATION about new state, not modifing anything.
  */
void st_new_state_prepare(struct st_tmp_trie * result, char * state, int prefix_pos, int key_pos,
        struct st_key_object_pair * key_object_pair, bool split_state);

/** Write the state to the buffer and offset buffer as precalculated */
int st_new_state_write(struct st_page_header * pghdr, struct st_tmp_trie * new_state, char * state, sptr_t state_len);

/** Free state insertion info */
void st_new_state_free(struct st_tmp_trie * state);


void __debug_walkthrough_branch(char * buffer, char * prefix_append, char * state);




#endif /* _BTRIE_INTERNAL_H */
