#ifndef __ST_PAGES_H
#define __ST_PAGES_H

#include "st_unify.h"
#include "st_trie.h"

struct tree_state_path {
    int state_count;
    int key_break;

    struct tree_state * last_state;
    struct tree_state * states[0];
};

struct st_page_header {
    xptr page; // page long pointer

    struct trie_data trie;

    shift_t root_offset; // offset to trie from the beginning of page
    shift_t trie_offset; // offset to trie from the beginning of page

    shift_t free_space; // free space left on page

//    int depth; // unused
};

/* methods */

/* Read state at <p>, fill the preallocatied descriptor and compare its prefix with key */
struct tree_state * state_find_key(char * p, char ** k, size_t * key_length, struct tree_state * state);

st_t make_first_state(st_t tree, struct st_key_object_pair * value);
//struct str_tree * page_split(struct str_tree * tree, xptr * page_stack, shift_t size, shift_t state, struct st_page_header * page_header);
void page_split(xptr * page_stack, shift_t size, shift_t state, struct st_page_header * page_header);

struct st_page_header * st_create_markup_page(struct st_page_header * new_page_header, int root_count, bool create);
struct st_page_header * read_page_header(xptr page, struct st_page_header * header);
struct st_page_header * save_page_header(struct st_page_header * header);

struct tree_state_path * find_state_path(st_t tree, const char * key, size_t key_len, xptr * page_stack);
void free_state_path(struct tree_state_path * state_path);

#endif /* __ST_PAGES_H */

