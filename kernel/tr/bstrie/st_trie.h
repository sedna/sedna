#ifndef __ST_TRIE_H
#define __ST_TRIE_H

#include "st_unify.h"
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

typedef uint16_t shift_t;

struct trie_data {
    char * p;
    shift_t len;
    shift_t root_count;
    shift_t * roots;
};

struct state_descriptor {
    char * p;
    uint8_t flags;

    xptr long_jump;

    size_t len, prefix_len, object_len;
    uint8_t edge_count;
    char * edges;
    shift_t * pointers;
    char * prefix;
    char * object;
};

struct st_key_object_pair {
    char * key;
    size_t key_length;
    char * object;
    size_t object_size;
};

enum state_meta_t {
    STATE_LONG_JUMP = 0x01,
    STATE_HAS_OUTGOING_EDGES = 0x04,
    STATE_FINAL = 0x08,
    STATE_LARGE_OBJECT = 0x10,
    STATE_SHORT_PREFIX = 0x20,
    STATE_NO_OBJECT = 0x40,
    STATE_LONG_PREFIX = 0x80
};

struct tree_state {
    struct state_descriptor dsc;

// Some flags
    bool prefix_differs;
    bool final;

// Begin and end
    xptr ptr;
    char * end_ptr;

// Working helpers
    int edge_index;
    int prefix_break_index;
};

struct trie_data * trie_delete_object(struct trie_data * data, struct tree_state * state);

/* Read state at <p> and fill the preallocatied descriptor */
struct state_descriptor * read_state(void * p, struct state_descriptor * state);

size_t get_space_needed(struct tree_state * state, struct st_key_object_pair * value);

void * state_image_find_prefix(char k, struct tree_state * state);

/* Calculate state length of state at <p>, with or withour all of it subtrees */
size_t get_local_state_length(void * p, bool subtree);

/* Creates first state in trie data */
struct trie_data * create_initial_state(struct trie_data * data, struct st_key_object_pair * value);

/* Split state */
struct trie_data * split_state(struct trie_data * data, struct tree_state * state, struct st_key_object_pair * value);

#endif
