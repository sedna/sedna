/*
* BTrie run-time structures
* Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _BTRIE_STRUCTURES_H
#define _BTRIE_STRUCTURES_H

#include <stdint.h>
#include <stdlib.h>

struct btrie {
    xptr_t root_page;
};

/********************************************************************
    Runtime state description
*/

typedef uint8_t flags_t;

struct state_descriptor {
    char * p;

    flags_t flags;
    xptr_t long_jump;
    sptr_t len, prefix_len, object_len;
    uint8_t edge_count;
    sptr_t edge_len;
    char * edges;
    sptr_t * pointers;
    char * prefix;
    char * object;
};

/* State flags  */

enum state_meta_t {
    STATE_LONG_JUMP      = 0x0001,
    STATE_FINAL          = 0x0002,
    STATE_HAS_EDGES      = 0x0004,
    STATE_NO_PREFIX      = 0x0008,
    STATE_SHORT_PREFIX   = 0x0010,
    STATE_NO_OBJECT      = 0x0020,
    STATE_EDGE_TREE      = 0x0040,
    STATE_FIXED_OBJECT   = 0x0080
};

#define IS_FINAL_STATE(p) (((p).dsc.flags & STATE_FINAL) > 0)


/********************************************************************
    Structure to pack key and object info
*/

struct st_key_object_pair {
    char * key;
    size_t key_length;
    char * object;
    size_t object_size;
    bool is_final;
};


/********************************************************************
    Runtime page header description
*/

struct st_page_header {
    xptr_t page;        // page long pointer
    int trie_count;     // number of ties with different roots
    sptr_t * tries;     // in-page pointer (volid only while page is in memory)

    sptr_t trie_offset; // offset to trie from the beginning of page
    sptr_t data_end;    // offset to data end from the beginning of page
    sptr_t free_space;  // free space left on page
};

/********************************************************************
    Structures, used to describe page split and new state insertion
*/

struct st_tmp_trie {
    char * buf;
    sptr_t len;
    sptr_t offset;
};

struct st_page_split_subrecord {
    sptr_t * node_offsets;
    int node_count;
    char * nodes;
    int nodes_len;
    xptr_t ** jumps;
};

struct st_page_split_record {
    char * root;
    int root_len;

    struct st_page_split_subrecord left, right;

    struct st_page_split_subrecord * new_subject_wing;
    sptr_t new_subject_pos;
};

/******************************************************
    Structures, used to describe the path to some key
*/

struct st_state_ext {
    xptr_t ptr;
    int edge_index;
    char edge;

    struct state_descriptor dsc;
};


struct st_static_page_data;
struct st_static_state_data;

struct st_path {
    int prefix_break_position;
    int key_break_position;
    bool key_found;

    struct st_static_state_data * last_state;

    int __page_cap;
    int page_count;
    struct st_static_page_data * pages;

    int __state_cap;
    int state_count;
    struct st_static_state_data * states;
};

struct st_static_page_data {
    xptr_t page;
    sptr_t free_space;
    sptr_t long_jump;
    struct st_static_state_data * parent_state;
};

struct st_static_state_data {
    struct st_static_page_data * page;

    xptr_t p;
    bool is_root;
//    sptr_t prefix_len;
//    char * prefix;
//    uint8_t edge_count;
//    uint8_t long_edge_count;
};

#endif /* _BTRIE_STRUCTURES_H */