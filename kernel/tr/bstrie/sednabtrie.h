/*
 * BTrie Sedna specialization
 * Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEDNABTRIE_H
#define SEDNABTRIE_H

#include <string.h>
#include "btrie.h"
#include <stdint.h>

void btrie_collect_stat(xptr entry_point);

inline
static btrie_record_t sbtrie_find_str(const xptr tree, const char * key) {
    btrie_t btrie = btrie_open(tree);
    btrie_record_t result = btrie_find(btrie, key, strlen(key));

    btrie_close(btrie);
    return result;
}

inline
static xptr sbtrie_insert(xptr tree, const char * key, size_t key_len, const char * obj, size_t obj_length, bool replace) {
    btrie_t btrie = btrie_open(tree);
	xptr result;
	btrie_insert(btrie, key, key_len, obj, obj_length, replace);
    result = btrie_get_root(btrie);
    btrie_close(btrie);
    return result;
}

inline
static xptr sbtrie_insert_str(xptr tree, const char * key, const char * obj, size_t obj_length, bool replace) {
    btrie_t btrie = btrie_open(tree);
	xptr result;
	btrie_insert(btrie, key, strlen(key), obj, obj_length, replace);
    result = btrie_get_root(btrie);
    btrie_close(btrie);
    return result;
}

inline
static xptr sbtrie_insert_str_str(xptr tree, const char * key, const char * obj, bool replace) {
    btrie_t btrie = btrie_open(tree);
	xptr result;
	btrie_insert(btrie, key, strlen(key), obj, strlen(obj), replace);
    result = btrie_get_root(btrie);
    btrie_close(btrie);
    return result;
}

inline
static xptr sbtrie_delete_str(xptr tree, const char * key) {
    btrie_t btrie = btrie_open(tree);
	btrie_delete(btrie, key, strlen(key));
    xptr result = btrie_get_root(btrie);
    btrie_close(btrie);
    return result;
}


#endif /* SEDNABTRIE_H */
