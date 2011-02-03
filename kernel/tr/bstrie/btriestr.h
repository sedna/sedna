/*
 * BTrie Sedna specialization
 * Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef BTRIESTR_H
#define BTRIESTR_H

#include <string.h>
#include "btrie.h"

static inline btrie_record_t btrie_find_str(const btrie_t tree, const char * key) {
  return btrie_find(tree, key, strlen(key));
}
/*
inline btrie_record_t btrie_find_nearest_str(const btrie_t tree, const char * key) {
  return btrie_find_nearest(tree, key, strlen(key));
}
*/
static inline btrie_record_t btrie_insert_str(btrie_t tree, const char * key, const char * obj, size_t obj_length, bool replace) {
  return btrie_insert(tree, key, strlen(key), obj, obj_length, replace);
}

static inline btrie_record_t btrie_insert_str_str(btrie_t tree, const char * key, const char * obj, bool replace) {
  return btrie_insert(tree, key, strlen(key), obj, strlen(obj), replace);
}

static inline bool btrie_delete_str(btrie_t tree, const char * key) {
  return btrie_delete(tree, key, strlen(key));
}


#endif /* BTRIESTR_H */
