/*
* BTrie
* This library works as the part of Sedna project
* Copyright (c) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef BTRIE_H
#define BTRIE_H

#include <stdlib.h>
#include <stdint.h>

#include "btrie_unify.h"

extern int btrie_last_error;
struct btrie;
typedef struct btrie * btrie_t;
typedef xptr_t btrie_record_t;

#define BTRNULL XNULL

enum error_codes {
    ST_ERROR_NO_ERROR = 0,
    ST_ERROR_NOT_FOUND = 1,
    ST_ERROR_KEY_EXISTS = 2,
    ST_ERROR_UNKNOWN = 3,
};

btrie_t  btrie_open(const xptr_t root);
xptr_t   btrie_get_root(btrie_t bt);
void     btrie_close(btrie_t bt);

btrie_record_t btrie_find(const btrie_t tree, const char * key, size_t key_length);
btrie_record_t btrie_find_nearest(const btrie_t tree, const char * key, size_t key_length);
btrie_record_t btrie_insert(btrie_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace);
bool           btrie_delete(btrie_t tree, const char * key, size_t key_length);
size_t         btrie_get_object(const btrie_record_t p, char * object);

#if EL_DEBUG == 1

void __debug_walkthrough(btrie_t tree);

#endif /* EL_DEBUG == 1 */

#endif /* BTRIE_H */
