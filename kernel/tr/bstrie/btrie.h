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

#define BTRNULL ((btrie_record_t) XNULL)

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
btrie_record_t btrie_insert(btrie_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace);
bool           btrie_delete(btrie_t tree, const char * key, size_t key_length);
size_t         btrie_get_object(const btrie_record_t p, char * object, size_t object_size = 0);
bool		   btrie_replace_object(const btrie_record_t p, const char * object);

struct btrie_enum;

typedef struct btrie_enum * btrie_enum_t;

#define NULL_ENUM ((btrie_enum_t) NULL)

/* Trie traversing and values enumeration */

btrie_enum_t btrie_find_prefix(const btrie_t tree, const char * key, size_t key_length, bool * first_key_equal = NULL);
bool btrie_is_EOT(btrie_enum_t cursor);

bool btrie_enum_next(btrie_enum_t cursor);
void btrie_enum_close(btrie_enum_t cursor);
char * btrie_get_key(btrie_enum_t cursor);
size_t btrie_get_key_len(btrie_enum_t cursor);
btrie_record_t btrie_get_state(btrie_enum_t cursor);

#if EL_DEBUG == 1

void __debug_walkthrough(btrie_t tree);

#endif /* EL_DEBUG == 1 */

#endif /* BTRIE_H */
