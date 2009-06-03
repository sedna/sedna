#ifndef BLOCK_STRING_TREE_H
#define BLOCK_STRING_TREE_H

#include "st_unify.h"

extern int st_last_error;

struct str_tree;

enum error_codes {
    ST_ERROR_NO_ERROR = 0,
    ST_ERROR_NOT_FOUND = 1,
    ST_ERROR_KEY_EXISTS = 2
};

st_t st_init();

xptr st_find_string(const st_t tree, const char * key);
xptr st_find_binary(const st_t tree, const char * key, size_t key_length);

//xptr st_find_nearest_string(const st_t tree, const char * key);
//xptr st_find_nearest_binary(const st_t tree, const char * key, size_t key_length);

st_t st_insert_string(st_t tree, const char * key, const char * obj, size_t obj_length, bool replace);
st_t st_insert_binary(st_t tree, const char * key, size_t key_length, const char * obj, size_t obj_length, bool replace);

st_t st_delete_string(st_t tree, const char * key);
st_t st_delete_binary(st_t tree, const char * key, size_t key_length);

size_t st_get_object(const xptr p, char * object);
size_t st_serialize_object(const xptr p, char ** object);

struct st_enum;
typedef struct st_enum * st_enumeration;

st_enumeration st_enum_start(xptr first_state);
bool st_enum_next(st_enumeration en);
bool st_enum_free(st_enumeration en);

size_t st_enum_get_key(st_enumeration en, char * key);
char * st_enum_get_key_string(st_enumeration en, size_t * len);
xptr st_enum_get_object(st_enumeration en);

xptr get_initial_state_ptr(const st_t tree);

#endif /* BLOCK_STRING_TREE_H */
