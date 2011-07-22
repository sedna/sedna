/*
 * File:  xmlns.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "tr/structures/xmlns.h"

#include "tr/cat/catstore.h"

#define XMLNS_HASH_SIZE 32

struct xmlns_hash_object {
    struct xmlns_local_object object;
    struct xmlns_hash_object *next;
};

struct xmlns_hash_object * xmlns_hash[XMLNS_HASH_SIZE] = {NULL};

xmlns_ptr xmlns_touch(const char * prefix, const char * uri)
{
    if ((prefix == NULL) && (uri == NULL)) return NULL_XMLNS;

    if (prefix == NULL) { prefix = ""; }
    if (uri == NULL) { uri = ""; }

    unsigned int original_hash = (strhash(prefix) + 33 * strhash(uri)) % XMLNS_HASH_SIZE;
    struct xmlns_hash_object * i = xmlns_hash[original_hash];

    while (i != NULL) {
        if ((strcmpnull(i->object.prefix, prefix) == 0) && (strcmpnull(i->object.uri, uri) == 0)) {
            return &(i->object);
        }
        i = i->next;
    }

    i = (struct xmlns_hash_object *) cat_malloc_context(CATALOG_COMMON_CONTEXT, sizeof(struct xmlns_hash_object));
    i->object.prefix = cat_strcpy(i, prefix);
    i->object.uri = cat_strcpy(i, uri);
    i->next = xmlns_hash[original_hash];
    xmlns_hash[original_hash] = i;

    return &(i->object);
}

xmlns_ptr xmlns_touch_len(const char* prefix, const char* uri, size_t uri_len)
{
    U_ASSERT(uri != NULL);
    if (prefix == NULL) { prefix = ""; }

    unsigned int original_hash = (strhash(prefix) + 33 * strnhash(uri, uri_len)) % XMLNS_HASH_SIZE;
    struct xmlns_hash_object * i = xmlns_hash[original_hash];

    while (i != NULL) {
        if ((strcmpnull(i->object.prefix, prefix) == 0) && (strncmp(i->object.uri, uri, uri_len) == 0)) {
            return &(i->object);
        }
        i = i->next;
    }

    i = (struct xmlns_hash_object *) cat_malloc_context(CATALOG_COMMON_CONTEXT, sizeof(struct xmlns_hash_object));
    i->object.prefix = cat_strcpy(i, prefix);
    i->object.uri = cat_strncpy(i, uri, uri_len);
    i->next = xmlns_hash[original_hash];
    xmlns_hash[original_hash] = i;

    return &(i->object);
}

void free_xmlns_hash()
{
    for (unsigned k = 0; k < XMLNS_HASH_SIZE; k++)
        xmlns_hash[k] = NULL;
}

catalog_object_header * xmlns_indb_object::create(const char* prefix, const char* uri, const xptr root, const xmlns_ptr_pers next_xmlns)
{
    CatalogMemoryContext *context = (root != XNULL) ? CATALOG_PERSISTENT_CONTEXT : CATALOG_TEMPORARY_CONTEXT;

    xmlns_indb_object * a = new (cat_malloc_context(context, sizeof(xmlns_indb_object)))
      xmlns_indb_object(prefix, uri, root, next_xmlns);

    return catalog_create_object(a, (root != XNULL));
}


void xmlns_indb_object::serialize_data(se_simplestream &stream)
{
    cs_set_hint(root);

    stream.write(&next_xmlns, sizeof(xmlns_ptr_pers));
    stream.write_string(prefix);
    stream.write_string(uri);
}

void xmlns_indb_object::deserialize_data(se_simplestream &stream)
{
    root = p_object; /* actually there is no need in root now */

    stream.read(&next_xmlns, sizeof(xmlns_ptr_pers));
    prefix = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, prefix);
    uri = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, uri);
}

void xmlns_indb_object::drop()
{
    cs_free(p_object);
    catalog_delete_object(this);
}

xmlns_ptr generate_prefix(const char * prefix, const char * uri)
{
    size_t len = strlen(prefix);
    char * x = (char *) malloc(len + 3 + 1);
    xmlns_ptr result;

    memcpy(x, prefix, len);
    memcpy(x + len, "-se\0", 4);

    result = xmlns_touch(x, uri);
    free(x);
    return result;
}

xmlns_ptr generate_prefix(int ctr,const char* uri,dynamic_context *cxt)
{
    char x[12] = "XXX";
    if (ctr!=0)
        sprintf(x+3, "%d",ctr );
    return xmlns_touch(x,uri);
}
