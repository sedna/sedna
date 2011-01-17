/*
 * File:  metadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _META_DATA_H
#define _META_DATA_H

#include <string>

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/structures/schema.h"
#include "common/u/usem.h"
#include "tr/cat/catalog.h"
#include "tr/cat/catmem.h"

struct metadata_cell_object : public catalog_object {
    friend xptr insert_document(const char *uri, bool persistent);
    friend col_schema_node_xptr insert_collection(const char *collection_name);

    friend void delete_document(const char *document_name);
    friend void delete_collection(const char *collection_name);

    friend void rename_collection(const char *old_collection_name, const char *new_collection_name);
  private:
/* Fields */
    char* name; /* persistent string */
    schema_node_xptr snode; /* persistent */
    bool is_doc; /* persistent */
  public:
    const char * get_name() const { return name; };
    schema_node_xptr get_schema_node() const { return snode; };
    bool is_document() const { return is_doc; };

/* Common catalog object interface */

    static const int magic = 0x020;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

    inline metadata_cell_object() {};

    inline metadata_cell_object(bool _is_doc, const char * _name) :
        name(NULL),
        is_doc(_is_doc)
      { name = cat_strcpy(this, _name); };

    static catalog_object_header * create(bool _is_doc, const char * _name)
    {
        metadata_cell_object * obj =
          new(cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(metadata_cell_object)))
          metadata_cell_object(_is_doc, _name);

        catalog_object_header * header = catalog_create_object(obj);

        catalog_set_name(catobj_metadata, _name, header);
        return header;
    };

};

struct metadata_cell_cptr : public catalog_cptr_template<metadata_cell_object> {
    explicit inline metadata_cell_cptr (catalog_object_header * aobj, bool writable = false) :
        catalog_cptr_template<metadata_cell_object>(aobj, writable) {} ;
    explicit inline metadata_cell_cptr (const char * title, bool write_mode = false) :
        catalog_cptr_template<metadata_cell_object>(catalog_find_name(catobj_metadata, title), write_mode) {};
    inline metadata_cell_cptr (const xptr p, bool writable = false) :
        catalog_cptr_template<metadata_cell_object>(p, writable) {};
};

typedef metadata_cell_cptr metadata_cptr;

void metadata_on_session_begin();
void metadata_on_session_end();

void delete_document(const char *document_name);
void delete_collection(const char *collection_name);
void delete_document_from_collection(const char *collection_name, const char *document_name);

xptr insert_document(const char *uri, bool persistent = true);
col_schema_node_xptr insert_collection(const char *collection_name);
xptr insert_document_into_collection(const char *collection_name, const char *uri);

inline bool document_or_collection_exists(const char *name) {
  return catalog_name_exists(catobj_metadata, name);
}

doc_schema_node_xptr find_document(const char *document_name);
col_schema_node_xptr find_collection(const char *collection_name);
xptr find_document_in_collection(const char *collection_name, const char *document_name);

void rename_collection(const char *old_collection_name, const char *new_collection_name);

#endif

