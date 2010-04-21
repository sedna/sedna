/*
 * File:  index_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEX_DATA_H
#define _INDEX_DATA_H

#include "common/sedna.h"

#include "tr/cat/catalog.h"
#include "tr/cat/catmem.h"
#include "common/u/usem.h"
#include "tr/structures/nodes.h"
#include "tr/executor/base/XPathOnSchema.h"

struct PathExpr;

extern USemaphore index_sem;

void index_on_session_begin();
void index_on_session_end();

struct index_cell_object : public catalog_object {
public:
/* Common catalog object interface */

    static const int magic = 0x007;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

/* Fields */

    char * index_title; /* persistent string */
    xmlscm_type keytype; /* persistent */  // Xml type of key node as defined when index was created
    xptr btree_root; /* persistent */      // Btree root page if exists; may be XNULL

    PathExpr *object; /* persistent special */  // absolute xPath expression for object nodes xPath
    PathExpr *key; /* persistent special */     // relative xPath expression for key value nodes

    /*
     * schemaroot - pointer to a document, which index is created for.
     * doc_name, is_doc - the name of the document (or collection), which the index was created for.
     * Latters are calculateble, so I don't think we should store it.
     */

    doc_schema_node_xptr schemaroot; /* persistent */
    char* doc_name; /* persistent string */
    bool is_doc; /* persistent */

    /*
     * Error counter: stores the number of uncastable key values.
     * If not zero, index is inconsistent and can not be used.
     * Can be updated quite often.
     */

    int err_cntr; /* persistent */

    void set_index_title(const char * a_index_title);
    void set_doc_name(const char * a_doc_name);

/*
 * Specific index cell interface functions
 */

//    t_scmnodes fits_to_index_as_key(schema_node_cptr snode) const;
    void new_node_available(schema_node_cptr snode) const;

    void put_to_index(xptr key_node, xptr object_indir);
/*
    void put_to_index(xptr node, schema_node_cptr accessor);
    void put_to_index(xptr node,const char* value, int size, schema_node_cptr accessor);
*/
    void delete_from_index(xptr key_node, xptr object_indir);
/*
    void delete_from_index(xptr node, schema_node_cptr accessor);
    void delete_from_index(xptr node,const char* value, int size, schema_node_cptr accessor);
*/
    inline index_cell_object() {};

    inline index_cell_object(
        const char * _index_title, doc_schema_node_xptr _schemaroot,
        xmlscm_type _keytype, xptr _btree_root, PathExpr * _object,
        PathExpr * _key, const char * _doc_name, bool _is_doc
      ) :
        index_title(NULL),
        keytype(_keytype),
        btree_root(_btree_root),
        object(_object),
        key(_key),
        schemaroot(_schemaroot),
        doc_name(NULL),
        is_doc(_is_doc),
        err_cntr(0)
    {
      index_title = cat_strcpy(this, _index_title);
      doc_name = cat_strcpy(this, _doc_name);
    };

    static catalog_object_header * create(
        const char * _index_title, doc_schema_node_xptr _schemaroot,
        xmlscm_type _keytype, xptr _btree_root, PathExpr * _object,
        PathExpr * _key, const char * _doc_name, bool _is_doc
      )
    {
        index_cell_object * obj =
          new(cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(index_cell_object)))
          index_cell_object(_index_title, _schemaroot, _keytype, _btree_root, _object, _key, _doc_name, _is_doc);

        catalog_object_header * header = catalog_create_object(obj);

        catalog_set_name(catobj_indicies, _index_title, header);
        catalog_htable_set(catobj_indicies, _index_title, (_is_doc ? 'D' : 'C'), _doc_name);

        return header;
    };
};

/*
 * Counted pointer for index cell.
 */

struct index_cell_cptr : public catalog_cptr_template<index_cell_object> {
    explicit inline index_cell_cptr (catalog_object_header * aobj, bool writable = false) :
        catalog_cptr_template<index_cell_object>(aobj, writable) {} ;
    explicit inline index_cell_cptr (const char * index_title, bool write_mode = false) :
        catalog_cptr_template<index_cell_object>(catalog_find_name(catobj_indicies, index_title), write_mode) {};
    inline index_cell_cptr (const xptr p, bool writable = false) :
        catalog_cptr_template<index_cell_object>(p, writable) {};
};

#endif
