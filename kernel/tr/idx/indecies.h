/*
 * File:  indecies.h
 * Copyright (C) 2004-2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDECIES_H
#define _INDECIES_H

#include "common/sedna.h"
#include "common/u/usem.h"

#include "tr/idx/index_types.h"
#include "tr/idx/indeximpl.h"
#include "tr/structures/metadata.h"

extern USemaphore index_sem;

index_cell_xptr create_index(index_descriptor_t * index_dsc);
void drop_index (const char *index_title);

struct index_cell_object : public catalog_object {
  friend index_cell_xptr create_index(index_descriptor_t * index_dsc);
  private:
    /* schemaroot - pointer to a document, which index is created for */
    char * index_title; /* persistent string */

    metadata_cell_xptr owner;
//    lockid_t lock_identifier; /* currently the same as index title */
//    lockid_t parent_object_lock_identifier; /* the identifier of the object to lock when the index is about to be accessed */

    xmlscm_type keytype; /* persistent */ // xml type of key node as defined when index was created
    xptr entry_point; /* persistent */ // index entry point if exists; may be XNULL

    index_backend_t backend_type; /* persistent */

    PathExpr *object; /* persistent special */  // absolute xPath expression for object nodes xPath
    PathExpr *key; /* persistent special */     // relative xPath expression for key value nodes

    /*
     * Error counter: stores the number of uncastable key values.
     * If not zero, index is inconsistent and can not be used.
     * Can be updated quite often.
     */

    int err_cntr; /* persistent */

    mutable idx::KeyValueMultimap * backend;
  public:
    static const int magic = 0x007;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

    const char * get_title() const { return index_title; };
    metadata_cell_xptr get_owner() const { return owner; };
    doc_schema_node_cptr get_root_node() const { return doc_schema_node_cptr(owner->get_schema_node()); };
    xmlscm_type get_keytype() const { return keytype; };

    void get_index_descriptor(index_descriptor_t* dsc) const;

    /* returned result does not need to be deleted after use */
    idx::KeyValueMultimap * get_backend();

    void on_schema_node_created(schema_node_cptr snode) const;

    void put_to_index(xptr key_node, xptr object_indir);
    void delete_from_index(xptr key_node, xptr object_indir);

    inline
    index_cell_object() {};

    index_cell_object(index_descriptor_t * index_dsc);

    virtual ~index_cell_object();

    static catalog_object_header * create(index_descriptor_t * index_dsc);
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

#endif /* _INDECIES_H */
