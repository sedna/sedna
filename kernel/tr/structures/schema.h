/*
 * File:  schema.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SCHEMA_H
#define _SCHEMA_H

#include <vector>
#include <set>

#include "common/sedna.h"
#include "common/base.h"

#include "common/errdbg/d_printf.h"

#include "tr/structures/nodetypes.h"
#include "tr/structures/descriptor.h"
#include "tr/structures/xmlns.h"
#include "tr/strings/strings_base.h"
#include "tr/cat/catalog.h"
#include "tr/cat/catptr.h"
#include "tr/cat/catstructures.h"
#include "tr/executor/base/xsd.h"

#define ISINDEXSUPPORTED(schema) schema->index_object!=NULL

struct index_ref {
    index_cell_xptr index;
    schema_node_xptr object;
    schema_node_xptr key;

    index_ref (const index_cell_xptr a_idx, const schema_node_xptr a_object, const schema_node_xptr a_key) : index(a_idx), object(a_object), key(a_key) {};
};

struct sc_ref {
    schema_node_xptr snode;
    char* name;

    xmlns_ptr_pers xmlns_pers;
    mutable xmlns_ptr xmlns_local;

    t_item type;

    inline xmlns_ptr get_xmlns() const {
        if (xmlns_pers == XNULL) { return NULL; }
        else if (xmlns_local != NULL) { return xmlns_local; }
        else {
            return xmlns_local = xmlns_touch(xmlns_pers->prefix, xmlns_pers->uri);
        }
    };

    inline sc_ref() : name(NULL), xmlns_pers(XNULL), xmlns_local(NULL) {};

    inline bool same_node(const xmlns_ptr xmlns, const char * name, t_item type) const {
        return (strcmpex(this->name, name) == 0 && this->type == type && this->get_xmlns() == xmlns);
    }

    inline bool same_node_fair(const char * uri, const char * name, t_item type) const {
        return (strcmpex(this->name, name) == 0 && this->type == type && same_xmlns_uri(this->get_xmlns(), uri));
    }

    inline bool matches(const char * uri, const char * name, t_item type) const {
        return ((this->type & type) > 0 && strcmpex(this->name, name) == 0 && same_xmlns_uri(this->get_xmlns(), uri));
    }
};

typedef cat_list<sc_ref>::item sc_ref_item;

struct sc_node_ref_list : public cat_list<sc_ref> {
    sc_ref_item * last;

    void add_object_tail(schema_node_xptr snode, const char* name, xmlns_ptr_pers xmlns, t_item type);
    sc_ref_item * get(int i) const;
    int count() const;

    void serialize(se_simplestream &stream);
    void deserialize(se_simplestream &stream);
};


/**************
 * Scheme node
 */


struct schema_node_object : public catalog_object
{
private:
    xmlns_ptr_pers xmlns_pers; /* persistent */
    mutable xmlns_ptr xmlns_local;
    mutable int indexInParent;

public:

/* Common catalog object interface */

    static const int magic = 0x010;
    int get_magic() { return magic; };

    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

/* Fields */

    bool persistent; /* True if serialized. Calculatable. */

    char * name; /* for elements/attributes */ /* persistent string */

    doc_schema_node_xptr root; /* persistent */
    schema_node_xptr     parent; /* pointer to the parent node */ /* persistent */

    t_item type; /* type of node: element/text/attribute/simple */ /* persistent */
    xptr bblk; /* pointer to the first block of block chain */ /* persistent */
    xptr bblk_indir; /* beggining block with free indirection quota*/ /* persistent */

    int getIndex() const {
        if (indexInParent == -2) {
            if (parent == XNULL) {
                indexInParent = -1;
            } else {
                indexInParent = parent->find_child(this->p_object);
            }
        }

        return indexInParent;
    }

    const char * get_name () const {
        return name;
    }

    inline xmlns_ptr get_xmlns() const {
        if (xmlns_pers == XNULL) { return NULL; }
        else if (xmlns_local != NULL) { return xmlns_local; }
        else {
            return xmlns_local = xmlns_touch(xmlns_pers->prefix, xmlns_pers->uri);
        }
    };

/* Child list */

    sc_node_ref_list *children;  /* persistent special */

/* Statistics */

    unsigned int nodecnt; /* persistent */
    unsigned int blockcnt; /* persistent */
    unsigned int extnids; /* persistent */
    unsigned int indir_blk_cnt; /* persistent */
    strsize_t textcnt; /* persistent */
    xptr lastnode_ind; /* persistent */

    cat_list<index_ref> *index_list; /* persistent special */
    void remove_index(const index_cell_xptr &c);
#ifdef SE_ENABLE_FTSEARCH
    cat_list<ft_index_cell_xptr> *ft_index_list; /* persistent special */
    void remove_ft_index(const ft_index_cell_xptr &c);
#endif
#ifdef SE_ENABLE_TRIGGERS
    cat_list<trigger_cell_xptr> *trigger_list; /* persistent special */
    void remove_trigger(const trigger_cell_xptr &c);
#endif

    schema_node_object();
//    inline schema_node_object(bool _persistent = true) : persistent(_persistent) {};
    schema_node_object(const doc_schema_node_xptr _root, xmlns_ptr _xmlns, const char * _name, t_item _type, bool _persistent);
    ~schema_node_object();

    /* Schema node comparition */
    inline bool same_node(const char * uri, const char * local, t_item type) const {
        return (this->type == type) && same_xmlns_uri(get_xmlns(), uri) && (strcmpex(name, local) == 0);
    }

    inline bool matches(const char * uri, const char * local, t_item type) const {
        return ((this->type & type) > 0) && same_xmlns_uri(get_xmlns(), uri) && (strcmpex(name, local) == 0);
    }

    inline bool same_node(const xmlns_ptr xmlns, const char * local, t_item type) const {
        return (this->type == type) && same_xmlns_uri(get_xmlns(), xmlns) && (strcmpex(name, local) == 0);
    }

    /* Create new schema node */
    static catalog_object_header * create(
        doc_schema_node_xptr root,
        xmlns_ptr            xmlns,
        const char *         name,
        t_item               type,
        bool                 persistent
    );

    /* Find first child of given type of this node by name */
    schema_node_xptr get_first_child(
        const xmlns_ptr xmlns,
        const char *    name,
        t_item          type
    ) const;

    schema_node_xptr get_first_child(const xsd::QName & qname, t_item type);

    /* Returns info about the child */
    const sc_ref * get_first_child_ref(
        const xmlns_ptr xmlns,
        const char *    name,
        t_item          type
    ) const;

    /* Insert new node to the schema as the child of the existing one */
    schema_node_xptr add_child(
        const xmlns_ptr xmlns,
        const char *    name,
        t_item          type
    );

    schema_node_xptr add_child(const xsd::QName & qname, t_item type);

    /* Returns position of the child with the given name and of the given descriptor
     * type exist in schema as the child of the node that corresponds to the current
     * block header; -1 otherwise */
    int find_first_child (
        const xmlns_ptr xmlns,
        const char *    name,
        t_item          type
    ) const;

    int find_child(const xptr schema_node) const;

    int find_child_fair(const char * uri, const char * name, t_item type) const;
    int find_child_fair(const xsd::QName & qname, t_item type) const;

    inline int get_node_position_in_parent__depricated() const {
        return (parent == XNULL) ? -1 : parent->find_first_child(get_xmlns(), name, type);
    };

    bool is_ancestor_or_self (schema_node_cptr node);

    inline char * get_child_name(int i) { return children->get(i)->object.name; };
    inline t_item get_child_type(int i) { return children->get(i)->object.type; };
    inline int    get_child_count() { return children->count(); }

    inline static bool has_children(t_item type) { return (type == element || type == document || type == virtual_root); }
    inline bool has_children() { return has_children(this->type); }

    inline static bool has_text(t_item type) { return internal::isTextType(type); }
    inline bool has_text() { return has_text(this->type); }
//    inline char * get_child_name(int i) { return children.get(i)->name; };
};



struct doc_schema_node_object: public schema_node_object
{
/* Common catalog object interface */

    static const int magic = 0x011;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

/* Fields */

    xptr    ext_nids_block; /* persistent */
    uint64_t total_ext_nids; /* persistent */
    xmlns_ptr_pers xmlns_list;

    cat_list<index_cell_xptr> *full_index_list; /* persistent special */
    void delete_index(index_cell_xptr c);
#ifdef SE_ENABLE_FTSEARCH
    cat_list<ft_index_cell_xptr> *full_ft_index_list; /* persistent special */
    void delete_ftindex(ft_index_cell_xptr c);
#endif
#ifdef SE_ENABLE_TRIGGERS
    cat_list<trigger_cell_xptr> *full_trigger_list; /* persistent special */
    void delete_trigger(trigger_cell_xptr c);
#endif

    xmlns_ptr_pers xmlns_register(xmlns_ptr xmlns);

    /* Tells if it is document or collection */
    inline bool is_document() { return get_magic() == doc_schema_node_object::magic; }

    /* Create new doc node */
    static catalog_object_header * create(bool persistent);

    /* Create virtual root */
    static catalog_object_header * create_virtual_root();

    doc_schema_node_object();
    doc_schema_node_object(bool _persistent);
    ~doc_schema_node_object() {};
};


struct col_schema_node_object : public doc_schema_node_object
{
/* Common catalog object interface */

    static const int magic = 0x012;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

/* Fields */

    xptr    eblk;     /* persistent */       /* pointer to the last block of block chain */
    xptr    metadata; /* persistent */

    /* Create new collection node */
    static catalog_object_header * create();

    inline col_schema_node_object() : doc_schema_node_object(true), eblk(XNULL), metadata(XNULL) {};
    ~col_schema_node_object() {};

    xptr find_document(const char * doc_name);
    void delete_document(const char * doc_name);
    void insert_document(const char * doc_name, xptr node);
};

/* Schema node comparison function for node utils */
typedef bool (*comp_schema)(schema_node_cptr scm, const char* uri, const char* name, t_item type);

/* returns the name of atomic type */
// char* convertTypeToName(xmlscm_type i);

/* checks if the node is the descendant of one of the nodes in the vector */
bool hasAncestorInSet(schema_node_cptr scm_node, std::set<schema_node_xptr>* scm_nodes_set );

#endif
