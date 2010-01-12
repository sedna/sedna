/*
 * File:  schema.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SCHEMA_H
#define _SCHEMA_H

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"

#include "tr/tr_base.h"
#include "tr/structures/nodes.h"
#include "tr/cat/catalog.h"
#include "tr/cat/catptr.h"
#include "tr/cat/catstructures.h"

#define ISINDEXSUPPORTED(schema) schema->index_object!=NULL

/******************************
 * Namespace handling routines
 */

struct xmlns_local_object {
public:
    char* prefix; /* persistent string */
    char* uri; /* persistent string */
};

struct xmlns_indb_object : public catalog_object {
    char* prefix; /* persistent string */
    char* uri; /* persistent string */

    xptr root; /* where to save this object in catalog */
    xmlns_ptr_pers next_xmlns; /* xmlns list */

/* Common catalog object interface */

    static const int magic = 0x008;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

    inline xmlns_indb_object() :
        prefix(NULL), uri(NULL), root(XNULL), next_xmlns(XNULL) {};
    inline xmlns_indb_object(const char* _prefix, const char* _uri, const xptr _root, const xmlns_ptr_pers _next_xmlns) :
        prefix(NULL), uri(NULL), root(_root), next_xmlns(_next_xmlns) {
        prefix = cat_strcpy(this, _prefix);
        uri = cat_strcpy(this, _uri);
    };
    inline ~xmlns_indb_object() {
        cat_free(prefix);
        cat_free(uri);
    };
    static catalog_object_header * create(const char* prefix, const char* uri, const xptr root, const xmlns_ptr_pers next_xmlns);
};


typedef xmlns_local_object * xmlns_ptr;

#define NULL_XMLNS NULL

void free_xmlns_hash();

xmlns_ptr xmlns_touch(const char * prefix, const char * uri);

inline xmlns_ptr xmlns_touch(xmlns_ptr_pers xmlns) {
    catalog_cptr_template<xmlns_indb_object> a = xmlns;
    return xmlns_touch(a->prefix, a->uri);
}

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
    xmlns_ptr xmlns_local;
    t_item type;

    inline xmlns_ptr get_xmlns() {
        if (xmlns_pers == XNULL) { return NULL; }
        else if (xmlns_local != NULL) { return xmlns_local; }
        else {
            return xmlns_local = xmlns_touch(xmlns_pers->prefix, xmlns_pers->uri);
        }
    };

    inline sc_ref() : name(NULL), xmlns_local(NULL) {
    };

    inline sc_ref(sc_ref const &source) : xmlns_local(NULL) {
        this->snode = source.snode;
        this->name = cat_strcpy(this, source.name);
        this->xmlns_pers = source.xmlns_pers;
        this->type = source.type;
    };

    inline sc_ref & operator = (sc_ref const &source) {
        if (this != &source) {
            this->snode = source.snode;
            this->name = cat_strcpy(this, source.name);
            this->xmlns_pers = source.xmlns_pers;
            this->xmlns_local = NULL;
            this->type = source.type;
        }
        return *this;
    };

    inline ~sc_ref() {
        cat_free(name);
    };

    inline sc_ref(schema_node_xptr _snode, const char* _name, xmlns_ptr_pers _xmlns, t_item _type) :
       snode(_snode), name(NULL), xmlns_pers(_xmlns), xmlns_local(NULL), type(_type) {
        this->name = cat_strcpy(this, _name);
    };

    inline bool same_node(const xmlns_ptr xmlns, const char * name, t_item type) {
        return (my_strcmp(this->name, name) == 0 && this->type == type && this->get_xmlns() == xmlns);
    }
};

typedef cat_list<sc_ref>::item sc_ref_item;

struct sc_node_ref_list : public cat_list<sc_ref> {
    sc_ref_item * last;

    void add_object_tail(sc_ref obj);
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

    inline xmlns_ptr get_xmlns() const {
        if (xmlns_pers == XNULL) { return NULL; }
        else if (xmlns_local != NULL) { return xmlns_local; }
        else {
            return xmlns_local = xmlns_touch(xmlns_pers->prefix, xmlns_pers->uri);
        }
    };

/* Child list */

    sc_node_ref_list children;  /* persistent special */

/* Statistics */

    unsigned int nodecnt; /* persistent */
    unsigned int blockcnt; /* persistent */
    unsigned int extnids; /* persistent */
    unsigned int indir_blk_cnt; /* persistent */
    strsize_t textcnt; /* persistent */
    xptr lastnode_ind; /* persistent */

    cat_list<index_ref> index_list; /* persistent special */
    void remove_index(const index_cell_xptr &c);
#ifdef SE_ENABLE_FTSEARCH
    cat_list<ft_index_cell_xptr> ft_index_list; /* persistent special */
    void remove_ft_index(const ft_index_cell_xptr &c);
#endif
#ifdef SE_ENABLE_TRIGGERS
    cat_list<trigger_cell_xptr> trigger_list; /* persistent special */
    void remove_trigger(const trigger_cell_xptr &c);
#endif

    inline schema_node_object() : xmlns_local(NULL), persistent(true), lastnode_ind(XNULL) {};
//    inline schema_node_object(bool _persistent = true) : persistent(_persistent) {};
    schema_node_object(const doc_schema_node_xptr _root, xmlns_ptr _xmlns, const char * _name, t_item _type, bool _persistent);
    ~schema_node_object();

    /* Schema node comparition */
    inline bool same_node(const xmlns_ptr xmlns, const char * name, t_item type) {
        return (my_strcmp(this->name, name) == 0 && this->type == type && this->get_xmlns() == xmlns);
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

    /* Returns position of the child with the given name and of the given descriptor
     * type exist in schema as the child of the node that corresponds to the current
     * block header; -1 otherwise */
    int find_first_child (
        const xmlns_ptr xmlns,
        const char *    name,
        t_item          type
    ) const;

    inline int get_node_position_in_parent() const {
        return (parent == XNULL) ? -1 : parent->find_first_child(get_xmlns(), name, type);
    };

    bool is_ancestor_or_self (schema_node_cptr node);

    inline char * get_child_name(int i) { return children.get(i)->object.name; };
    inline t_item get_child_type(int i) { return children.get(i)->object.type; };
    inline int    get_child_count() { return children.count(); }

    inline static bool has_children(t_item type) { return (type == element || type == document || type == virtual_root); }
    inline bool has_children() { return has_children(this->type); }

    inline static bool has_text(t_item type) { return (type == comment || type == text || type == attribute || type == pr_ins || type == cdata); }
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
    __int64 total_ext_nids; /* persistent */
    xmlns_ptr_pers xmlns_list;

    cat_list<index_cell_xptr> full_index_list; /* persistent special */
    void delete_index(index_cell_xptr c);
#ifdef SE_ENABLE_FTSEARCH
    cat_list<ft_index_cell_xptr> full_ft_index_list; /* persistent special */
    void delete_ftindex(ft_index_cell_xptr c);
#endif
#ifdef SE_ENABLE_TRIGGERS
    cat_list<trigger_cell_xptr> full_trigger_list; /* persistent special */
    void delete_trigger(trigger_cell_xptr c);
#endif

    xmlns_ptr_pers xmlns_register(xmlns_ptr xmlns);

    /* Create new doc node */
    static catalog_object_header * create(bool persistent);

    /* Create virtual root */
    static catalog_object_header * create_virtual_root();

    inline doc_schema_node_object() {};
    inline doc_schema_node_object(bool _persistent) :
        schema_node_object(XNULL, NULL, NULL, document, _persistent),
        ext_nids_block(XNULL), total_ext_nids(0), xmlns_list(XNULL) {};
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

/* returns the name of atomic type */
// char* convertTypeToName(xmlscm_type i);

/* returns the type of the scheme node */
#define GETTYPE(scm_node) scm_node->type
/* returns the name of the scheme node */
#define GETNAME(scm_node) scm_node->name
/* updates pointer to the first block with the descriptors that correspond to schema node */
#define UPDATEFIRSTBLOCKPOINTER(scm_node, block) scm_node->bblk=block
/* returns pointer to the first block with the descriptors that correspond to schema node */
#define GETFIRSTBLOCKPOINTER(scm_node) scm_node->bblk

#endif
