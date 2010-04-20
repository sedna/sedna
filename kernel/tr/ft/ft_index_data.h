/*
 * File:  ft_index_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_INDEX_DATA_H
#define _FT_INDEX_DATA_H


#include "common/sedna.h"

#include "common/u/usem.h"
#include "tr/structures/nodes.h"
#include "tr/strings/strings.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/ft/ft_cache.h"
//#include "tr/ft/ft_index.h"
#include "tr/ft/update_history.h"

#include "tr/cat/catmem.h"

struct PathExpr;

enum ft_index_type
{
	ft_xml,
	ft_xml_ne, //xml without escaping special chars in elements, replacing " with \u+e803 in attributes
	ft_xml_hl,
	ft_string_value,
	ft_delimited_value,
	ft_customized_value
};

enum ft_index_impl
{
	ft_ind_dtsearch,
	ft_ind_native
};

struct ft_custom_cell
{
	xmlns_ptr_pers ns_pers;
	xmlns_ptr ns_local;

	char* local;
	ft_index_type cm;

    inline xmlns_ptr get_xmlns() {
        if ((ns_local != NULL_XMLNS) || (ns_pers == XNULL)) return ns_local;
        else return ns_local = xmlns_touch(ns_pers);
    }

	inline ft_custom_cell() : ns_local(NULL_XMLNS) {};

	inline ft_custom_cell(xmlns_ptr_pers _ns, xmlns_ptr _ns_local, const char* _local,ft_index_type _cm) :
        	ns_pers(_ns), ns_local(_ns_local), local(NULL), cm(_cm) { local = cat_strcpy(this, _local); };

    inline bool less( ft_custom_cell *p1)
    {
        int val= my_strcmp(this->local,p1->local);
        if (val<0) return true;
        if (val>0) return false;
        return ((ptrdiff_t) get_xmlns() < (ptrdiff_t) p1->get_xmlns());
    }

    inline bool equals( ft_custom_cell *p1)
    {
        return (my_strcmp(this->local,p1->local)==0 && ((ptrdiff_t) get_xmlns() == (ptrdiff_t) p1->get_xmlns()));
    }

    inline bool less(const void* p1, const void* p2)
    {
        int val= my_strcmp(local,(char*)p1);
        if (val<0) return true;
        if (val>0) return false;
        return ((ptrdiff_t) get_xmlns() < (ptrdiff_t) p2);
    }

    inline bool equals(const void* p1, const void* p2)
    {
        return (my_strcmp(this->local,(char*)p1)==0 && (ptrdiff_t) get_xmlns() == (ptrdiff_t) p2);
    }
};

typedef sedna_rbtree<ft_custom_cell> ft_custom_tree_t;
typedef std::pair< std::pair<xmlns_ptr,char*>,ft_index_type> ft_index_pair_t;
typedef std::vector< ft_index_pair_t > ft_index_template_t;

struct doc_parser
{
	string_consumer_fn fn;
	void* p;
	doc_parser(string_consumer_fn _fn,void* _p): fn(_fn),p(_p){};
};

struct ft_idx_data
{
	xptr btree_root;
};

typedef struct ft_idx_data ft_idx_data_t;

void delete_ft_custom_tree(ft_custom_tree_t * custom_tree);

struct ft_index_cell_object : public catalog_object
{
/* Common catalog object interface */

    static const int magic = 0x025;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

/* Fields */

    doc_schema_node_xptr schemaroot;
    char * index_title;
    PathExpr *object;
    char* doc_name;
    bool is_doc;
    ft_index_type ftype;
    ft_index_impl impl;
    ft_idx_data_t ft_data;

    xptr serial_root;
    xptr pstr_sequence;

    ft_custom_tree_t * custom_tree;

/* Methods */

    bool fits_to_index(schema_node_cptr snode);

    inline ft_index_cell_object() {};

    inline ft_index_cell_object(
        PathExpr *_object_path, ft_index_type _it,
        const doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
	ft_index_impl _impl = ft_ind_dtsearch
      ) :
        schemaroot(_schemaroot),
        index_title(NULL),
        object(_object_path),
        doc_name(NULL),
        is_doc(_is_doc),
        ftype(_it),
        impl(_impl),
        serial_root(XNULL),
        pstr_sequence(XNULL),
        custom_tree(NULL)
    {
        ft_data.btree_root = XNULL;
        index_title = cat_strcpy(this, _index_title);
        doc_name = cat_strcpy(this, _doc_name);
    };

    ~ft_index_cell_object() {
        cat_free(index_title);
        cat_free(doc_name);

        if (this->custom_tree!=NULL)  delete_ft_custom_tree(this->custom_tree);
    };

    static catalog_object_header * create(
        PathExpr *_object_path, ft_index_type _it,
        const doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        ft_index_impl _impl = ft_ind_dtsearch
      )
    {
        ft_index_cell_object * obj =
          new(cat_malloc(CATALOG_PERSISTENT_CONTEXT, sizeof(ft_index_cell_object)))
          ft_index_cell_object(_object_path, _it, _schemaroot, _index_title, _doc_name, _is_doc, _impl);

        catalog_object_header * header = catalog_create_object(obj);
        catalog_set_name(catobj_ft_indicies, _index_title, header);
        catalog_htable_set(catobj_ft_indicies, _index_title, (_is_doc ? 'D' : 'C'), _doc_name);
        
        return header;
    };

	void update_index(update_history *h);

	void init_serial_tree();
	void destroy_serial_tree();
	doc_serial_header serial_put (xptr& node, xptr& node_indir, op_str_buf& tbuf);
	doc_serial_header serial_get (xptr& node_indir);
	void serial_remove (xptr& node_indir);
	void remove_from_pstr(doc_serial_header& head );
	doc_serial_header serial_update (xptr& node, xptr& node_indir, op_str_buf& tbuf);

	xptr put_buf_to_pstr(op_str_buf& tbuf);
};

ft_index_cell_xptr create_ft_index(
        PathExpr *_object_path, ft_index_type _it,
        doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        ft_index_template_t* _templ, bool just_heap, ft_index_impl _impl
    );

void delete_ft_index (const char *index_title, bool just_heap=false);

ft_index_cell_xptr find_ft_index(const char* title, ftc_index_t *ftc_idx);


//inits metadata library

void ft_index_on_session_begin();
void ft_index_on_session_end();

/* Counted pointer for fulltext index cell.
 *  */

struct ft_index_cell_cptr : public catalog_cptr_template<ft_index_cell_object> {
    explicit inline ft_index_cell_cptr (catalog_object_header * aobj, bool writable = false) :
        catalog_cptr_template<ft_index_cell_object>(aobj, writable) {} ;
    explicit inline ft_index_cell_cptr (const char * index_title, bool write_mode = false) :
        catalog_cptr_template<ft_index_cell_object>(catalog_find_name(catobj_ft_indicies, index_title), write_mode) {};
    inline ft_index_cell_cptr (const xptr p, bool writable = false) :
        catalog_cptr_template<ft_index_cell_object>(p, writable) {};
};


/*
void inline ft_index_sem_down()
{
#ifndef NOSEM
	USemaphoreDown(ft_index_sem, __sys_call_error);
#endif
}
void inline ft_index_sem_up()
{
#ifndef NOSEM
	USemaphoreUp(ft_index_sem, __sys_call_error);
#endif
}
*/

#endif
