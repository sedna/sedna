/*
 * File:  ft_index_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_INDEX_DATA_H
#define _FT_INDEX_DATA_H


#include "common/sedna.h"

#include "tr/strings/strings.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/ft/ft_types.h"
#include "tr/ft/ft_cache.h"
#include "tr/ft/ft_storage.h"
#include "tr/ft/update_history.h"

#include "tr/cat/catptr.h"
#include "tr/cat/catmem.h"

struct PathExpr;

/*
struct doc_parser
{
	string_consumer_fn fn;
	void* p;
	doc_parser(string_consumer_fn _fn,void* _p): fn(_fn),p(_p){};
};*/

void delete_ft_custom_tree(ft_custom_tree_t * custom_tree);
void delete_cust_rules_vector(ft_index_template_t* &v);

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
	char * stemming;
    struct FtsData fts_data;

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
		const char * options
      ) :
        schemaroot(_schemaroot),
        index_title(NULL),
        object(_object_path),
        doc_name(NULL),
        is_doc(_is_doc),
        ftype(_it),
        impl(ft_ind_undefined),
		stemming(NULL),
        fts_data(),
        serial_root(XNULL),
        pstr_sequence(XNULL),
        custom_tree(NULL)
    {
        index_title = cat_strcpy(this, _index_title);
        doc_name = cat_strcpy(this, _doc_name);
		if (options)
			parse_options(options);
		else
			impl = ft_ind_dtsearch;
    };

    ~ft_index_cell_object() {
        if (this->custom_tree!=NULL)  delete_ft_custom_tree(this->custom_tree);
    };

    static catalog_object_header * create(
        PathExpr *_object_path, ft_index_type _it,
        const doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        const char * options
      )
    {
        ft_index_cell_object * obj =
          new(cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(ft_index_cell_object)))
          ft_index_cell_object(_object_path, _it, _schemaroot, _index_title, _doc_name, _is_doc, options);

        catalog_object_header * header = catalog_create_object(obj);
        catalog_set_name(catobj_ft_indicies, _index_title, header);
        catalog_htable_set(catobj_ft_indicies, _index_title, (_is_doc ? 'D' : 'C'), _doc_name);

        return header;
    };

	const char *impl_str();
	void parse_options(const char * options);
	void write_options_str(op_str_buf *buf);
	void serialize_info(xptr left_sib, xptr parent);
	void update_index(update_history *h);

	/*
	void init_serial_tree();
	void destroy_serial_tree();
	doc_serial_header serial_put (xptr& node, xptr& node_indir, op_str_buf& tbuf);
	doc_serial_header serial_get (xptr& node_indir);
	void serial_remove (xptr& node_indir);
	void remove_from_pstr(doc_serial_header& head );
	doc_serial_header serial_update (xptr& node, xptr& node_indir, op_str_buf& tbuf);

	xptr put_buf_to_pstr(op_str_buf& tbuf);*/
};

ft_index_cell_xptr create_ft_index(
        PathExpr *_object_path, ft_index_type _it,
        doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        ft_index_template_t* _templ, bool just_heap, const char * options
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

#endif
