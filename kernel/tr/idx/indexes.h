/*
 * File:  indexes.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __INDEXES_H
#define __INDEXES_H

#include "common/sedna.h"

#include "tr/idx/index_data.h"
#include "tr/executor/base/XPath.h"
#include "tr/executor/base/tuple.h"
#include "tr/idx/btree/btree.h"
#include "tr/vmm/vmm.h"


/* creates bt_key (fills key argument) from tuple_cell */
bt_key& tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key);

/* creates tuple_cell (fills tc argument) from bt_key */
//void bt_key2tuple_cell(const bt_key& /*in*/ key, tuple_cell& /*out*/ tc);

/* the creation of index with the following paths in data and with the following key_type connected to the xml document or xml collection identified by the descriptive schema root node*/
index_cell_xptr create_index (PathExpr *object_path,
                          PathExpr *key_path,
                          xmlscm_type key_type,
                          doc_schema_node_cptr schemaroot,
                          const char * index_title,
                          const char* doc_name,
                          bool is_doc);

void delete_index (const char *index_title);

/* sets the binding between the input schema_node and indexes */
//void set_schema_node_binding(schema_node* schema);

/* inserts the following node modified to key together with some objects to the all indexes needed */
//void insert_node_to_indexes (xptr node);

/* deletes the following node modified to key  together with some objects from all the indexes needed */
//void delete_node_from_indexes (xptr node);

xptr find_btree(const char* title);

xmlscm_type get_index_xmlscm_type(const char* title);


////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////
/// SORT values before index creation
////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////

class idx_buffer
{
private:
    char* internal_buffer;
    int   buffer_lgth;

public:
    idx_buffer (): internal_buffer(NULL) , buffer_lgth(0) {};
    ~idx_buffer ()
    {
        if (buffer_lgth) {
            delete[] internal_buffer;
            internal_buffer = NULL;
        }
    };

    inline void copy_to_buffer(xptr addr, shft size)
    {
       CHECKP(addr);
       copy_to_buffer(XADDR(addr),size);
    }

    inline void copy_to_buffer(xptr addr, shft shift,shft size)
    {
        CHECKP(addr);
        copy_to_buffer(XADDR(addr),shift,size);
    }

    inline char* get_buffer_pointer()
    {
        return internal_buffer;
    }

    void copy_data_ser_to_buffer(xptr v1,int sz);
    void copy_to_buffer(const void* addr, shft size);
    void copy_to_buffer(const void* addr, shft shift,shft size);
    void copy_from_buffer(xptr addr, shft shift,shft size);
    void expand_to_size(int size);
};

struct idx_user_data
{
private:
    char* temps[2];
    int   sizes[2];

public:

    idx_user_data()
    {
        temps[0] = NULL;
        temps[1] = NULL;
        sizes[0] = 0;
        sizes[1] = 0;
    }

    ~idx_user_data()
    {
        if(buf != NULL)   { delete buf; buf = NULL; }
        if(sizes[0]) { delete[] temps[0]; sizes[0] = 0; }
        if(sizes[1]) { delete[] temps[1]; sizes[1] = 0; }
    }

    char* make_sure_temp_size(int n, int size);

    xmlscm_type t;         /// Type of the index
    idx_buffer* buf;       /// Pointer to the buffer used to serialize/deserialize
};


/// Functions needed by sorted_sequence

int    idx_compare_less       (xptr v1,xptr v2, const void * Udata);
int    idx_get_size           (tuple& t, const void * Udata);
void   idx_serialize          (tuple& t,xptr v1, const void * Udata);
void   idx_serialize_2_blks   (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
void   idx_deserialize        (tuple &t, xptr& v1, const void * Udata);
void   idx_deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);

////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////


#endif

