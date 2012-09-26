/*
 * File: catptr.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CATXPTR
#define _CATXPTR

#include "common/xptr/xptr.h"
#include "tr/cat/catalog.h"

/** For information on catalog pointers, please refer to catalog.h */

template <class T>
struct catalog_fast_xptr : public xptr {
    inline catalog_fast_xptr() : xptr() {};
    inline catalog_fast_xptr(const xptr& x) : xptr(x) {};

    inline T * modify() { return (T *) catalog_acquire_object(*this)->object->modify_self(); };

    inline T * operator ->() const {
        return (T *) (catalog_acquire_object(*this)->object);
    };

    inline T * get_object() const {
        return (T *) (catalog_acquire_object(*this)->object);
    }
};


template <class T>
struct catalog_cptr_template : public catalog_cptr {
    explicit inline catalog_cptr_template (catalog_object_header * aobj, bool writable = false) : catalog_cptr(aobj, writable) {} ;
    inline catalog_cptr_template () : catalog_cptr(XNULL) {} ;
    inline catalog_cptr_template (const xptr p, bool writable = false) : catalog_cptr(p, writable) {} ;

    inline catalog_cptr & operator = (const catalog_cptr_template<T> & a)
        { return (* (catalog_cptr *) this) = a; };
/*
    inline catalog_cptr & operator = (const xptr & a)
        { return (* (catalog_cptr *) this) = a; };
*/
    inline bool operator == (const catalog_cptr_template<T> & a) const
        { return ((obj == NULL) && (a.obj == NULL)) || ((obj != NULL) && (a.obj != NULL) && (obj->p == a.obj->p)); };

    inline T * modify() { _modify(); return (T *) obj->object; };
    inline T * operator ->() { return (T *) obj->object; };
    inline T & operator *() { return * ((T *) obj->object); };

    inline const T * operator ->() const { return (const T *) obj->get_object(); };
};


struct schema_node_object;
struct doc_schema_node_object;
struct col_schema_node_object;
struct metadata_cell_object;
struct index_cell_object;
#ifdef SE_ENABLE_TRIGGERS
struct trigger_cell_object;
#endif
#ifdef SE_ENABLE_FTSEARCH
struct ft_index_cell_object;
#endif

typedef catalog_cptr_template<schema_node_object> schema_node_cptr;
typedef catalog_cptr_template<doc_schema_node_object> doc_schema_node_cptr;
typedef catalog_cptr_template<col_schema_node_object> col_schema_node_cptr;

typedef catalog_fast_xptr<schema_node_object> schema_node_xptr;
typedef catalog_fast_xptr<doc_schema_node_object> doc_schema_node_xptr;
typedef catalog_fast_xptr<col_schema_node_object> col_schema_node_xptr;
typedef catalog_fast_xptr<index_cell_object> index_cell_xptr;
typedef catalog_fast_xptr<metadata_cell_object> metadata_cell_xptr;
#ifdef SE_ENABLE_TRIGGERS
typedef catalog_fast_xptr<trigger_cell_object> trigger_cell_xptr;
#endif
#ifdef SE_ENABLE_FTSEARCH
typedef catalog_fast_xptr<ft_index_cell_object> ft_index_cell_xptr;
#endif

struct xmlns_indb_object;
typedef catalog_fast_xptr<xmlns_indb_object> xmlns_ptr_pers;

#endif
