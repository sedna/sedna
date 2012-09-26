/*
 * File: catalog.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef CATALOG_H
#define CATALOG_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "u/uatomic.h"
#include "auxiliary/commutil.h"
#include "common/errdbg/d_printf.h"
#include "auxiliary/counted_ptr.h"

#include "tr/cat/simplestream.h"
#include "tr/cat/catmem.h"

#include "tr/tr_base.h"

/** Catalog objects are heap-stored structures, that may be saved in the physical block memory.
 *
 * They possess several basic properties :
 *  - each catalog object is bind to its "storage canton" (document or collection);
 *  - each object has unique xptr, that never changes, once the object has been created;
 *  - object can be temporary (never stored in blocks) or persistent (is saved to block on the end of transaction);
 *  - some objects have names (see "Catalog name handling" further).
 *
 */

/** Catalog object pointers
 *
 * Catalog object can be accessed either via name (if it has one), or via its pointer (like in schema_node case).
 *
 ** Catalog name handling *************************************************
 *
 *    Each catalog object type, that may be referred by name has it's own global (in scope of the
 *   whole database) name table. In this name table each instance must have the unique name.
 *   Usually, catalog_name_exists() and catalog_find_name() functions should be enough to deal with
 *   names:
 *
 *     index_cell_cptr index;
 *
 *     if (catalog_name_exists(catobj_indicies, index_name)) {
 *         index = catalog_find_name(catobj_indicies, index_name);
 *     }
 *
 *   However this is completely equal to:
 *
 *     index_cell_cptr index = catalog_find_name(catobj_indicies, index_name);
 *
 *    Note, that if you just need to check the object existence, use catalog_name_exists()
 *   for it does not deserialize object, and does not read block with actual cell.
 *
 ** Pointer access (direct pointer: fast_xptr) ****************************
 *
 *    Catalog object xptr in some structures may be extended by catalog fast xptr, like this :
 *
 *     struct node_block_header {
 *      ...
 *      schema_node_xptr snode;
 *      ...
 *     }
 *
 *   This kind of pointer extends usual xptr only by adding overloaded dereference operator, that
 *   dereferences xptr directly to goal catalog object type, like this:
 *
 *     node_block_header * header = getBlockHeader(block);
 *     U_ASSERT(header->snode->has_text()); // has_text() is a method of schema_node_object
 *
 *    Each dereference in this case calls catalog_acquire_object(). It is considered to be a bit slow,
 *   but such code is fast and easy to be written (that's why such pointers are called "fast" in spite
 *   of the fact, they are actually quite "slow" ones).
 *
 *  Note: This pointer is implemented mainly for backward compatibility. However, it should be still
 *        used, when only one short dereference is needed. In other cases it is strongly recommended
 *        to create local _cptr variable and use it.
 *
 ** Pointer access ("counted" pointer : _cptr) *****************************
 *
 *   _cptr is the main type of pointer planned to be used for catalog objects. It is much like any
 *  counted pointer (but it is not actually counted in current implementation). The main difference
 *  between _cptr and the fast_ptr is that _cptr is actually faster: catalog_acquire_object() called
 *  only once for _cptr, and after that direct pointer is stored and used. For example:
 *
 *    // snode is being constructed from xptr, and catalog_acquire_object() is called at this point
 *    schema_node_cptr snode = getBlockHeader(block)->snode;
 *    // Further, snode may be dereferenced like simple pointer
 *    if (snode->has_text()) { strsize += snode->textcnt; }
 *
 *   Each dereference in this case is just a simple pointer dereference without any function calls
 *  or hash searches.
 *
 *  Note: This pointer is not actually counted, so "CPtr" may be considered to stand for "Catalog Pointer".
 *
 *  Note: Both _cptr and fast_xptr pointers do not change current block while being dereferenced
 *        (There is no need to call CHECKP after it).
 *
 ** Modifying catalog objects **********************************************
 *
 *   Members of objects, referenced by both _cptr and fast_xptr can not be simply modified. Each object
 *  instance has a flag, that tells whether or not the object should be written to data blocks. This flag
 *  is set by special modify() function, implemented for both types of pointers. For example:
 *
 *    schema_node_cptr snode = getBlockHeader(block)->snode;
 *    snode->modify();
 *    snode->nodecnt++;
 *
 *  Latter code is equal to:
 *
 *    schema_node_cptr snode = getBlockHeader(block)->snode;
 *    snode->modify()->nodecnt++;
 *
 *  Or even more simple, using fast_xptr:
 *
 *    getBlockHeader(block)->snode->modify()->nodecnt++;
 *
 */


#define CCACHE_XPTR_BUCKETS 256
#define CCACHE_NAME_BUCKETS 128

#define IS_CATALOG_TMP_PTR(x) ((x).layer <= TEMPORARY_CATALOG_LAYER_START)
#define TEMP_CAT_LAYER2CHUNK(layer) (TEMPORARY_CATALOG_LAYER_START - (layer))
#define CHUNK2TEMP_CAT_LAYER(chunk) (TEMPORARY_CATALOG_LAYER_START - (chunk))


/***********************************
  Catalog named object identifiers
*/

enum catalog_named_objects {
    catobj_metadata = 0,
    catobj_indicies = 1,
    catobj_triggers = 2,
    catobj_ft_indicies = 3,
    catobj_count = 4
};

inline const char* object_type2c_str(enum catalog_named_objects obj_type)
{
    switch(obj_type)
    {
    case catobj_metadata:    return "metadata";
    case catobj_indicies:    return "index";
    case catobj_triggers:    return "trigger";
    case catobj_ft_indicies: return "ft-index";
    case catobj_count:       return "count";
    default: throw USER_EXCEPTION2(SE1003, "Unknown catalog named object type");
    }
}

/******************************
 * Catalog interface functions
 */

struct catalog_object_header;
struct catalog_object;

catalog_object_header * catalog_create_object(catalog_object * object, bool persistent = true);
catalog_object_header * catalog_acquire_object(const xptr &ptr);
void                    catalog_release_object(catalog_object_header * object);
void                    catalog_delete_object(catalog_object * object);
catalog_object *        catalog_deserialize_object(xptr p, CatalogMemoryContext *context);

bool                    catalog_set_name(enum catalog_named_objects obj_type, const char * name, catalog_object_header * obj);
catalog_object_header * catalog_find_name(enum catalog_named_objects obj_type, const char * name);
bool                    catalog_delete_name(enum catalog_named_objects obj_type, const char * name);
xptr                    catalog_get_names(enum catalog_named_objects obj_type);
bool                    catalog_name_exists(enum catalog_named_objects obj_type, const char * name);

bool __catalog_cache_valid_name(enum catalog_named_objects obj_type, const char * name);

char *  catalog_htable_get(enum catalog_named_objects obj_type, const char * key);
bool    catalog_htable_set(enum catalog_named_objects obj_type, const char * key, char par_type, const char * par_name);

/**********************************
 * Initialization and finalization
 */

void catalog_lock_metadata();
void catalog_unlock_metadata();

class SafeMetadataSemaphore {
private:
    bool _aquired;
public:
    inline void Aquire() { catalog_lock_metadata(); _aquired = true; };
    inline void Release() { catalog_unlock_metadata(); _aquired = false; };
    inline ~SafeMetadataSemaphore() { if (_aquired) { catalog_unlock_metadata(); } };
    inline SafeMetadataSemaphore() : _aquired(false) { };
};

void catalog_on_session_begin();
void catalog_on_session_end();

void catalog_on_transaction_begin();
void catalog_on_transaction_end(bool is_commit);

void catalog_validate_objects();

void catalog_before_commit(bool is_commit);
void catalog_after_commit(bool is_commit);

// Structures

struct shared_catalog_header;
struct local_catalog_header;

struct catalog_name_record;
struct catalog_object_header;
struct catalog_object;

/***************************
 * Catalog headers
 */

//extern struct shared_catalog_header * shared_catalog;
extern struct local_catalog_header  * local_catalog;

/**************************
 * Abstract catalog object
 */

struct catalog_object {
    xptr    p_object;

    void serialize();
    void deserialize(xptr p);

    virtual void serialize_data(se_simplestream &stream) = 0;
    virtual void deserialize_data(se_simplestream &stream) = 0;
    virtual int get_magic() = 0;
    virtual void drop() = 0;

    virtual ~catalog_object() {};

    inline catalog_object() : p_object(XNULL) {};

    catalog_object * modify_self() const;
};

// # define IS_CAT_OBJ_NEW()
#define CAT_OBJECT_INVALID_FLAG 0x01
#define CAT_OBJECT_DELETED_FLAG 0x02

struct catalog_object_header {
    xptr p;
    catalog_object * object;
    int flags;
    int cptr_refcount;
    int xptr_refcount;

    inline const catalog_object * get_object() const { return object; };

    catalog_object_header * next;
    catalog_object_header * prev;
    catalog_object_header * next_invalid;
    catalog_object_header * latest_version;

    catalog_object_header(const xptr ap) :
        p(ap), object(NULL), flags(0),
        cptr_refcount(0), xptr_refcount(0),
        next(NULL), prev(NULL)
    { /* U_ASSERT(ap != XNULL) */ };

    void validate();

    inline catalog_object * load();

    catalog_object_header * invalidate();
    inline void lock() { /* u_atomic_increment(cptr_refcount); */ };
    inline void unlock() {
        /* U_ASSERT(cptr_refcount != 0); */
        /* u_atomic_decrement(cptr_refcount); */
//        if (invalid()) cat_free(this, ...);
    };
    inline void ref() { ++xptr_refcount; };
    inline void deref() { --xptr_refcount; };
    inline bool isloaded() { return object != NULL; };
};

/*
 * Counted pointer for catalog cache cell.
 */

struct catalog_cptr {
    catalog_object_header * obj;

    catalog_cptr (const catalog_cptr &a) : obj(a.obj) {
        if (obj != NULL) obj->lock();
    }

    ~catalog_cptr() { if (obj != NULL) obj->unlock(); };

    catalog_cptr & operator = (const catalog_cptr & a) {
        if (&a != this) {
            if (obj != NULL) obj->unlock();
            obj = a.obj;
            if (obj != NULL) obj->lock();
        };
        return *this;
    }

    catalog_cptr & operator = (const xptr &a) {
        if ((obj == NULL) || (a != obj->p)) {
            if (obj != NULL) { obj->unlock(); }
            obj = (a == XNULL) ? NULL : catalog_acquire_object(a);
            if (obj != NULL) { obj->lock(); }
        };
        return *this;
    }

    inline void renew() {
        if (obj != NULL) {
          obj->unlock();
          obj = obj->latest_version;
          obj->lock();
        }
    }

    inline explicit catalog_cptr (catalog_object_header * aobj, bool write_mode = false) {
        obj = aobj;

        if (obj != NULL) {
            if (write_mode) { obj = obj->invalidate(); }
            obj->lock();
        }
    }

    inline explicit catalog_cptr (const xptr p, bool write_mode = false) {
        obj = (p == XNULL) ? NULL : catalog_acquire_object(p);

        if (obj != NULL) {
            if (write_mode) { obj = obj->invalidate(); }
            obj->lock();
        }
    }

    inline void _modify() {
        if (obj != NULL) {
            obj->unlock();
            obj = obj->invalidate();
            obj->lock();
        }
    }

    inline bool found() const {
//        return (obj != NULL);
        return ((obj != NULL) && !(GET_FLAG(obj->flags, CAT_OBJECT_DELETED_FLAG))); // It is a dirty workaround. Should be reconsidered.
    }

    inline xptr ptr() const { return (obj == NULL) ? XNULL : obj->p; }
};


/**
 * Returns document/collection the provided object provided
 * (index, ft-index, etc) is created on. Throws SE1061 if
 * object hasn't been found.
 */
counted_ptr<db_entity>
find_db_entity_for_object(enum catalog_named_objects obj_type,
                          const char* title);


#endif /* CATALOG_H */
