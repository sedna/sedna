/*
 * File: catalog.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef CATALOG_H
#define CATALOG_H

#include "common/sedna.h"
#include "common/xptr.h"
#include "common/u/uatomic.h"
#include "common/commutil.h"
#include "common/errdbg/d_printf.h"

#include "tr/cat/simplestream.h"

#define CCACHE_XPTR_BUCKETS 256
#define CCACHE_NAME_BUCKETS 128

#define TEMPORARY_CATALOG_LAYER     -2

#define IS_CATALOG_TMP_PTR(x) (x.layer == TEMPORARY_CATALOG_LAYER)

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

/********************************
  Possible catalog lock objects
*/
/*
struct catalog_locks {
    uslock_t name_lock;
    uslock_t hash_lock;
};
*/
/******************************
 * Catalog interface functions
 */

struct catalog_object_header;
struct catalog_object;

catalog_object_header * catalog_create_object(catalog_object * object, bool persistent = true);
catalog_object_header * catalog_acquire_object(const xptr &ptr);
void                    catalog_release_object(catalog_object_header * object);
void                    catalog_delete_object(catalog_object * object);
catalog_object *        catalog_deserialize_object(xptr p, void * context);

/*
  Catalog has extremely complicated name handling.

*/

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
        U_ASSERT(cptr_refcount != 0);
        u_atomic_decrement(cptr_refcount);
//        if (invalid()) cat_free(this, ...);
    };
    inline void ref() { u_atomic_increment(xptr_refcount); };
    inline void deref() { u_atomic_decrement(xptr_refcount); };
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

    inline bool found() {
//        return (obj != NULL);
        return ((obj != NULL) && !(GET_FLAG(obj->flags, CAT_OBJECT_DELETED_FLAG))); // It is a dirty workaround. Should be reconsidered.
    }

    inline xptr ptr() const { return (obj == NULL) ? XNULL : obj->p; }
};


#endif
