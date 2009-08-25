/*
 * File: catalog.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/cat/catalog.h"

#include <string.h>

#include "tr/cat/catstore.h"
#include "tr/cat/catvars.h"
#include "tr/cat/catmem.h"

#include "common/u/ushm.h"
#include "common/sm_vmm_data.h"
#include "common/u/uthread.h"
#include "common/u/usafesync.h"

#include "tr/vmm/vmm.h"
#include "tr/idx/btree/btree.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/cat/simplestream.h"

#include "tr/cat/catjournal.h"
#include "tr/executor/base/XPath.h"
#include "tr/bstrie/block_string_tree.h"

void catalog_issue_warning(const char * warning) {
    elog(EL_WARN, (warning));
}

#define catalog_zero_counter(C) (C) = 0
#define DESERIALIZATION_WARNING_LIMIT 170000
#define catalog_increment_counter(C) if (((C)++) == DESERIALIZATION_WARNING_LIMIT)  \
    { catalog_issue_warning("Too many catalog objects created. Consider using collections for similar documents or smaller transactions."); }

uint64_t deserialized_objects;

inline char * catalog_ht_fullname_string(enum catalog_named_objects obj_type, const char * key);

//struct local_catalog_header * local_catalog;
struct local_catalog_header * local_catalog;

USemaphore _cat_master_semaphore;
int _cat_master_semaphore_locked;
bool catalog_objects_initialized;

/*************************************************
 * Catalog session registering and unregistering
 */

void catalog_lock_metadata()
{
    if (_cat_master_semaphore_locked == 0) {
        USemaphoreDown(_cat_master_semaphore, __sys_call_error);
    }
    _cat_master_semaphore_locked++;
}

void catalog_unlock_metadata()
{
    if (_cat_master_semaphore_locked > 0) {
        _cat_master_semaphore_locked--;
        if (_cat_master_semaphore_locked == 0) {
            USemaphoreUp(_cat_master_semaphore, __sys_call_error);
        }
    }
}

void catalog_on_session_begin()
{
    if (USemaphoreOpen(&_cat_master_semaphore, CATALOG_MASTER_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "CATALOG_MASTER_SEMAPHORE_STR");

    _cat_master_semaphore_locked = 0;

    catalog_objects_initialized = true;
}

void catalog_on_session_end()
{
    if (!catalog_objects_initialized) return;

    if (_cat_master_semaphore_locked > 0) {
        USemaphoreUp(_cat_master_semaphore, __sys_call_error);
        _cat_master_semaphore_locked = 0;
    }

    if (USemaphoreClose(_cat_master_semaphore, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4013, "CATALOG_MASTER_SEMAPHORE_STR");

//    printf("\nAllocated : %d / %d\n", allocated_objects, deallocated_objects);
}

/*****************************************************
 * Catalog transaction registering and unregistering
 */

void initialize_masterblock()
{
    elog(EL_LOG, ("Initializing catalog masterdata block"));
    vmm_alloc_data_block(&(catalog_masterblock));
    CHECKP(catalog_masterblock);

    for (int i = 0; i < catobj_count; i++) {
        local_catalog->masterdata.trees[i] = bt_create(xs_string);
    }
    local_catalog->masterdata.htable = XNULL;

    memcpy(
        &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
        &(local_catalog->masterdata),
        sizeof(catalog_name_trees));

    memset(
        ((catalog_master_record *) XADDR(catalog_masterblock))->last_nid,
        0, MAX_ROOT_NID_SIZE);

    memset(
        &(((catalog_master_record *) XADDR(catalog_masterblock))->last_nid_size),
        0, sizeof(int));
}

catalog_journal_record * __catalog_ff(catalog_journal_record * p, enum catalog_named_objects obj_type)
{
    while ((p != NULL) && (p->type != catalog_journal_record::add_name)
          && (p->nor.object_type != obj_type) && p->nor.name_to_save->name_deleted) { p = p->next; }

    return p;
}

void * __catalog_name_enum_init(enum catalog_named_objects obj_type)
{
    return __catalog_ff(local_catalog->catalog_journal, obj_type);
}

void * __catalog_name_enum_next(void * jr, enum catalog_named_objects obj_type)
{
    return __catalog_ff(((catalog_journal_record *) jr)->next, obj_type);
}

char * __catalog_name_enum_get_name(void * jr)
{
    return ((catalog_journal_record *) jr)->nor.name_to_save->name;
}

xptr   __catalog_name_enum_get_object(void * jr)
{
    return ((catalog_journal_record *) jr)->nor.name_to_save->obj->p;
}

void catalog_before_commit(bool is_commit)
{
/*
 * Final journal commit. Done by single transaction at a time
 */
    catalog_lock_metadata();

    try {
        catalog_journal_record *p, *r = local_catalog->catalog_journal;
        xptr *tree;
        bt_key name;
        char * htname;

        while (r != NULL) {
            switch (r->type) {
            case catalog_journal_record::add_name:
                tree = &(CATALOG_NAME_TREE(r->nor.object_type));
                name.setnew(r->nor.name_to_save->name);
                if (*tree == XNULL) { *tree = bt_create(xs_string); }
                bt_insert(*tree, name, r->nor.name_to_save->obj->p);
                break;

            case catalog_journal_record::del_name:
                tree = &(CATALOG_NAME_TREE(r->nor.object_type));
                name.setnew(r->nor.name_to_save->name);
                bt_delete(*tree, name, r->nor.name_to_save->obj->p);
                htname = catalog_ht_fullname_string(r->nor.object_type, r->nor.name_to_save->name);
                st_delete_string(local_catalog->masterdata.htable, htname);
                cat_free(htname);
                break;

            case catalog_journal_record::add_htable_record:
                local_catalog->masterdata.htable = st_insert_string(local_catalog->masterdata.htable, r->htr.name, r->htr.data, strlen(r->htr.data) + 2, true);
                cat_free(r->htr.name);
                cat_free(r->htr.data);
                break;
            }

            p = r->next;
            cat_free(r);
            r = p;
            local_catalog->masterdata_updated = true;
        }

        local_catalog->catalog_journal = NULL;

        if (local_catalog->masterdata_updated) {
            CHECKP(catalog_masterblock);
            VMM_SIGNAL_MODIFICATION(catalog_masterblock);

            memcpy(
                &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
                &(local_catalog->masterdata),
                sizeof(catalog_name_trees));

            local_catalog->masterdata_updated = false;
        }

        if (!ccache_available) {
            CHECKP(catalog_masterblock);
            VMM_SIGNAL_MODIFICATION(catalog_masterblock);

            memcpy(
                &(((catalog_master_record *) XADDR(catalog_masterblock))->last_nid_size),
                &last_nid_size,
                sizeof(int));

            memcpy(
                ((catalog_master_record *) XADDR(catalog_masterblock))->last_nid,
                last_nid,
                last_nid_size);

            cat_free(last_nid);
        }
    } catch (...) {
        catalog_unlock_metadata();
        throw;
    }

    if (pe_catalog_aspace->free_all) pe_catalog_aspace->free_all();

    delete local_catalog;
}

void catalog_after_commit(bool is_commit)
{
    catalog_unlock_metadata();
}

void catalog_on_transaction_begin()
{
    catalog_zero_counter(deserialized_objects);

    SafeMetadataSemaphore cat_masterlock;
    cat_masterlock.Aquire();

    local_catalog = new local_catalog_header;

    if (catalog_masterblock == XNULL) {
        initialize_masterblock();
    } else {
        CHECKP(catalog_masterblock);
        memcpy(
            &(local_catalog->masterdata),
            &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
            sizeof(catalog_name_trees));
    }

    if (!ccache_available) {
        memcpy(
            &last_nid_size,
            &(((catalog_master_record *) XADDR(catalog_masterblock))->last_nid_size),
            sizeof(int));

        last_nid = (unsigned char *) cat_malloc_context(CATALOG_COMMON_CONTEXT, MAX_ROOT_NID_SIZE);

        memcpy(
            last_nid,
            ((catalog_master_record *) XADDR(catalog_masterblock))->last_nid,
            last_nid_size);
    }

    cat_masterlock.Release();
}

void catalog_on_transaction_end(bool is_commit)
{
    // TODO: EXIT IF READONLY!

    elog(EL_DBG, ("Catalog objects deserialized : %d", deserialized_objects));

/*
    if (!is_commit) {
        delete local_catalog;
        return;
    }
*/

    catalog_validate_objects();
}

catalog_object_header * catalog_object_header::invalidate() {
    if (IS_CATALOG_TMP_PTR(this->p) || GET_FLAG(this->flags, CAT_OBJECT_INVALID_FLAG))
        return this;

#ifdef AAADEBUG
    for (catalog_object_header * i = local_catalog->invalid_list; i != NULL; i = i->next_invalid) {
        U_ASSERT(i != this);
    }
#endif

    if (local_catalog->invalid_list == NULL) {
        local_catalog->invalid_list = this;
    }

    if (local_catalog->invalid_list_tail != NULL) {
        local_catalog->invalid_list_tail->next_invalid = this;
    }

    local_catalog->invalid_list_tail = this;
    this->next_invalid = NULL;

    SET_FLAG(flags, CAT_OBJECT_INVALID_FLAG);

    return this;
};

/***********************************
 *  catalog_object implementation
 */

catalog_object * catalog_object::modify_self() const
{
    return (catalog_acquire_object(this->p_object)->invalidate())->object;
}

void catalog_object::serialize()
{
    cs_stream stream(p_object, cs_stream::mode_write);
    int magic = get_magic();
    stream.write(&magic, sizeof(int));
    serialize_data(stream);
    stream.commit();
    if (p_object == XNULL) { p_object = stream.get_xptr(); };
}

void catalog_object::deserialize(xptr p)
{
    cs_stream stream((p == XNULL) ? (p = p_object) : (p_object = p), cs_stream::mode_read);
    stream.seek(4);
    deserialize_data(stream);
}


/********************************
 *  catalog interface functions
 */

catalog_object_header * catalog_create_object(catalog_object * object, bool persistent)
{
    catalog_object_header * obj;

    if (persistent) {
        cs_pushp();
        object->serialize();
        cs_popp();
        obj = new (cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(catalog_object_header))) catalog_object_header(object->p_object);
    } else {
        obj = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_object_header))) catalog_object_header(XNULL);
        object->p_object.addr = obj;
        object->p_object.layer = TEMPORARY_CATALOG_LAYER;
        obj->p = object->p_object;
    }

    local_catalog->header_list.add(obj);
    local_catalog->object_list.add(object);

    obj->object = object;
    obj->invalidate();

    if (persistent) {
        local_catalog->xptrhash.set(obj->p, obj);
    }

    return obj;
}

catalog_object_header * catalog_acquire_object(const xptr &ptr)
{
    U_ASSERT(local_catalog != NULL);

    /* for XNULL xptr return NULL unconditionally */
    if (ptr == XNULL) return NULL;

    /* for temporary schema nodes, return pointer immidiately:
       temporary xptr = TEMPORARY_LAYER (special) + CATALOG HEADER POINTER */
    if (IS_CATALOG_TMP_PTR(ptr)) return (catalog_object_header *) (XADDR(ptr));

    /* look up for a catalog header, corresponding to the xpointer in hash table */
    catalog_object_header * obj =
      (catalog_object_header *) local_catalog->xptrhash.get(ptr);

    /* if not found in hash table, try to deserialize pointer */
    if (obj == NULL) {
        obj = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_object_header))) catalog_object_header(ptr);
        local_catalog->xptrhash.set(ptr, obj);
        obj->object = catalog_deserialize_object(ptr, CATALOG_PERSISTENT_CONTEXT);
        catalog_increment_counter(deserialized_objects);

        local_catalog->header_list.add(obj);
        local_catalog->object_list.add(obj->object);
    }

    /* though, header could be found, it can point to a deleted structure
       in this case, we still should return NULL */
    if (GET_FLAG(obj->flags, CAT_OBJECT_DELETED_FLAG)) {
        return NULL;
    }

//    obj->hit();

    return obj;
}


void catalog_release_object(catalog_object_header * object)
{
    if (GET_FLAG(object->flags, CAT_OBJECT_INVALID_FLAG)) { object->validate(); }
//    object->object->~catalog_object();
//    cat_free(object->object);
    object->object = NULL;
    CLEAR_FLAG(object->flags, CAT_OBJECT_INVALID_FLAG);
}

static void catalog_delete_object_internal(catalog_object_header * object)
{
    U_ASSERT(object->object != NULL);
//    object->object->~catalog_object();
//    object->object = NULL;
    SET_FLAG(object->flags, CAT_OBJECT_DELETED_FLAG);
    object->invalidate();
}

void catalog_delete_object(catalog_object * object)
{
    catalog_delete_object_internal(catalog_acquire_object(object->p_object));
}

void catalog_update_metadata()
{
    CHECKP(catalog_masterblock);
    memcpy(
        &(local_catalog->masterdata),
        &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
        sizeof(catalog_name_trees));
}

inline xptr catalog_nametree_find_name(const xptr &tree, const char * name)
{
    SafeMetadataSemaphore lock;
    bt_key k;
    xptr obj;

    if (tree == XNULL) { return XNULL; }

    lock.Aquire();

    catalog_update_metadata();

    k.setnew(name);

    bt_cursor c = bt_find(tree, k);
    obj = c.bt_next_obj();

    lock.Release();

    return obj;
}


bool catalog_set_name(
        enum catalog_named_objects obj_type,
        const char * name,
        catalog_object_header * obj)
{
    catalog_name_record * n;
    catalog_journal_record * cr;

    n = catalog_cachetree_add_name(obj_type, name, obj);

    cr = new (cat_malloc(local_catalog, sizeof(catalog_journal_record))) catalog_journal_record;
    cr->type = catalog_journal_record::add_name;
    cr->nor.object_type = obj_type;
    cr->nor.name_to_save = n;

    local_catalog->add_journal_record(cr);

    return true;
}

bool __catalog_cache_valid_name(
        enum catalog_named_objects obj_type,
        const char * name)
{
    catalog_object_header * o;
    catalog_name_record * n;

    if ((n = catalog_cachetree_find_name(obj_type, name)) != NULL) {
        if (n->name_deleted) { return false; }
        o = n->obj;
        return (!GET_FLAG(o->flags, CAT_OBJECT_DELETED_FLAG));
    }

    return true;
}

bool catalog_name_exists(
        enum catalog_named_objects obj_type,
        const char * name)
{
    catalog_object_header * o;
    catalog_name_record * n;

    if ((n = catalog_cachetree_find_name(obj_type, name)) != NULL) {
        if (n->name_deleted) { return false; }
        o = n->obj;
        return (!GET_FLAG(o->flags, CAT_OBJECT_DELETED_FLAG));
    }

    if (catalog_nametree_find_name(CATALOG_NAME_TREE(obj_type), name) != XNULL) {
        return true;
    }

    return false;
}


catalog_object_header * catalog_find_name(
        enum catalog_named_objects obj_type,
        const char * name)
{
    catalog_object_header * result = NULL;
    catalog_name_record * n = NULL;

    n = catalog_cachetree_find_name(obj_type, name);

    if (n != NULL) {
        if (n->name_deleted) { return NULL; }
        result = n->obj;
    }

    if (result == NULL) {
        xptr obj = catalog_nametree_find_name(CATALOG_NAME_TREE(obj_type), name);

        if (obj != XNULL) {
            result = catalog_acquire_object(obj);
            catalog_cachetree_add_name(obj_type, name, result);
        }
    } else if (GET_FLAG(result->flags, CAT_OBJECT_DELETED_FLAG)) {
        return NULL;
    }

    return result;
}

bool catalog_delete_name(
        enum catalog_named_objects obj_type,
        const char * name)
{
    catalog_object_header * obj;
    catalog_name_record * n;
    catalog_journal_record * cr;

    obj = catalog_find_name(obj_type, name);

    if (obj == NULL) { throw SYSTEM_EXCEPTION("Catalog name being deleted doesn't exists"); }

    /* We really ADD the same name to cache (although it DOES already exist there)
       We don't care here who is going to set DELETED or RENAMED flag.
     */

    n = catalog_cachetree_add_name(obj_type, name, obj);
    n->name_deleted = true;

    cr = new (cat_malloc(local_catalog, sizeof(catalog_journal_record))) catalog_journal_record;
    cr->type = catalog_journal_record::del_name;
    cr->nor.object_type = obj_type;
    cr->nor.name_to_save = n;

    local_catalog->add_journal_record(cr);

    return true;
}

inline char * catalog_ht_fullname_string(enum catalog_named_objects obj_type, const char * key)
{
    size_t len = strlen(key);
    char * result = (char *) malloc(len + 1 + 1);
    char t = 'A' + (char) obj_type;

    strncpy(result + 1, key, len);
    result[0] = t;
    result[len+1] = 0;

    return result;
}

inline xptr catalog_htable_find_name(const char * name)
{
    if (local_catalog->masterdata.htable == XNULL) { return XNULL; }
    SafeMetadataSemaphore lock;
    xptr obj;

    lock.Aquire();

    CHECKP(catalog_masterblock);
    memcpy(
        &(local_catalog->masterdata),
        &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
        sizeof(catalog_name_trees));

    obj = st_find_string(local_catalog->masterdata.htable, name);

    lock.Release();

    return obj;
}


char * catalog_htable_get(
        enum catalog_named_objects obj_type,
        const char * key)
{
    char * result = NULL;
    xptr object;
    char * fullname;
    catalog_name_record * cache_result;
    catalog_journal_record *r;

    cache_result = catalog_cachetree_find_name(obj_type, key);

    if (cache_result != NULL) {
        if (cache_result->name_deleted || (GET_FLAG(cache_result->obj->flags, CAT_OBJECT_DELETED_FLAG))) {
            return NULL;
        }

        r = local_catalog->catalog_journal;
        fullname = catalog_ht_fullname_string(obj_type, key);

        while (r != NULL) {
            if ((r->type == catalog_journal_record::add_htable_record) &&
                  (strcmp(r->htr.name, fullname) == 0)) {
                size_t object_len = strlen(r->htr.data);
                result = (char *) cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, object_len + 2);
                memcpy(result, r->htr.data, object_len + 2);

                break;
            }
            r = r->next;
        }

        free(fullname);
        if (result != NULL) { return result; }
    }

    U_ASSERT(result == NULL);

    size_t object_len;

    fullname = catalog_ht_fullname_string(obj_type, key);
    object = catalog_htable_find_name(fullname);
    free(fullname);

    if (object == XNULL) {
        return NULL;
    }

    object_len = st_get_object(object, NULL);
    result = (char *) cat_malloc(CATALOG_TEMPORARY_CONTEXT, object_len);
    st_get_object(object, result);

    return result;
}

bool catalog_htable_set(
        enum catalog_named_objects obj_type,
        const char * key,
        char flag,
        const char * par_name)
{
    size_t pname_len;
    catalog_journal_record * cr;

    cr = new (cat_malloc(local_catalog, sizeof(catalog_journal_record))) catalog_journal_record;
    cr->type = catalog_journal_record::add_htable_record;
    cr->htr.object_type = obj_type;
    cr->htr.name = catalog_ht_fullname_string(obj_type, key);

    pname_len = strlen(par_name);
    cr->htr.data = (char *) cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, pname_len+2);
    strncpy(cr->htr.data, par_name, pname_len);
    cr->htr.data[pname_len] = 0;
    cr->htr.data[pname_len+1] = flag;

    local_catalog->add_journal_record(cr);

    return true;
}


xptr catalog_get_names(enum catalog_named_objects obj_type)
{
    return CATALOG_NAME_TREE(obj_type);
}

void catalog_validate_objects()
{
    catalog_object_header * i = local_catalog->invalid_list;
    while (i != NULL) {
        if (GET_FLAG(i->flags, CAT_OBJECT_DELETED_FLAG)) {
        } else if (GET_FLAG(i->flags, CAT_OBJECT_INVALID_FLAG)) {
            i->validate();
        }
        i = i->next_invalid;
    }

    local_catalog->invalid_list = NULL;
}


inline void catalog_object_header::validate() {
    //S_LOCK(lock);
    cs_pushp();
    if (object != NULL) object->serialize();
    cs_popp();
    CLEAR_FLAG(flags, CAT_OBJECT_INVALID_FLAG);
    //S_UNLOCK(lock);
};



inline catalog_object * catalog_object_header::load() {
    if (!isloaded()) { object = catalog_deserialize_object(p, CATALOG_PERSISTENT_CONTEXT); };
    return object;
};

