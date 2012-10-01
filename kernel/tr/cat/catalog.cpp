/*
 * File: catalog.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/cat/catalog.h"

#include <string.h>

#include "tr/cat/catstore.h"
#include "tr/cat/catvars.h"
#include "tr/cat/catmem.h"

#include "u/ushm.h"
#include "common/xptr/sm_vmm_data.h"
#include "u/uthread.h"
#include "u/usafesync.h"

#include "tr/vmm/vmm.h"
#include "tr/tr_globals.h"
#include "tr/btree/btree.h"
#include "tr/btree/btstruct.h"
#include "tr/cat/simplestream.h"

#include "tr/cat/catjournal.h"
#include "tr/executor/base/XPath.h"
#include "tr/bstrie/sednabtrie.h"

void catalog_issue_warning(const char * warning) {
    elog(EL_WARN, (warning));
}

#define catalog_zero_counter(C) (C) = 0
#define DESERIALIZATION_WARNING_LIMIT 170000
#define catalog_increment_counter(C) if (((C)++) == DESERIALIZATION_WARNING_LIMIT)  \
    { catalog_issue_warning("Too many catalog objects created. Consider using collections for similar documents or smaller transactions."); }

uint64_t deserialized_objects;

struct local_catalog_header * local_catalog;

USemaphore _cat_master_semaphore;
int _cat_master_semaphore_locked;
bool catalog_objects_initialized;



inline catalog_object * __dbg_catalog_preview(xptr &p) { return catalog_acquire_object(p)->object; };

/********************************
 *  catalog interface functions
 */

static
inline char *catalog_ht_fullname_string(enum catalog_named_objects obj_type,
        const char * key, CatalogMemoryContext *context)
{
    size_t len = strlen(key);
    char * result = (char *) cat_malloc_context(context, len + 1 + 1);
    char t = 'A' + (char) obj_type;

    strncpy(result + 1, key, len);
    result[0] = t;
    result[len+1] = 0;

    return result;
}

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

   CATALOG_COMMON_CONTEXT = new CatalogMemoryContext(DEF_CHUNK_SIZE);

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

    delete CATALOG_COMMON_CONTEXT;
    CATALOG_COMMON_CONTEXT = NULL;
    //    printf("\nAllocated : %d / %d\n", allocated_objects, deallocated_objects);
}

/*****************************************************
 * Catalog transaction registering and unregistering
 */

void initialize_masterblock()
{
    elog(EL_LOG, ("Initializing catalog masterdata block"));

    for (int i = 0; i < catobj_count; i++) {
        local_catalog->masterdata.trees[i] = bt_create(xs_string);
    }
    local_catalog->masterdata.htable = XNULL;

    vmm_alloc_data_block(&(catalog_masterblock));
    WRITEP(catalog_masterblock);

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

    cs_initp();
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
    if (!is_commit) { return; }

    try {
        catalog_journal_record *r;
        xptr *tree;
        bt_key name;
        char * htname;

        catalog_validate_objects();

        catalog_lock_metadata();

        cs_initp();
        r = local_catalog->catalog_journal;
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
                htname = catalog_ht_fullname_string(r->nor.object_type, r->nor.name_to_save->name, NULL);
                local_catalog->masterdata.htable = sbtrie_delete_str(local_catalog->masterdata.htable, htname);
                free(htname);
                break;

              case catalog_journal_record::add_htable_record:
                local_catalog->masterdata.htable = sbtrie_insert_str(local_catalog->masterdata.htable, r->htr.name, r->htr.data, strlen(r->htr.data) + 2, true);
                break;
            }

            r = r->next;
            local_catalog->masterdata_updated = true;
        }

        local_catalog->catalog_journal = NULL;

        if (local_catalog->masterdata_updated) {
            WRITEP(catalog_masterblock);

            memcpy(
                &(((catalog_master_record *) XADDR(catalog_masterblock))->masterdata),
                &(local_catalog->masterdata),
                sizeof(catalog_name_trees));

            local_catalog->masterdata_updated = false;
        }

        if (!tr_globals::is_ro_mode) {
            WRITEP(catalog_masterblock);

            memcpy(
                &(((catalog_master_record *) XADDR(catalog_masterblock))->last_nid_size),
                &last_nid_size,
                sizeof(int));

            memcpy(
                ((catalog_master_record *) XADDR(catalog_masterblock))->last_nid,
                last_nid,
                last_nid_size);
        }
    } catch (ANY_SE_EXCEPTION) {
        catalog_unlock_metadata();
        throw;
    }
}

void catalog_after_commit(bool is_commit)
{
    cs_initp();
    catalog_unlock_metadata();
}

void catalog_update_nid();

void catalog_on_transaction_begin()
{
    CATALOG_TEMPORARY_CONTEXT = new CatalogMemoryContext(DEF_CHUNK_SIZE);
    local_space_base = cat_malloc_context_(CATALOG_TEMPORARY_CONTEXT, 32);
    catalog_space_base = local_space_base;

    cs_initp();

    catalog_zero_counter(deserialized_objects);

    SafeMetadataSemaphore cat_masterlock;
    VMMMicrotransaction mtrn;

    mtrn.begin();
    cat_masterlock.Aquire();

    local_catalog = new local_catalog_header;

    if (catalog_masterblock == XNULL) {
        initialize_masterblock();
    }

    last_nid = (unsigned char *) cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, MAX_ROOT_NID_SIZE);

    catalog_update_nid();

    cat_masterlock.Release();
    mtrn.end();
}

void catalog_on_transaction_end(bool is_commit)
{
    elog(EL_DBG, ("Catalog objects deserialized : %d", deserialized_objects));

    delete local_catalog;
    local_catalog = NULL;

    delete CATALOG_TEMPORARY_CONTEXT;
    CATALOG_TEMPORARY_CONTEXT = NULL;
}

catalog_object_header * catalog_object_header::invalidate() {
    if (IS_CATALOG_TMP_PTR(this->p) || GET_FLAG(this->flags, CAT_OBJECT_INVALID_FLAG))
        return this;

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
        U_ASSERT(CATALOG_TEMPORARY_CONTEXT);
        obj = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_object_header))) catalog_object_header(XNULL);
        object->p_object.layer = CHUNK2TEMP_CAT_LAYER(CATALOG_TEMPORARY_CONTEXT->addr2chunk(obj));
        object->p_object.setOffs(CATALOG_TEMPORARY_CONTEXT->addr2offs(obj));
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
    if (IS_CATALOG_TMP_PTR(ptr))
        return (catalog_object_header *)(CATALOG_TEMPORARY_CONTEXT->offs2addr(TEMP_CAT_LAYER2CHUNK(ptr.layer), ptr.getOffs()));

    /* look up for a catalog header, corresponding to the xpointer in hash table */
    catalog_object_header * obj =
      (catalog_object_header *) local_catalog->xptrhash.get(ptr);

    /* if not found in hash table, try to deserialize pointer */
    if (obj == NULL) {
        obj = new (cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(catalog_object_header))) catalog_object_header(ptr);
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
    object->object = NULL;
    CLEAR_FLAG(object->flags, CAT_OBJECT_INVALID_FLAG);
}

static void catalog_delete_object_internal(catalog_object_header * object)
{
    U_ASSERT(object->object != NULL);
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

void catalog_update_nid()
{
    catalog_update_metadata();

    memcpy(
        &last_nid_size,
        &(((catalog_master_record *) XADDR(catalog_masterblock))->last_nid_size),
        sizeof(int));

    memcpy(
        last_nid,
        ((catalog_master_record *) XADDR(catalog_masterblock))->last_nid,
        last_nid_size);
}

inline xptr catalog_nametree_find_name(const xptr &tree, const char * name)
{
    SafeMetadataSemaphore lock;
    VMMMicrotransaction mtrn;

    bt_key k;
    xptr obj;

    mtrn.begin();
    lock.Aquire();

    catalog_update_metadata();

    if (tree == XNULL) { return XNULL; }

    k.setnew(name);

    bt_cursor c = bt_find(tree, k);
    obj = c.bt_next_obj();

    lock.Release();
    mtrn.end();

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

    cr = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_journal_record))) catalog_journal_record;
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

    cr = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_journal_record))) catalog_journal_record;
    cr->type = catalog_journal_record::del_name;
    cr->nor.object_type = obj_type;
    cr->nor.name_to_save = n;

    local_catalog->add_journal_record(cr);

    return true;
}

inline char * catalog_htable_find_name(const char * name)
{
    if (local_catalog->masterdata.htable == XNULL) { return NULL; }

    SafeMetadataSemaphore lock;
    VMMMicrotransaction mtrn;

    xptr obj;

    mtrn.begin();
    lock.Aquire();

    catalog_update_metadata();

    obj = sbtrie_find_str(local_catalog->masterdata.htable, name);

    if (obj == XNULL) {
        return NULL;
    }

    size_t object_len = btrie_get_object(obj, NULL);
    char * result = new char[object_len];
    btrie_get_object(obj, result, object_len);

    lock.Release();
    mtrn.end();

    return result;
}


/*
 * This function allocates result using 'new' since it will be dealloced
 * by counted_ptr
 */
char * catalog_htable_get(enum catalog_named_objects obj_type,
                          const char * key)
{
    char * result = NULL;
    char * fullname;
    catalog_name_record * cache_result;
    catalog_journal_record *r;

    cache_result = catalog_cachetree_find_name(obj_type, key);

    if (cache_result != NULL) {
        if (cache_result->name_deleted || (GET_FLAG(cache_result->obj->flags, CAT_OBJECT_DELETED_FLAG))) {
            return NULL;
        }

        r = local_catalog->catalog_journal;
        fullname = catalog_ht_fullname_string(obj_type, key, NULL /* use malloc */);

        while (r != NULL) {
            if ((r->type == catalog_journal_record::add_htable_record) &&
                  (strcmp(r->htr.name, fullname) == 0)) {
                size_t object_len = strlen(r->htr.data);
                result = new char[object_len + 2];
                memcpy(result, r->htr.data, object_len + 2);

                break;
            }
            r = r->next;
        }

        free(fullname);
        if (result != NULL) { return result; }
    }

    U_ASSERT(result == NULL);

    fullname = catalog_ht_fullname_string(obj_type, key, NULL /* use malloc */);
    result = catalog_htable_find_name(fullname);
    free(fullname);

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

    cr = new (cat_malloc_context(CATALOG_TEMPORARY_CONTEXT, sizeof(catalog_journal_record))) catalog_journal_record;
    cr->type = catalog_journal_record::add_htable_record;
    cr->htr.object_type = obj_type;
    cr->htr.name = catalog_ht_fullname_string(obj_type, key, CATALOG_TEMPORARY_CONTEXT);

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


/*
 * Returns document/collection the provided object provided
 * (index, ft-index, etc) is created on. Throws SE1061 if
 * object hasn't been found.
 */
counted_ptr<db_entity>
find_db_entity_for_object(enum catalog_named_objects obj_type,
                          const char* title)
{
    SafeMetadataSemaphore lock;
    char t;

    lock.Aquire();

    if (!catalog_name_exists(obj_type, title)) {
        throw USER_EXCEPTION2(SE1061, (std::string(object_type2c_str(obj_type)) + " '" + title + "'").c_str());
    }

    counted_ptr<db_entity> result(new db_entity);
    /* catalog_htable_get allocates memory which must be freed by counted ptr*/
    result->name = catalog_htable_get(obj_type, title);
    size_t len = strlen(result->name);

    lock.Release();

    t = result->name[len + 1];
    if (t == 'D') {
        result->type = dbe_document;
    } else {
        result->type = dbe_collection;
    }

    return result;
}
