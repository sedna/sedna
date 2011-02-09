/*
 * File:  index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>
#include <sstream>

#include "tr/idx/indecies.h"

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#include "tr/structures/schema.h"
#include "tr/executor/fo/op_map.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/cat/catstore.h"
#include "tr/mo/modebug.h"
#include "tr/log/log.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/structures/nodeutils.h"

#include "tr/idx/bstrieindex.h"
#include "tr/idx/btreeindex.h"

using namespace std;

static bool index_initialized = false;

#ifdef _WIN32
#define strcasecmp lstrcmpiA
#endif /* _WIN32 */

index_backend_t str2index_type(const char *str)
{
    if (0 == strcasecmp(str, "btree")) {
        return index_btree;
    } else if (0 == strcasecmp(str, "bstrie")) {
        return index_bstrie;
    } else {
        throw USER_EXCEPTION(SE4084);
    }
}

//inits metadata library
void index_on_session_begin()
{
    index_initialized = true;
}

void index_on_session_end()
{
//    U_ASSERT(index_initialized);
    index_initialized = false;
}

index_cell_object::index_cell_object(index_descriptor_t* index_dsc):
  index_title(NULL), owner(index_dsc->owner),
  keytype(index_dsc->keytype), entry_point(XNULL),
  backend_type(index_dsc->backend_type),
  object(index_dsc->object), key(index_dsc->key),
  err_cntr(0), backend(NULL)
{
    index_title = cat_strcpy(this, index_dsc->index_title);
}


void index_cell_object::serialize_data(se_simplestream &stream)
{
    U_ASSERT(owner != XNULL);
    cs_set_hint(owner);

    stream.write(&keytype, sizeof(xmlscm_type));
    stream.write(&backend_type, sizeof(index_backend_t));
    stream.write(&owner, sizeof(doc_schema_node_xptr));

    /* Look for updated entry point  */
    if (backend != NULL) {
        entry_point = get_backend()->getEntryPoint();
    }

    stream.write(&entry_point, sizeof(xptr));
    stream.write(&err_cntr, sizeof(int));

    stream.write_string(index_title);

    std::ostringstream obj_str(std::ios::out | std::ios::binary);
    std::ostringstream key_str(std::ios::out | std::ios::binary);

    PathExpr2lr(object, obj_str);
    PathExpr2lr(key, key_str);

    stream.write_string(obj_str.str().c_str());
    stream.write_string(key_str.str().c_str());
};

void index_cell_object::deserialize_data(se_simplestream &stream)
{
    stream.read(&keytype, sizeof(xmlscm_type));
    stream.read(&backend_type, sizeof(index_backend_t));
    stream.read(&owner, sizeof(doc_schema_node_xptr));
    stream.read(&entry_point, sizeof(xptr));
    stream.read(&err_cntr, sizeof(int));

    index_title = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, index_title);

    char * obj_str = NULL;
    char * key_str = NULL;
    se_size_t len;

    if ((len = stream.read_string_len()) != 0)
        obj_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, obj_str);
    object = lr2PathExpr(NULL, obj_str, pe_catalog_aspace);
    free(obj_str);

    if ((len = stream.read_string_len()) != 0)
        key_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, key_str);
    key = lr2PathExpr(NULL, key_str, pe_catalog_aspace);
    free(key_str);
};

void index_cell_object::drop()
{
    get_root_node().modify()->delete_index(p_object);
    catalog_delete_name(catobj_indicies, this->index_title);
    get_backend()->dropTree();
    cs_free(p_object);
    catalog_delete_object(this);
};

index_cell_object::~index_cell_object()
{
    delete backend;
}

catalog_object_header* index_cell_object::create(index_descriptor_t* index_dsc)
{
    index_cell_object * obj = new(cat_malloc_context(CATALOG_PERSISTENT_CONTEXT, sizeof(index_cell_object)))
      index_cell_object(index_dsc);

    switch (index_dsc->backend_type) {
      case index_bstrie: obj->backend = idx::BSTrieMultimap::createIndex(); break;
      case index_btree: obj->backend = idx::BTreeMultimap::createIndex(index_dsc->keytype); break;
    }

    obj->entry_point = obj->backend->getEntryPoint();

    catalog_object_header * header = catalog_create_object(obj);
    metadata_cell_cptr owner = index_dsc->owner;

    catalog_set_name(catobj_indicies, index_dsc->index_title, header);
    catalog_htable_set(catobj_indicies, index_dsc->index_title, (owner->is_document() ? 'D' : 'C'), owner->get_name());

    return header;
};

#define FOR_EACH(i, l, t) for (t::iterator i = l.begin(); i != l.end(); i++)

void index_cell_object::on_schema_node_created(schema_node_cptr snode) const
{
    t_scmnodes res;
    t_scmnodes objs = execute_abs_path_expr(snode->root, object);
    const NodeTest node_test_nodes_deep = {axis_descendant, node_test_wildcard_star};

    FOR_EACH(i, objs, t_scmnodes) {
        if ((*i)->is_ancestor_or_self(snode)) {
            t_scmnodes keys=execute_abs_path_expr(*i, key);
            FOR_EACH(j, keys, t_scmnodes) {
                if (snode.ptr() == *j) {
                    snode->index_list->add(index_ref(this->p_object, *i, *j));
                    break;
                }
                t_scmnodes keydeps = execute_node_test(*i, node_test_nodes_deep);
                FOR_EACH(k, keydeps, t_scmnodes) {
                    if (snode.ptr() == *j) {
                        snode->index_list->add(index_ref(this->p_object, *i, *j));
                        break;
                    }
                }
            }
        }
    }
}

idx::KeyValueMultimap* index_cell_object::get_backend()
{
    if (backend == NULL) {
        switch (backend_type) {
          case index_bstrie: backend = idx::BSTrieMultimap::openIndex(entry_point); break;
          case index_btree: backend = idx::BTreeMultimap::openIndex(entry_point); break;
        }
    }
    return backend;
}

void index_cell_object::put_to_index(xptr key_node, xptr object_indir)
{
    idx::KeyValueMultimap * index_backend = get_backend();
    tuple_cell tc = dm_typed_value(key_node);

    if (!((getNodeType(checkp(key_node)) == element) && (tc.get_strlen() == 0))) {
        try {
            tc = cast(tc, keytype);
        } catch (SednaUserException) {
            // TODO: Check for error code!
            static_cast<index_cell_object *>(this->modify_self())->err_cntr++;
            return;
        }

        index_backend->insertPair(tc, tuple_cell::safenode_indir(object_indir));

        if (index_backend->getEntryPoint() != entry_point) {
            this->modify_self(); /* New entry point will be set on serialization */
        }
    }
}

void index_cell_object::delete_from_index(xptr key_node, xptr object_indir)
{
    idx::KeyValueMultimap * index_backend = get_backend();
    tuple_cell tc = dm_typed_value(key_node);

    if (!((getNodeType(checkp(key_node)) == element) && (tc.get_strlen() == 0))) {
        try {
            tc = cast(tc, keytype);
        } catch (SednaUserException) {
            // TODO: Check for error code!
            static_cast<index_cell_object *>(this->modify_self())->err_cntr--;
            return;
        }
        index_backend->deletePair(tc, tuple_cell::safenode_indir(object_indir));

        if (index_backend->getEntryPoint() != entry_point) {
            this->modify_self(); /* New entry point will be set on serialization */
        }
    }
}


/***********************************************************************************************
 * Index creation
 */

/****************************************************
  Utilites for sorting values before index creation
*/

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

/* Throws invalid index type exception if we try to create index
 * with unsupported type */

static void inline
check_index_key_type(xmlscm_type type) {
    if (type != xs_integer && type != xs_float && type != xs_double && type != xs_string && type != xs_date &&
        type != xs_dateTime && type != xs_time && type != xs_yearMonthDuration && type != xs_dayTimeDuration)
        throw USER_EXCEPTION2(SE2034, xmlscm_type2c_str(type));
}

int    idx_compare_less       (xptr v1,xptr v2, const void * Udata);
int    idx_get_size           (tuple& t, const void * Udata);
void   idx_serialize          (tuple& t,xptr v1, const void * Udata);
void   idx_serialize_2_blks   (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
void   idx_deserialize        (tuple &t, xptr& v1, const void * Udata);
void   idx_deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);

index_cell_xptr create_index(index_descriptor_t* index_dsc)
{
    int64_t counter1 = 0;
    int64_t counter2 = 0;
    const NodeTest node_test_nodes_deep = {axis_descendant, node_test_wildcard_star};

    // 0. Check index type
    check_index_key_type(index_dsc->keytype);

    // I. Create and fill new index cell
    if (catalog_find_name(catobj_indicies, index_dsc->index_title) != NULL) {
        throw USER_EXCEPTION(SE2033);
    }

    down_concurrent_micro_ops_number();

    index_cell_cptr idc(index_cell_object::create(index_dsc), true);

    doc_schema_node_cptr root_node = index_dsc->owner->get_schema_node();
    root_node.modify()->full_index_list->add(idc.ptr());

    idx_user_data ud;
    ud.t=index_dsc->keytype;
    ud.buf=se_new idx_buffer();
    sorted_sequence * ss = se_new sorted_sequence(idx_compare_less,
      idx_get_size, idx_serialize, idx_serialize_2_blks, idx_deserialize, idx_deserialize_2_blks, &ud);
    tuple tup(2);

    // ALGORITHM: indexing data

    //II. Execute abs path (object_path) on the desriptive schema

    t_scmnodes sobj = execute_abs_path_expr(root_node.ptr(), index_dsc->object);

    //III. For each schema node found (sn_obj)
    for (unsigned int i = 0; i < sobj.size(); i++)
    {
        //IV. Execute path expression (key_path) on the descriptive schema
        t_scmnodes skey = execute_abs_path_expr(sobj[i], index_dsc->key);
        //V. For each schema node found (sn_key)
        for (unsigned int j = 0; j < skey.size(); j++)
        {
            xptr blk;
            //VI. Add pair <&ind,&sn_obj> into schema node (the special list is used)
            skey[j].modify()->index_list->add(index_ref(idc.ptr(), sobj[i], skey[j]));

            t_scmnodes skeydep = execute_node_test(skey[j], node_test_nodes_deep);
            for (t_scmnodes::iterator k = skeydep.begin(); k != skeydep.end(); k++) {
                (*k)->index_list->add(index_ref(idc.ptr(), sobj[i], skey[j]));
            }

            RECOVERY_CRASH;

            blk = getNonemptyBlockLookFore(skey[j]->bblk);
            if (blk != XNULL)
            {
                xptr node_key = getFirstBlockNode(blk);
                //VII. For every descriptor node_key that corresponds to sn_key.
                while (node_key != XNULL)
                {
                    CHECK_TIMER_FLAG;

                    do {
                        tuple_cell tc;

                        try {
                            //VIII. Evaluate key: (cast typed-value(node_key) as key_type)->bt_key
                            tc = cast(dm_typed_value(node_key), index_dsc->keytype);
                        } catch (SednaUserException) {
                            //IX. Increment the counter, which shows the number of
                            //    items that were not inserted because they have
                            //    the type other than the index has
                            idc->err_cntr++;

                            break;
                        }

                        //X. Find descriptor object that corresponds to sn_obj
                        //   schema node and is an ancestor to node_key
                        CHECKP(node_key);
                        xptr obj_indir = getAncestorIndirectionByScheme(node_key, skey[j], sobj[i]);

                        //XI. Create tuple with key and xptr to value object
                        tup.cells[0] = tc;
                        tup.cells[1] = tuple_cell::safenode_indir(obj_indir);

                        //XII. Insert created tuple into sorted sequence.
                        ss->add(tup);
                        counter1++;
                    } while (false);

                    node_key = getNextDescriptorOfSameSort(node_key);
                }
            }
        }
    }

    //XIII. Perfom sorting of the keys in ascending order.
    ss->lazy_sort();

    idx::KeyValueMultimap * index_backend = idc->get_backend();
    index_backend->setSortedInsertionHint(false);

    vmm_init_block_counter();

    while (true)
    {
        ss->next(tup);
        if (tup.is_eos())
        {
            delete ss;
            ss = NULL;
            break;
        }
        else
        {
            try
            {
                //XV. Create and insert key/value pair into index.
                index_backend->insertPair(tup.cells[0], tup.cells[1]);
                counter2++;
            }
            catch (SednaUserException e) {
                // FIXME : we MUST check for a error type here
                if (e.get_code() != SE1003) { throw; }

                //XIV. Increment the counter, which shows the number of
                //     items that were not inserted because they have
                //     the type other than the index has
                idc->err_cntr++;
            }
        }
    }

    elog(EL_INFO, ("Blocks created: %d", vmm_get_block_counter()));

    index_backend->setSortedInsertionHint(false);

    hl_logical_log_index(index_dsc, true);
    up_concurrent_micro_ops_number();
    return idc.ptr();
}

void index_cell_object::get_index_descriptor(index_descriptor_t * dsc) const
{
    dsc->backend_type = backend_type;
    dsc->index_title = index_title;
    dsc->key = key;
    dsc->keytype = keytype;
    dsc->object = object;
    dsc->owner = owner;
}

void drop_index (const char *index_title)
{
    index_cell_cptr idc(index_title, true);
    if (idc.found()) {
        index_descriptor_t index_dsc;
        idc->get_index_descriptor(&index_dsc);
        hl_logical_log_index(&index_dsc, false);
        idc->drop();
    }
}

/*** idx_buffer ***/

void idx_buffer::copy_to_buffer(const void* addr, shft size)
{
    if (size > buffer_lgth)
    {
        if (buffer_lgth)
        {
            delete [] internal_buffer;
        }
        internal_buffer = se_new char[size];
        buffer_lgth = size;
    }
    memcpy(internal_buffer, addr, size);
}

void idx_buffer::copy_from_buffer(xptr addr, shft shift, shft size)
{
    CHECKP(addr);
    VMM_SIGNAL_MODIFICATION(addr);
    memcpy(XADDR(addr), internal_buffer + shift, size);
}

void idx_buffer::expand_to_size(int size)
{
    if (size > buffer_lgth)
    {
        if (buffer_lgth)
        {
            char* buf = se_new char[size];
            memcpy(buf, internal_buffer, buffer_lgth);
            delete [] internal_buffer;
            internal_buffer = buf;
        }
        else
           internal_buffer = se_new char[size];
        buffer_lgth = size;
    }
}

void idx_buffer::copy_to_buffer(const void* addr, shft shift, shft size)
{
    if (size + shift > buffer_lgth)
    {
        if (buffer_lgth)
        {
            char* buf = se_new char[size+shift];
            memcpy(buf, internal_buffer, shift);
            delete [] internal_buffer;
            internal_buffer = buf;
        }
        else
            internal_buffer = se_new char[size+shift];
        buffer_lgth = size + shift;
    }
    memcpy(internal_buffer + shift, addr, size);
}

void idx_buffer::copy_data_ser_to_buffer(xptr v1, int sz)
{
    if (sz > GET_FREE_SPACE(v1))
    {
        copy_to_buffer(v1, GET_FREE_SPACE(v1));
        xptr nblk = ((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
        CHECKP(nblk);
        copy_to_buffer(XADDR(nblk), GET_FREE_SPACE(v1), sz-GET_FREE_SPACE(v1));
    }
    else
    {
        copy_to_buffer(v1, sz);
    }
}

/*** idx_user_data ***/

char* idx_user_data::make_sure_temp_size(int n, int size)
{
    int idx = n - 1;

    if (size > sizes[idx])
    {
        if (sizes[idx])
        {
            char* buf = se_new char[size];
            delete [] temps[idx];
            temps[idx] = buf;
        }
        else
           temps[idx] = se_new char[size];
        sizes[idx] = size;
    }
    return temps[idx];
}

/*******************************************************************************************
* Functions needed by sorted sequence implementation
*/

static tuple_cell get_tc(void* buf, xmlscm_type type, shft size)
{
    switch (type)
    {
        case xs_float                : {float value = *((float*)buf); return tuple_cell(value); }
        case xs_double               : {double value = *((double*)buf); return tuple_cell(value); }
        case xs_integer              : {int64_t value = *((int64_t*)buf); return tuple_cell(value); }
        case xs_string               :
        {
            char* str = se_new char[size+1];
            memcpy(str, (char*)buf, size);
            str[size]='\0';
            return tuple_cell(xs_string, str);
        }
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = *((xs_packed_datetime*)buf); return tuple_cell(value, type);}
        case xs_yearMonthDuration    :
        case xs_dayTimeDuration      : {xs_packed_duration value = *((xs_packed_duration*)buf); return tuple_cell(value, type);}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
}

static void idx_get_size (xptr& v1, xptr& v2, int& s1, int&s2, const void * Udata, xptr& key)
{
    shft sz = 0;
    CHECKP(v1);

    if (GET_FREE_SPACE(v1)<sizeof(shft))
    {
        ((idx_user_data*)Udata)->buf->copy_to_buffer(v1,GET_FREE_SPACE(v1));
        xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
        CHECKP(v);
        ((idx_user_data*)Udata)->buf->copy_to_buffer(v+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(shft)-GET_FREE_SPACE(v1));
        sz=*(shft*)(((idx_user_data*)Udata)->buf->get_buffer_pointer());

        key = *((xptr*)(XADDR(v +
                              sizeof(seq_blk_hdr) +
                              sizeof(shft)-GET_FREE_SPACE(v1))));
        CHECKP(v1);
    }
    else
    {
        sz=*((shft*)XADDR(v1));
        if (GET_FREE_SPACE(v1)<sizeof(shft)+sizeof(xptr))
        {
            char buf[sizeof(xptr)];
            memcpy(buf, XADDR(v1 + sizeof(shft)), GET_FREE_SPACE(v1) - sizeof(shft));
            xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
            CHECKP(v);
            memcpy(buf + GET_FREE_SPACE(v1) - sizeof(shft),
                   XADDR(v + sizeof(seq_blk_hdr)),
                   sizeof(xptr) - (GET_FREE_SPACE(v1) - sizeof(shft)));
            key = *((xptr*)buf);
            CHECKP(v1);
        }
        else
        {
            key = *((xptr*) (XADDR(v1 + sizeof(shft))));
        }
    }
    if (GET_FREE_SPACE(v1)<sizeof(shft)+sizeof(xptr)+sz)
    {
        if (GET_FREE_SPACE(v1)<=sizeof(shft)+sizeof(xptr))
        {
            xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
            v1=v+sizeof(seq_blk_hdr)+(sizeof(shft)+sizeof(xptr)-GET_FREE_SPACE(v1));
            v2=XNULL;
            s1=sz;
            s2=0;
        }
        else
        {
            xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
            s1=GET_FREE_SPACE(v1)-(sizeof(shft)+sizeof(xptr));
            v1=v1+(sizeof(shft)+sizeof(xptr));
            v2=v+sizeof(seq_blk_hdr);
            s2=sz-s1;
        }
    }
    else
    {
        v1=v1+(sizeof(shft)+sizeof(xptr));
        v2=XNULL;
        s1=sz;
        s2=0;
    }
}

static inline xptr idx_get_ptr_to_complete_serialized_data(xptr v, /* out */ char** temp, int n, idx_user_data* ud, /* out */ int& s, /* out */ xptr& key)
{
    CHECKP(v);
    xptr v1 = v;
    xptr v2;
    int s1 = 0, s2 = 0;

    idx_get_size(v1, v2, s1, s2, ud, key);
    s = s1 + s2;

    if(v1 != XNULL && v2 != XNULL)
    {
        *temp = ud->make_sure_temp_size(n, s);
        CHECKP(v1);
        memcpy(*temp, XADDR(v1), s1);
        CHECKP(v2);
        memcpy(*temp + s1, XADDR(v2), s2);
        return XNULL;
    }
    else
    {
        if(v1 == XNULL) return v2;
        else return v1;
    }
}

int idx_compare_less(xptr v1, xptr v2, const void * Udata)
{
    CHECK_TIMER_FLAG;

    idx_user_data* ud = (idx_user_data*)Udata;
    xmlscm_type type = ud->t;

    char* temp1 = NULL;
    char* temp2 = NULL;
    xptr addr1, addr2;
    int s1, s2;

    xptr key1;
    xptr key2;

    addr1 = idx_get_ptr_to_complete_serialized_data(v1, &temp1, 1, ud, s1, key1);
    addr2 = idx_get_ptr_to_complete_serialized_data(v2, &temp2, 2, ud, s2, key2);

    if(addr1 != XNULL) CHECKP(addr1);
    tuple_cell tc1 = get_tc(addr1 != XNULL ? XADDR(addr1) : temp1, type, s1);
    if(addr2 != XNULL) CHECKP(addr2);
    tuple_cell tc2 = get_tc(addr2 != XNULL ? XADDR(addr2) : temp2, type, s2);

    get_binary_op_res r = get_binary_op(xqbop_lt, type, type);

    bool result;
    if (r.collation)
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    else
        result = r.f.bf(tc1, tc2).get_xs_boolean();

    if(result) return -1;
    r = get_binary_op(xqbop_gt, type, type);

    if (r.collation)
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    else
        result = r.f.bf(tc1, tc2).get_xs_boolean();
    if(result) return 1;

    if(key1 < key2) return -1;
    if(key2 < key1) return 1;

    return 0;
}

int idx_get_size (tuple& t, const void * Udata)
{
    xmlscm_type tp=((idx_user_data*)Udata)->t;
    return sizeof(shft)+sizeof(xptr)+((xmlscm_type_size(tp) == 0) ? t.cells[0].get_strlen() : xmlscm_type_size(tp));
}

void idx_serialize (tuple& t,xptr v1, const void * Udata)
{
    CHECK_TIMER_FLAG;

    xmlscm_type type=((idx_user_data*)Udata)->t;
    shft sz=(shft)xmlscm_type_size(type);
    if (!sz)
        sz=t.cells[0].get_strlen();
    CHECKP(v1);
    void * p=XADDR(v1);
    VMM_SIGNAL_MODIFICATION(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(p, &sz, sizeof(shft));
    xptr temp_value = t.cells[1].get_xptr();
    memcpy((char*)p+sizeof(shft), &temp_value, sizeof(xptr));
#else
    *((shft*)p)=sz;
    *((xptr*)((char*)p+sizeof(shft)))=t.cells[1].get_xptr();
#endif
    shft offset=sizeof(shft)+sizeof(xptr);

    tuple_cell& tc=t.cells[0];

    switch (type)
    {
        case xs_float                : {float value = tc.get_xs_float();  memcpy((char*)p+offset, &value, sz); break;}
        case xs_double               : {double value = tc.get_xs_double(); memcpy((char*)p+offset, &value, sz); break;}
        case xs_integer              : {int64_t value = tc.get_xs_integer(); memcpy((char*)p+offset, &value, sz); break;}
        case xs_string               :
        {
            tc = tuple_cell::make_sure_light_atomic(tc);
            WRITEP(v1);
            char* str = tc.get_str_mem();
            memcpy((char*)p+offset, str, sz);
            break;
        }
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = tc.get_xs_dateTime(); memcpy((char*)p+offset, &value, sz); break;}
        case xs_yearMonthDuration    :
        case xs_dayTimeDuration      : {xs_packed_duration value = tc.get_xs_duration(); memcpy((char*)p+offset, &value, sz); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
}

void idx_serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    idx_user_data* ud = (idx_user_data*)Udata;
    idx_buffer* buffer = ud -> buf;
    xmlscm_type type=ud->t;
    shft sz=(shft)xmlscm_type_size(type);
    if (!sz)
        sz=t.cells[0].get_strlen();
    buffer->copy_to_buffer(&sz,sizeof(shft));
    xptr tmp=t.cells[1].get_xptr();
    buffer->copy_to_buffer(&tmp, sizeof(shft),sizeof(xptr));
    shft offset=sizeof(shft)+sizeof(xptr);
    tuple_cell& tc=t.cells[0];

    switch (type)
    {
        case xs_float                : {float value = tc.get_xs_float();  buffer->copy_to_buffer(&value, offset, sz); break;}
        case xs_double               : {double value = tc.get_xs_double(); buffer->copy_to_buffer(&value, offset, sz);  break;}
        case xs_integer              : {int64_t value = tc.get_xs_integer(); buffer->copy_to_buffer(&value, offset, sz); break;}
        case xs_string               :
        {
            buffer->expand_to_size(offset+sz);
            tc = tuple_cell::make_sure_light_atomic(tc);
            char* str = tc.get_str_mem();
            memcpy(buffer->get_buffer_pointer()+offset, str, sz);
            break;
        }
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = tc.get_xs_dateTime(); buffer->copy_to_buffer(&value, offset, sz); break;}
        case xs_yearMonthDuration    :
        case xs_dayTimeDuration      : {xs_packed_duration value = tc.get_xs_duration(); buffer->copy_to_buffer(&value, offset, sz); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }

    buffer->copy_from_buffer(v1, 0, size1);
    buffer->copy_from_buffer(v2, size1, sz+sizeof(shft)+sizeof(xptr)-size1);
}

void idx_deserialize (tuple &t, xptr& v1, const void * Udata)
{
    CHECK_TIMER_FLAG;

    idx_user_data* ud = (idx_user_data*)Udata;
    idx_buffer* buffer = ud -> buf;
    CHECKP(v1);
    shft sz=*((shft*)XADDR(v1));

#ifdef ALIGNMENT_REQUIRED
    xptr v=v1+sizeof(shft);
    buffer->copy_to_buffer(XADDR(v),sizeof(xptr)+sz);
    t.copy(
        get_tc( buffer->get_buffer_pointer()+sizeof(xptr),
                ((idx_user_data*)Udata)->t,
                sz
               ),
        tuple_cell::safenode_indir(*((xptr*)buffer->get_buffer_pointer())));
#else
    xptr v2=v1+sizeof(shft);
    xptr v3=v2+sizeof(xptr);

    tuple_cell key = get_tc( XADDR(v3),((idx_user_data*)Udata)->t,sz);
    t.copy(key, tuple_cell::safenode_indir(*((xptr*)XADDR(v2))));
#endif
}

void idx_deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    idx_user_data* ud = (idx_user_data*)Udata;
    idx_buffer* buffer = ud -> buf;
    xptr vf=v1;
    xptr vs=v2;
    int s1,s2;
    xptr key;
    idx_get_size(vf,vs,s1,s2,Udata,key); /// idx_get_size is used here only to get (s1+s2)! vf and vs are not used further ...
    buffer->copy_to_buffer(v1, size1);
    vs=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
    buffer->copy_to_buffer(vs, size1,s1+s2+sizeof(shft)+sizeof(xptr)-size1);
    t.copy(
            get_tc( buffer->get_buffer_pointer()+sizeof(shft)+sizeof(xptr),
                    ((idx_user_data*)Udata)->t,
                    s1+s2
                   ),
            tuple_cell::safenode_indir(*((xptr*)(buffer->get_buffer_pointer()+sizeof(shft)))));
}

/**********************************************************************************************************************************/
