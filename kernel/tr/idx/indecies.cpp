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
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"

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

    stream.write_string(object->toLRString().c_str());
    stream.write_string(key->toLRString().c_str());
};

void index_cell_object::deserialize_data(se_simplestream &stream)
{
    setDefaultSpace(catalog_space_base);

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
    object = new xpath::PathExpression(obj_str, NULL);
    free(obj_str);

    if ((len = stream.read_string_len()) != 0)
        key_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, key_str);
    key = new xpath::PathExpression(key_str, NULL);
    free(key_str);

    popDefaultSpace();
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
    t_scmnodes objs;
    executePathExpression(snode->root, *object, &objs, NULL, NULL);

    const xpath::NodeTest node_test_nodes_deep(xpath::axis_descendant, xpath::node_test_wildcard_star);

    FOR_EACH(i, objs, t_scmnodes) {
        if ((*i)->is_ancestor_or_self(snode)) {
            t_scmnodes keys;
            executePathExpression(*i, *key, &keys, NULL, NULL);
            FOR_EACH(j, keys, t_scmnodes) {
                if (snode.ptr() == *j) {
                    snode->index_list->add(index_ref(this->p_object, *i, *j));
                    break;
                }
                t_scmnodes keydeps;
                executeNodeTest(*i, node_test_nodes_deep, &keydeps, NULL, NULL);
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

/* Throws invalid index type exception if we try to create index
 * with unsupported type
 * At this moment:
 * B-tree supports xs:string, xs:integer, xs:float, * xs:double, xs:date, xs:time, xs:dateTime,
 *                xs_yearMonthDuration, xs_dateTimeDuration
 * BST supports xs:string only
 */

static void inline
check_index_key_type(xmlscm_type type, index_backend_t tree_type) {
  if (tree_type == index_btree) {
    if (type != xs_integer && type != xs_float && type != xs_double && type != xs_string && type != xs_date &&
        type != xs_dateTime && type != xs_time && type != xs_yearMonthDuration && type != xs_dayTimeDuration)
        throw USER_EXCEPTION2(SE2034, xmlscm_type2c_str(type));
  } else if (tree_type == index_bstrie) {
    if (type != xs_string) throw USER_EXCEPTION2(SE2042, xmlscm_type2c_str(type));
  } else throw USER_EXCEPTION(SE4084);
}

//Declaration of serializer for SortedSequence

class idx_serializer: public ITupleSerializer
{
private:
    xmlscm_type key_type;

    static tuple_cell get_tc(void* buf, xmlscm_type type, shft size);

public:
    idx_serializer(xmlscm_type t) { key_type = t; }

    size_t serialize(const tuple &t, void *buf);
    void deserialize(tuple &t, void *buf, size_t size);
    int compare(void *buf1, size_t size1, void *buf2, size_t size2);
};

index_cell_xptr create_index(index_descriptor_t* index_dsc)
{
    int64_t counter1 = 0;
    int64_t counter2 = 0;

    const xpath::NodeTest node_test_nodes_deep(xpath::axis_descendant, xpath::node_test_wildcard_star);

    // 0. Check index type
    check_index_key_type(index_dsc->keytype, index_dsc->backend_type);

    // I. Create and fill new index cell
    if (catalog_find_name(catobj_indicies, index_dsc->index_title) != NULL) {
        throw USER_EXCEPTION(SE2033);
    }

    down_concurrent_micro_ops_number();

    index_cell_cptr idc(index_cell_object::create(index_dsc), true);

    doc_schema_node_cptr root_node = index_dsc->owner->get_schema_node();
    root_node.modify()->full_index_list->add(idc.ptr());

    idx_serializer *serializer = new idx_serializer(index_dsc -> keytype);
    SortedSequence *ss = new SortedSequence(serializer);

    tuple tup(2);

    // ALGORITHM: indexing data

    //II. Execute abs path (object_path) on the desriptive schema

    t_scmnodes sobj;
    executePathExpression(root_node.ptr(), *index_dsc->object, &sobj, NULL, NULL);

    //III. For each schema node found (sn_obj)
    for (unsigned int i = 0; i < sobj.size(); i++)
    {
        //IV. Execute path expression (key_path) on the descriptive schema
        t_scmnodes skey;
        /* TODO: check, maybe we can evaluate the whole index at once? */
        executePathExpression(sobj[i], *index_dsc->key, &skey, NULL, NULL);
        //V. For each schema node found (sn_key)
        for (unsigned int j = 0; j < skey.size(); j++)
        {
            xptr blk;
            //VI. Add pair <&ind,&sn_obj> into schema node (the special list is used)
            skey[j].modify()->index_list->add(index_ref(idc.ptr(), sobj[i], skey[j]));

            t_scmnodes skeydep;
            executeNodeTest(skey[j], node_test_nodes_deep, &skeydep, NULL, NULL);
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
                        ss -> add(tup);
                        counter1++;
                    } while (false);

                    node_key = getNextDescriptorOfSameSort(node_key);
                }
            }
        }
    }

    //XIII. Perfom sorting of the keys in ascending order.
    ss -> sort();

    idx::KeyValueMultimap * index_backend = idc->get_backend();
    index_backend->setSortedInsertionHint(false);

    vmm_init_block_counter();

    while (true)
    {
        ss -> next(tup);
        if (tup.is_eos())
        {
            delete ss;
            ss = NULL;
            delete serializer;
            serializer = NULL;
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
    if (idc.found())
    {
        index_descriptor_t index_dsc;
        idc->get_index_descriptor(&index_dsc);
        hl_logical_log_index(&index_dsc, false);
        idc->drop();
    }
}

/*******************************************************************************************
* Functions needed by sorted sequence implementation
*/

size_t idx_serializer::serialize(const tuple &t, void *buf)
{
    CHECK_TIMER_FLAG;

    shft sz = (shft) xmlscm_type_size(key_type);
    if (!sz)
    {
        sz = t.cells[0].get_strlen();
    }

    memcpy((char *)buf, &sz, sizeof(shft));
    xptr temp_value = t.cells[1].get_xptr();
    memcpy((char *)buf + sizeof(shft), &temp_value, sizeof(xptr));

    shft offset = sizeof(shft) + sizeof(xptr);

    tuple_cell& tc = t.cells[0];
    switch (key_type)
    {
        case xs_float                : {float value = tc.get_xs_float();  memcpy((char *)buf + offset, &value, sz); break;}
        case xs_double               : {double value = tc.get_xs_double(); memcpy((char *)buf + offset, &value, sz); break;}
        case xs_integer              : {int64_t value = tc.get_xs_integer(); memcpy((char *)buf + offset, &value, sz); break;}
        case xs_string               :
            {
                tc = tuple_cell::make_sure_light_atomic(tc);
                char* str = tc.get_str_mem();
                memcpy((char *)buf + offset, str, sz);
                break;
            }
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             : {xs_packed_datetime value = tc.get_xs_dateTime(); memcpy((char *)buf + offset, &value, sz); break;}
        case xs_yearMonthDuration    :
        case xs_dayTimeDuration      : {xs_packed_duration value = tc.get_xs_duration(); memcpy((char *)buf + offset, &value, sz); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
    }
    return offset + sz;
}

tuple_cell idx_serializer::get_tc(void* buf, xmlscm_type type, shft size)
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

void idx_serializer::deserialize(tuple& t, void* buf, size_t size)
{
    CHECK_TIMER_FLAG;

    shft sz = *((shft *)buf);

    void *tmp = (char *)buf + sizeof(shft);
    t.copy(
        get_tc((char *)tmp + sizeof(xptr),
                key_type,
                sz
        ),
        tuple_cell::safenode_indir(*(xptr *)tmp));
}

int idx_serializer::compare(void* buf1, size_t size1, void* buf2, size_t size2)
{
    CHECK_TIMER_FLAG;

    xptr key1 = *((xptr *)((char *)buf1 + sizeof(shft)));
    xptr key2 = *((xptr *)((char *)buf2 + sizeof(shft)));

    shft s1 = *((shft *)buf1);
    shft s2 = *((shft *)buf2);

    char *addr1 = (char *)buf1 + sizeof(shft) + sizeof(xptr);
    char *addr2 = (char *)buf2 + sizeof(shft) + sizeof(xptr);

    tuple_cell tc1 = get_tc(addr1, key_type, s1);
    tuple_cell tc2 = get_tc(addr2, key_type, s2);

    get_binary_op_res r = get_binary_op(xqbop_lt, key_type, key_type);

    bool result;
    if (r.collation)
    {
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    }
    else
    {
        result = r.f.bf(tc1, tc2).get_xs_boolean();
    }

    if (result)
    {
        return -1;
    }
    r = get_binary_op(xqbop_gt, key_type, key_type);

    if (r.collation)
    {
        result = r.f.bf_c(tc1, tc2, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    }
    else
    {
        result = r.f.bf(tc1, tc2).get_xs_boolean();
    }

    if (result)
    {
        return 1;
    }

    if(key1 < key2)
    {
        return -1;
    }
    if(key2 < key1)
    {
        return 1;
    }
    return 0;
}

/**********************************************************************************************************************************/
