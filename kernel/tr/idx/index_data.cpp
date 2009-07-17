/*
 * File:  index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>
#include <sstream>

#include "common/sedna.h"

#include "tr/idx/index_data.h"
#include "common/xptr.h"
#include "tr/crmutils/node_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#include "tr/idx/btree/btree.h"
#include "tr/structures/schema.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/idx/indexes.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/cat/catstore.h"


using namespace std;

static bool index_initialized = false;

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


void index_cell_object::serialize_data(se_simplestream &stream)
{
    U_ASSERT(schemaroot != XNULL);
    cs_set_hint(schemaroot);

    stream.write_string(index_title);
    stream.write(&keytype, sizeof(xmlscm_type));
    stream.write(&btree_root, sizeof(xptr));

    std::ostringstream obj_str(std::ios::out | std::ios::binary);
    std::ostringstream key_str(std::ios::out | std::ios::binary);

    PathExpr2lr(object, obj_str);
    PathExpr2lr(key, key_str);

    stream.write_string(obj_str.str().c_str());
    stream.write_string(key_str.str().c_str());

    stream.write(&schemaroot, sizeof(doc_schema_node_xptr));
    stream.write_string(doc_name);
    stream.write(&is_doc, sizeof(bool));
    stream.write(&err_cntr, sizeof(int));
};

void index_cell_object::deserialize_data(se_simplestream &stream)
{
    index_title = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, index_title);

    stream.read(&keytype, sizeof(xmlscm_type));
    stream.read(&btree_root, sizeof(xptr));

    char * obj_str;
    char * key_str;

    obj_str = (char *) cat_malloc_context(CATALOG_COMMON_CONTEXT, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, obj_str);
    object = lr2PathExpr(NULL, obj_str, pe_catalog_aspace);
    free(obj_str);

    key_str = (char *) cat_malloc_context(CATALOG_COMMON_CONTEXT, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, key_str);
    key = lr2PathExpr(NULL, key_str, pe_catalog_aspace);
    free(key_str);

    /* object deserialization (PathExpr*) */
    /* key deserialization (PathExpr*) */

    stream.read(&schemaroot, sizeof(doc_schema_node_xptr));
    doc_name = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, doc_name);
    stream.read(&is_doc, sizeof(bool));
    stream.read(&err_cntr, sizeof(int));
};

void index_cell_object::drop()
{
    schemaroot.modify()->delete_index(p_object);
    catalog_delete_name(catobj_indicies, this->index_title);
    bt_drop(btree_root);
    cs_free(p_object);
    catalog_delete_object(this);
};



t_scmnodes index_cell_object::fits_to_index_as_key(schema_node_cptr snode) const
{
    t_scmnodes res;
    t_scmnodes objs=execute_abs_path_expr(snode->root, object);
    t_scmnodes::iterator it=objs.begin();
    while (it!=objs.end())
    {
        if ((*it)->is_ancestor_or_self(snode))
        {
            t_scmnodes keys=execute_abs_path_expr(*it, key);
            t_scmnodes::iterator it2=keys.begin();
            while (it2!=keys.end())
            {
                if (snode==*it2)
                {
                    res.push_back(*it);
                    break;
                }
                it2++;
            }
        }
        it++;
    }
    return res;
}

void index_cell_object::put_to_index(xptr node, schema_node_cptr accessor)
{
    xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
    xptr bt_root = btree_root;

    try
    {
        tuple_cell tc = cast(dm_typed_value(node), keytype);
        bt_key key;
        tuple_cell2bt_key(tc, key);
        bt_insert(bt_root, key, acc);

        if (bt_root != btree_root)
            ((index_cell_object *) (this->modify_self()))->btree_root = bt_root;
    }
    catch (SednaUserException &ex)
    {
        this->err_cntr++;
    }
}

void index_cell_object::delete_from_index(xptr node, schema_node_cptr accessor)
{
    xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
    xptr bt_root = btree_root;

    try
    {
        tuple_cell tc = cast(dm_typed_value(node), keytype);
        bt_key key;
        tuple_cell2bt_key(tc, key);
        bt_delete(bt_root, key, acc);

        if (bt_root != btree_root)
            ((index_cell_object *) (this->modify_self()))->btree_root = bt_root;
    }
    catch (SednaUserException &ex)
    {
        this->err_cntr--;
    }

}
void index_cell_object::delete_from_index(xptr node,const char* value, int size, schema_node_cptr accessor)
{
    xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
    char* z =se_new char[size+1];
    memcpy(z,value,size);
    z[size]='\0';
    xptr bt_root = btree_root;

    try
    {
        tuple_cell tc = cast(tuple_cell::atomic_deep(xs_string,z), keytype);
        bt_key key;
        tuple_cell2bt_key(tc, key);
        bt_delete(bt_root, key, acc);

        if (bt_root != btree_root)
            ((index_cell_object *) (this->modify_self()))->btree_root = bt_root;
    }
    catch (SednaUserException &ex)
    {
        this->err_cntr--;
    }

}
void index_cell_object::put_to_index(xptr node,const char* value, int size, schema_node_cptr accessor)
{
    xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
    char* z =se_new char[size+1];
    memcpy(z,value,size);
    z[size]='\0';
    xptr bt_root = btree_root;

    try
    {
        tuple_cell tc = cast(tuple_cell::atomic_deep(xs_string,z), keytype);
        bt_key key;
        tuple_cell2bt_key(tc, key);
        CHECKP(btree_root);
        bt_insert(bt_root, key, acc);

        if (bt_root != btree_root)
            ((index_cell_object *) (this->modify_self()))->btree_root = bt_root;
    }
    catch (SednaUserException &ex)
    {
        this->err_cntr++;
    }
}
