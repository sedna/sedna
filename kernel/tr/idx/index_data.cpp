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
#include "tr/mo/modebug.h"
#include "tr/bstrie/sednabtrie.h"


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

#define FOR_EACH(i, l, t) for (t::iterator i = l.begin(); i != l.end(); i++)

void index_cell_object::new_node_available(schema_node_cptr snode) const
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


/*

t_scmnodes index_cell_object::fits_to_index_as_key(schema_node_cptr snode) const
{
    t_scmnodes res;
    t_scmnodes objs = execute_abs_path_expr(snode->root, object);
    const NodeTest node_test_nodes_deep = {axis_descendant, node_test_wildcard_star};

    FOR_EACH(i, objs, t_scmnodes) {
        if ((*i)->is_ancestor_or_self(snode)) {
            t_scmnodes keys = execute_abs_path_expr(*i, key);
            FOR_EACH(j, keys, t_scmnodes) {
                if (snode.ptr() == *j) { res.push_back(*i); break; }
                t_scmnodes keydeps = execute_node_test(*i, node_test_nodes_deep);
                FOR_EACH(k, keydeps, t_scmnodes) { if (snode.ptr() == *j) { res.push_back(*i); break; } }
            }
        }
    }

    return res;
}
*/

void index_cell_object::put_to_index(xptr key_node, xptr object_indir)
{
    xptr bt_root = btree_root;
    bt_key key;

    try {
        tuple_cell tc = dm_typed_value(key_node);
        if (!((getNodeTypeCP(key_node) == element) && (tc.get_strlen() == 0))) {
            tc = cast(tc, keytype);
            bt_insert(bt_root, tuple_cell2bt_key(tc, key), object_indir);
            if (bt_root != btree_root) { ((index_cell_object *) (this->modify_self()))->btree_root = bt_root; }
        }
    } catch (SednaUserException) {
        this->err_cntr++;
    }
}


void index_cell_object::delete_from_index(xptr key_node, xptr object_indir)
{
    xptr bt_root = btree_root;
    bt_key key;

    try {
        tuple_cell tc = dm_typed_value(key_node);
        if (!((getNodeTypeCP(key_node) == element) && (tc.get_strlen() == 0))) {
            tc = cast(tc, keytype);
            bt_delete(bt_root, tuple_cell2bt_key(tc, key), object_indir);
            if (bt_root != btree_root) { ((index_cell_object *) (this->modify_self()))->btree_root = bt_root; }
        }
    } catch (SednaUserException) {
        this->err_cntr--;
    }
}

/*
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
*/
