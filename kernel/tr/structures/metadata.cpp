/*
 * File:  metadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/structures/metadata.h"
#include "common/xptr.h"
#include "tr/mo/mo.h"
#include "tr/log/log.h"
#include "tr/locks/locks.h"
#include "tr/idx/indexes.h"
#include "tr/executor/base/xs_uri.h"
#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#include "tr/updates/updates.h"
#endif
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif
#include "tr/cat/catstore.h"
#include "tr/mo/blocks.h"
#include "tr/mo/indirection.h"

using namespace std;

static bool metadata_initialized = false;



void metadata_cell_object::serialize_data(se_simplestream &stream)
{
    cs_set_hint(XNULL);
    stream.write(&snode, sizeof(schema_node_xptr));
    stream.write(&is_doc, sizeof(bool));
    stream.write_string(name);
}

void metadata_cell_object::deserialize_data(se_simplestream &stream)
{
    stream.read(&snode, sizeof(schema_node_xptr));
    stream.read(&is_doc, sizeof(bool));
    name = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, name);
}


void metadata_on_session_begin()
{
    metadata_initialized = true;
}

void metadata_on_session_end()
{
    free_xmlns_hash();
    metadata_initialized = false;
}

void metadata_cell_object::drop()
{
    snode->drop();
    catalog_delete_name(catobj_metadata, this->name);
    cs_free(p_object);
    catalog_delete_object(this);
};

void delete_document(const char *document_name)
{
    metadata_cptr document(document_name, true);
    if (!document.found() || (document->snode->get_magic() != doc_schema_node_object::magic))
      { throw USER_EXCEPTION2(SE2006, document_name); }

    xptr blk = document->snode->bblk;
    U_ASSERT(blk != XNULL); // Document MUST have exactly one document node, so blk is not null
    CHECKP(blk);
    delete_doc_node(getFirstBlockNode(blk), document_name, NULL);
    document->drop();
    up_concurrent_micro_ops_number();
};

void delete_collection(const char *collection_name)
{
    metadata_cptr collection(collection_name, true);
    if (!collection.found()) throw USER_EXCEPTION2(SE2003, collection_name);

// Delete documents from collection
    bt_key key;
    key.setnew(" ");

    bt_cursor cursor = bt_find_gt(((col_schema_node_xptr)collection->snode)->metadata, key);
    if (!cursor.is_null()) do {
        xptr indir = cursor.bt_next_obj();
        xptr node  = indirectionDereferenceCP(indir);
        if (node != XNULL) {
            CHECKP(node);
            xptr ind = nodeGetIndirection(node);
            delete_doc_node(node, (const char*) cursor.get_key().data(), collection_name);
            up_concurrent_micro_ops_number();
        }
    } while (cursor.bt_next_key());

    down_concurrent_micro_ops_number();

    collection->drop();
    hl_logical_log_collection(collection_name, false);

    up_concurrent_micro_ops_number();
}

void delete_document_from_collection(const char *collection_name, const char *document_name)
{
    metadata_cptr collection(collection_name, true);
    if (!collection.found()) throw USER_EXCEPTION2(SE2003, collection_name);
    col_schema_node_cptr col_node = collection->snode;
    xptr node = col_node->find_document(document_name);
    if (node == XNULL)       throw USER_EXCEPTION2(SE2006,document_name);

    col_node->delete_document(document_name);

    CHECKP(node);

    // TOREVIEW: Changing eblk
    if ((internal::getBlockHeader(node))->count == 1 && col_node->eblk == block_xptr(node))
          col_node.modify()->eblk = (internal::getBlockHeader(node))->pblk;

    delete_doc_node(node, document_name, collection_name);
    up_concurrent_micro_ops_number();
#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
}

xptr insert_document(const char * uri, bool persistent)
{
    const char * name = uri;
    xptr node_indir;
    metadata_cptr mdc = XNULL;

    bool valid = true;
    Uri::check_constraints(name, &valid, NULL);

    if (!valid)
        throw USER_EXCEPTION2(SE2008, (string("Invalid document name '") + name + "'").c_str());

    if (catalog_find_name(catobj_metadata, name) != NULL)
        throw USER_EXCEPTION2 (SE2001, name);

    if (persistent) {
        mdc = metadata_cptr(metadata_cell_object::create(true, name));
        cs_set_hint(mdc.ptr());
    }

    doc_schema_node_cptr scm(doc_schema_node_object::create(persistent));
    node_indir = insert_doc_node(scm, name, NULL);

    if (mdc.found()) { mdc->snode = scm.ptr(); }

    return indirectionDereferenceCP(node_indir);
}

col_schema_node_xptr insert_collection(const char *collection_name)
{
    bool valid = true;
    Uri::check_constraints(collection_name, &valid, NULL);

    if (!valid) throw USER_EXCEPTION2(SE2008, (std::string("Invalid collection name '") + collection_name + "'").c_str());

    if (catalog_find_name(catobj_metadata, collection_name) != NULL)
        throw USER_EXCEPTION2(SE2002,collection_name);

    down_concurrent_micro_ops_number();

    metadata_cptr mdc(metadata_cell_object::create(false, collection_name));
    cs_set_hint(mdc.ptr());
    mdc->snode = (schema_node_xptr) col_schema_node_object::create()->p;
    hl_logical_log_collection(collection_name, true);
    up_concurrent_micro_ops_number();

    return mdc->snode;
}

xptr insert_document_into_collection(const char *collection_name, const char *uri)
{
    const char * name = uri;

    bool valid = true;
    Uri::check_constraints(name, &valid, NULL);

    if (!valid) throw USER_EXCEPTION2(SE2008, (std::string("Invalid document name '") + name + "'").c_str());

    if (find_document_in_collection(collection_name, name) != XNULL)
        throw USER_EXCEPTION(SE2004);

    metadata_cptr collection(collection_name);
    if (!collection.found()) throw USER_EXCEPTION2(SE2003, collection_name);

    down_concurrent_micro_ops_number();

    col_schema_node_cptr scm = collection->snode;
    xptr node_indir;
    node_indir = insert_doc_node(collection->snode, name, collection_name);
    ((col_schema_node_xptr)collection->snode)->insert_document(name, node_indir);

    up_concurrent_micro_ops_number();
    return indirectionDereferenceCP(node_indir);
}

col_schema_node_xptr find_collection(const char *collection_name) {
    metadata_cptr mdc(collection_name, false);
    if (mdc.found() && (mdc->snode->get_magic() == col_schema_node_object::magic)) return mdc->snode;
    else return XNULL;
}

doc_schema_node_xptr find_document(const char *document_name) {
    metadata_cptr mdc(document_name, false);
    if (mdc.found() && (mdc->snode->get_magic() == doc_schema_node_object::magic)) return mdc->snode;
    else return XNULL;
}

xptr find_document_in_collection(const char *collection_name, const char *document_name)
{
    metadata_cptr mdc(collection_name, false);
    schema_node_object * snd;
    if (!mdc.found()) { return XNULL; }
    snd = &(*(schema_node_cptr(mdc->snode)));
    if ((dynamic_cast<col_schema_node_object *> (snd)) == NULL) { return XNULL; }
    xptr res = (dynamic_cast<col_schema_node_object *> (snd))->find_document(document_name);
    return res;
}

void rename_collection(const char *old_collection_name, const char *new_collection_name)
{
    bool valid = true;

    Uri::check_constraints(new_collection_name, &valid, NULL);
    if (!valid) throw USER_EXCEPTION2(SE2008, (std::string("Invalid collection name '") + new_collection_name + "'").c_str());

    metadata_cptr collection(old_collection_name, true);
    if (!collection.found())
        throw USER_EXCEPTION2(SE2003, old_collection_name);

    if (catalog_name_exists(catobj_metadata, new_collection_name))
        throw USER_EXCEPTION2(SE2002, new_collection_name);

    down_concurrent_micro_ops_number();

    collection.modify();
    catalog_delete_name(catobj_metadata, collection->name);
    collection->name = cat_strcpy(collection->name, new_collection_name);
    catalog_set_name(catobj_metadata, collection->name, collection.obj);

    hl_logical_log_rename_collection(old_collection_name, new_collection_name);
    up_concurrent_micro_ops_number();
}

