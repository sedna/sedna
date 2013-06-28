/*
 * File:  PPCreateMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPCreateMetadata.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/structures/metadata.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"


///////////////////////////////////////////////////////////////////////////////
/// PPCreateDocument
///////////////////////////////////////////////////////////////////////////////
PPCreateDocument::PPCreateDocument(PPOpIn _name_, 
                                   dynamic_context *_cxt_) : PPUpdate("PPCreateDocument"),
                                                             name(_name_), 
                                                             cxt(_cxt_)
{
}

PPCreateDocument::~PPCreateDocument()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateDocument::do_open()
{
    local_lock_mrg->lock(lm_x);

    cxt->global_variables_open();
    name.op->open();
}

void PPCreateDocument::do_close()
{
    name.op->close();
    cxt->global_variables_close();
}

void PPCreateDocument::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    v.pop();
}

void PPCreateDocument::do_execute()
{
    tuple_cell tc;
    xqp_tuple t(1);
    name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_document(tc.get_str_mem());

    auth_for_create_document(tc.get_str_mem());

    insert_document(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPCreateCollection
///////////////////////////////////////////////////////////////////////////////
PPCreateCollection::PPCreateCollection(PPOpIn _name_,
                                       dynamic_context *_cxt_) : PPUpdate("PPCreateCollection"),
                                                                 name(_name_),
                                                                 cxt(_cxt_)
{
}

PPCreateCollection::~PPCreateCollection()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateCollection::do_open()
{
    cxt->global_variables_open();
    name.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPCreateCollection::do_close()
{
    name.op->close();
    cxt->global_variables_close();
}

void PPCreateCollection::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    v.pop();
}

void PPCreateCollection::do_execute()
{
    tuple_cell tc;
    xqp_tuple t(1);
    name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_collection(tc.get_str_mem());

    auth_for_create_collection(tc.get_str_mem());

    insert_collection(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPCreateDocumentInCollection
///////////////////////////////////////////////////////////////////////////////
PPCreateDocumentInCollection::PPCreateDocumentInCollection(PPOpIn _document_,
                                                           PPOpIn _collection_,
                                                           dynamic_context *_cxt_) : PPUpdate("PPCreateDocumentInCollection"),
                                                                                      cxt(_cxt_),
                                                                                      document(_document_),
                                                                                      collection(_collection_)
{
}

PPCreateDocumentInCollection::~PPCreateDocumentInCollection()
{
    delete document.op;
    document.op = NULL;

    delete collection.op;
    collection.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateDocumentInCollection::do_open()
{
    cxt->global_variables_open();
    document.op->open();
    collection.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPCreateDocumentInCollection::do_close()
{
    document.op->close();
    collection.op->close();
    cxt->global_variables_close();
}

void PPCreateDocumentInCollection::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    document.op->accept(v);
    collection.op->accept(v);    
    v.pop();
}

void PPCreateDocumentInCollection::do_execute()
{
    tuple_cell tc, tc_document, tc_collection;
    xqp_tuple t(1);

    document.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = document.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    document.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
    tc_document = tuple_cell::make_sure_light_atomic(tc);


    collection.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = collection.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    collection.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
    tc_collection = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_collection(tc_collection.get_str_mem());

    auth_for_create_document_collection(tc_document.get_str_mem(), tc_collection.get_str_mem());

    insert_document_into_collection(tc_collection.get_str_mem(), tc_document.get_str_mem());
}
