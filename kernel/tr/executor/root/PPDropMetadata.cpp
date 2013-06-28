/*
 * File:  PPDropMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropMetadata.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/structures/metadata.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"

///////////////////////////////////////////////////////////////////////////////
/// PPDropDocument
///////////////////////////////////////////////////////////////////////////////
PPDropDocument::PPDropDocument(PPOpIn _name_,
                               dynamic_context *_cxt_) : PPUpdate("PPDropDocument"),
                                                         name(_name_),
                                                         cxt(_cxt_)
{
}

PPDropDocument::~PPDropDocument()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropDocument::do_open()
{
    local_lock_mrg->lock(lm_x);

    cxt->global_variables_open();
    name.op->open();
}

void PPDropDocument::do_close()
{
    name.op->close();
    cxt->global_variables_close();
}

void PPDropDocument::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    v.pop();
}

void PPDropDocument::do_execute()
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
    auth_for_drop_object(tc.get_str_mem(), "document", false);
    delete_document(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPDropCollection
///////////////////////////////////////////////////////////////////////////////
PPDropCollection::PPDropCollection(PPOpIn _name_,
                                   dynamic_context *_cxt_) : PPUpdate("PPDropCollection"),
                                                             name(_name_),
                                                             cxt(_cxt_)
{
}

PPDropCollection::~PPDropCollection()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropCollection::do_open()
{
    cxt->global_variables_open();
    name.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropCollection::do_close()
{
    name.op->close();
    cxt->global_variables_close();
}

void PPDropCollection::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    v.pop();
}

void PPDropCollection::do_execute()
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
    auth_for_drop_object(tc.get_str_mem(), "collection", false);
    delete_collection(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPDropDocumentInCollection
///////////////////////////////////////////////////////////////////////////////
PPDropDocumentInCollection::PPDropDocumentInCollection(PPOpIn _document_,
                                                       PPOpIn _collection_,
                                                       dynamic_context *_cxt_) : PPUpdate("PPDropDocumentInCollection"),
                                                                                  document(_document_),
                                                                                  collection(_collection_),
                                                                                  cxt(_cxt_)
{
}

PPDropDocumentInCollection::~PPDropDocumentInCollection()
{
    delete document.op;
    document.op = NULL;

    delete collection.op;
    collection.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropDocumentInCollection::do_open()
{
    cxt->global_variables_open();
    document.op->open();
    collection.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropDocumentInCollection::do_close()
{
    document.op->close();
    collection.op->close();
    cxt->global_variables_close();
}

void PPDropDocumentInCollection::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    document.op->accept(v);
    collection.op->accept(v);
    v.pop();
}

void PPDropDocumentInCollection::do_execute()
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
    auth_for_drop_object(tc_collection.get_str_mem(), "collection", true);
    delete_document_from_collection(tc_collection.get_str_mem(), tc_document.get_str_mem());
}

