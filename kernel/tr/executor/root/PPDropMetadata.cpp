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
PPDropDocument::PPDropDocument(PPOpIn _name_, dynamic_context *_cxt_) : name(_name_), cxt(_cxt_)
{
}

PPDropDocument::~PPDropDocument()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropDocument::open()
{
    local_lock_mrg->lock(lm_x);

    dynamic_context::global_variables_open();
    name.op->open();
}

void PPDropDocument::close()
{
    name.op->close();
    dynamic_context::global_variables_close();
}

void PPDropDocument::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    name.op->accept(v);
    v.pop();
}

void PPDropDocument::execute()
{
    tuple_cell tc;
    tuple t(1);
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
PPDropCollection::PPDropCollection(PPOpIn _name_, dynamic_context *_cxt_) : name(_name_), cxt(_cxt_)
{
}

PPDropCollection::~PPDropCollection()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropCollection::open()
{
    dynamic_context::global_variables_open();
    name.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropCollection::close()
{
    name.op->close();
    dynamic_context::global_variables_close();
}

void PPDropCollection::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    name.op->accept(v);
    v.pop();
}

void PPDropCollection::execute()
{
    tuple_cell tc;
    tuple t(1);
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
                                                       dynamic_context *_cxt1_,
                                                       PPOpIn _collection_,
                                                       dynamic_context *_cxt2_) : document(_document_),
                                                                                  collection(_collection_),
                                                                                  cxt1(_cxt1_),
                                                                                  cxt2(_cxt2_)
{
}

PPDropDocumentInCollection::~PPDropDocumentInCollection()
{
    delete document.op;
    document.op = NULL;

    delete collection.op;
    collection.op = NULL;

    delete cxt1;
    cxt1 = NULL;
    delete cxt2;
    cxt2 = NULL;
}

void PPDropDocumentInCollection::open()
{
    dynamic_context::global_variables_open();
    document.op->open();
    collection.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropDocumentInCollection::close()
{
    document.op->close();
    collection.op->close();
    dynamic_context::global_variables_close();
}

void PPDropDocumentInCollection::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    document.op->accept(v);
    collection.op->accept(v);
    v.pop();
}

void PPDropDocumentInCollection::execute()
{
    tuple_cell tc, tc_document, tc_collection;
    tuple t(1);

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

