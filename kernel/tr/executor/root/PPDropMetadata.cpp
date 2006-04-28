/*
 * File:  PPDropMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPDropMetadata.h"
#include "metadata.h"
#include "locks.h"

///////////////////////////////////////////////////////////////////////////////
/// PPDropDocument
///////////////////////////////////////////////////////////////////////////////
PPDropDocument::PPDropDocument(PPOpIn _name_) : name(_name_)
{
}

PPDropDocument::~PPDropDocument()
{
    delete name.op;
    name.op = NULL;
}

void PPDropDocument::open()
{
    local_lock_mrg->lock(lm_x);

    name.op->open();
}

void PPDropDocument::close()
{
    name.op->close();
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
    delete_document(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPDropCollection
///////////////////////////////////////////////////////////////////////////////
PPDropCollection::PPDropCollection(PPOpIn _name_) : name(_name_)
{
}

PPDropCollection::~PPDropCollection()
{
    delete name.op;
    name.op = NULL;
}

void PPDropCollection::open()
{
    name.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropCollection::close()
{
    name.op->close();
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
    delete_collection(tc.get_str_mem());
}


///////////////////////////////////////////////////////////////////////////////
/// PPDropDocumentInCollection
///////////////////////////////////////////////////////////////////////////////
PPDropDocumentInCollection::PPDropDocumentInCollection(PPOpIn _document_,
                                                       PPOpIn _collection_) : document(_document_),
                                                                              collection(_collection_)
{
}

PPDropDocumentInCollection::~PPDropDocumentInCollection()
{
    delete document.op;
    document.op = NULL;

    delete collection.op;
    collection.op = NULL;
}

void PPDropDocumentInCollection::open()
{
    document.op->open();
    collection.op->open();

    local_lock_mrg->lock(lm_x);
}

void PPDropDocumentInCollection::close()
{
    document.op->close();
    collection.op->close();
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
    delete_document(tc_collection.get_str_mem(), tc_document.get_str_mem());
}

