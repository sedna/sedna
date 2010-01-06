/*
 * File:  PPRetrieveMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/root/PPRetrieveMetadata.h"
#include "tr/executor/base/PPVisitor.h"
#include "tr/crmutils/crmutils.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"


PPRetrieveMetadata::PPRetrieveMetadata(db_entity_type _type_,
                                       PPOpIn _collection_,
                                       dynamic_context *_cxt_,
                                       bool _output_statistics_) : type(_type_),
                                                                   collection(_collection_),
                                                                   cxt(_cxt_),
                                                                   output_statistics(_output_statistics_)
{
}

PPRetrieveMetadata::~PPRetrieveMetadata()
{
    if (collection.op)
    {
        delete collection.op;
        collection.op = NULL;

        delete cxt;
        cxt = NULL;
    }
}

void PPRetrieveMetadata::open()
{
    local_lock_mrg->lock(lm_s);

    dynamic_context::global_variables_open();
    if (collection.op) collection.op->open();
}

void PPRetrieveMetadata::close()
{
    if (collection.op) collection.op->close();
    dynamic_context::global_variables_close();
}

void PPRetrieveMetadata::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    if(collection.op) collection.op->accept(v);
    v.pop();
}

void PPRetrieveMetadata::execute()
{
    if (type == dbe_collection)
    {
        local_lock_mrg->put_lock_on_db();
        print_collections(*tr_globals::client->get_se_ostream(), output_statistics);
    }
    else if (type == dbe_document)
    {
        if (collection.op == 0)
        {
            local_lock_mrg->put_lock_on_db();
            print_documents(*tr_globals::client->get_se_ostream(), output_statistics);
        }
        else 
        {
            tuple_cell tc;
            tuple t(1);

            collection.op->next(t);
            if (t.is_eos()) throw USER_EXCEPTION(SE1071);

            tc = collection.get(t);
            if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
                throw USER_EXCEPTION(SE1071);

            collection.op->next(t);
            if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
            tc = tuple_cell::make_sure_light_atomic(tc);

            local_lock_mrg->put_lock_on_collection(tc.get_str_mem());
            print_documents_in_collection(*tr_globals::client->get_se_ostream(), tc.get_str_mem());
        }
    }
    else throw USER_EXCEPTION2(SE1003, "Wrong combination of arguments in PPRetrieveMetadata");
}

