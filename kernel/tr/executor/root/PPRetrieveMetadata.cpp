/*
 * File:  PPRetrieveMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "PPRetrieveMetadata.h"
#include "crmutils.h"
#include "locks.h"


PPRetrieveMetadata::PPRetrieveMetadata(db_entity_type _type_,
                                       PPOpIn _collection_,
                                       bool _output_statistics_,
                                       se_ostream& _s_) : type(_type_),
                                                          collection(_collection_),
                                                          output_statistics(_output_statistics_),
                                                          s(_s_)
{
}

PPRetrieveMetadata::~PPRetrieveMetadata()
{
    if (collection.op)
    {
        delete collection.op;
        collection.op = NULL;
    }
}

void PPRetrieveMetadata::open()
{
    local_lock_mrg->lock(lm_s);

    if (collection.op) collection.op->open();
}

void PPRetrieveMetadata::close()
{
    if (collection.op) collection.op->close();
}

void PPRetrieveMetadata::execute()
{
    if (type == dbe_collection)
    {
        local_lock_mrg->put_lock_on_db();
        print_collections(s, output_statistics);
    }
    else if (type == dbe_document)
    {
        if (collection.op == 0)
        {
            local_lock_mrg->put_lock_on_db();
            print_documents(s, output_statistics);
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
            print_documents_in_collection(s, tc.get_str_mem());
        }
    }
    else throw USER_EXCEPTION2(SE1003, "Wrong combination of arguments in PPRetrieveMetadata");
}

