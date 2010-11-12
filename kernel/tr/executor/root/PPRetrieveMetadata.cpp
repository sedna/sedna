/*
 * File:  PPRetrieveMetadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/root/PPRetrieveMetadata.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"


PPRetrieveMetadata::PPRetrieveMetadata(db_entity_type _type_,
                                       PPOpIn _collection_,
                                       dynamic_context *_cxt_,
                                       bool _output_statistics_) : PPQueryEssence("PPRetrieveMetadata"),
                                                                   type(_type_),
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

void PPRetrieveMetadata::do_open()
{
    local_lock_mrg->lock(lm_s);

    cxt->global_variables_open();
    if (collection.op) collection.op->open();
}

void PPRetrieveMetadata::do_close()
{
    if (collection.op) collection.op->close();
    cxt->global_variables_close();
}

void PPRetrieveMetadata::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if(collection.op) collection.op->accept(v);
    v.pop();
}

void PPRetrieveMetadata::do_execute()
{
    throw USER_EXCEPTION2(SE2901, "Retrieve Metadata. Please use system documents i.e. doc('$documents')");
}

