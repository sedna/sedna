/*
 * File:  PPRetrieveMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRETRIEVEMETADATA_H
#define _PPRETRIEVEMETADATA_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/structures/system_tables.h"

class PPRetrieveMetadata : public PPQueryEssence
{
private:
    db_entity_type type;
    PPOpIn collection;
    dynamic_context *cxt;
    bool output_statistics;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    bool supports_next() { return false; }
    bool is_update() { return false; }

    PPRetrieveMetadata(db_entity_type _type_,
                       PPOpIn _collection_,
                       dynamic_context *_cxt_,
                       bool _output_statistics_);

    ~PPRetrieveMetadata();
};


#endif

