/*
 * File:  PPRetrieveMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRETRIEVEMETADATA_H
#define _PPRETRIEVEMETADATA_H

#include "sedna.h"
#include "PPBase.h"

class PPRetrieveMetadata : public PPQueryEssence
{
    // given parameters
    db_entity_type type;
    PPOpIn collection;
    dynamic_context *cxt;
    bool output_statistics;
    se_ostream& s;

public:
    void open();
    void close();
    void execute();

    bool supports_next() { return false; }
    bool is_update() { return false; }

    PPRetrieveMetadata(db_entity_type _type_,
                       PPOpIn _collection_,
                       dynamic_context *_cxt_,
                       bool _output_statistics_,
                       se_ostream& _s_);

    ~PPRetrieveMetadata();
};


#endif

