/*
 * File:  PPRetrieveDS.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRETRIEVEDS_H
#define _PPRETRIEVEDS_H

#include "sedna.h"
#include "PPBase.h"

class PPRetrieveDS : public PPQueryEssence
{
    // given parameters
    PPOpIn name;
    dynamic_context *cxt;
    db_entity_type type;
    se_ostream& s;

public:
    void open();
    void close();
    void execute();

    bool supports_next() { return false; }
    bool is_update() { return false; }

    PPRetrieveDS(PPOpIn _name_,
                 dynamic_context *_cxt_,
                 db_entity_type _type_,
                 se_ostream& _s_);
    ~PPRetrieveDS();
};



#endif

