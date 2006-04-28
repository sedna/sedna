/*
 * File:  PPCreateIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCREATEINDEX_H
#define _PPCREATEINDEX_H

#include "sedna.h"

#include "PPBase.h"
#include "XPathOnSchema.h"

class PPCreateIndex : public PPUpdate
{
    // given parameters
    PathExpr *object_path;
    PathExpr *key_path;
    xmlscm_type key_type;
    counted_ptr<db_entity> db_ent;
    PPOpIn index_name;

    // obtained parameters and local data
    schema_node *root;

public:
    void open();
    void close();
    void execute();

    PPCreateIndex(PathExpr *_object_path_,
                  PathExpr *_key_path_,
                  xmlscm_type _key_type_,
                  counted_ptr<db_entity> _db_ent_,
                  PPOpIn _index_name_);

    ~PPCreateIndex();
};

#endif

