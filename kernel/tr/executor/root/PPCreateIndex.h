/*
 * File:  PPCreateIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCREATEINDEX_H
#define _PPCREATEINDEX_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"


class PPCreateIndex : public PPUpdate
{
private:
    PathExpr *object_path;
    PathExpr *key_path;
    PathExprRoot root;
    xmlscm_type key_type;
    dynamic_context *cxt;
    PPOpIn index_name;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);
    
    PPCreateIndex(PPOpIn _index_name_,
                  PathExprRoot _root_,
                  PathExpr *_object_path_,
                  PathExpr *_key_path_,
                  xmlscm_type _key_type_,
                  dynamic_context *_cxt_);

    ~PPCreateIndex();
};

#endif /* _PPCREATEINDEX_H */

