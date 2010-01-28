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
    PPOpIn index_name;
    PathExprRoot root;
    PathExpr *object_path;
    PathExpr *key_path;
    xmlscm_type key_type;
    dynamic_context *cxt;

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
    
    inline xmlscm_type get_index_type() const { return key_type; }
    inline const PathExpr* get_object_path() const { return object_path; }
    inline const PathExpr* get_key_path() const { return key_path; }
    inline const PathExprRoot& get_path_root() const { return root; }
};

#endif /* _PPCREATEINDEX_H */

