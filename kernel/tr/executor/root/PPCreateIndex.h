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
    xpath::PathExprRoot root;
    xpath::PathExpression *object_path;
    xpath::PathExpression *key_path;

    xmlscm_type key_type;
    dynamic_context *cxt;
    std::string tree_type;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    
    PPCreateIndex(PPOpIn _index_name_,
                  xpath::PathExprRoot _root_,
                  xpath::PathExpression *_object_path_,
                  xpath::PathExpression *_key_path_,
                  xmlscm_type _key_type_,
                  dynamic_context *_cxt_,
                  const char * _tree_type_);

    ~PPCreateIndex();
    
    inline xmlscm_type get_index_type() const { return key_type; }
    inline std::string get_idxtree_type() const { return tree_type; }
    inline const xpath::PathExpression* get_object_path() const { return object_path; }
    inline const xpath::PathExpression* get_key_path() const { return key_path; }
    inline const xpath::PathExprRoot& get_path_root() const { return root; }
};

#endif /* _PPCREATEINDEX_H */

