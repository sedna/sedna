/*
 * File:  PPAbsPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPABSPATH_H
#define _PPABSPATH_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/structures/system_tables.h"


class PPAbsPath : public PPIterator
{
protected:
    PathExpr *path_expr;
    counted_ptr<db_entity> db_ent;
    PPOpIn name;

    schema_node_xptr root;
    xptr* merged_seq_arr;
    int scmnodes_num;

    friend int compare_merged_seq_elem(const void *e1, const void *e2);

    bool determine_root();
    void create_merged_seq(int &scmnodes_num, xptr*& merged_seq_arr,
                           schema_node_cptr root, PathExpr *path_expr);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPAbsPath(dynamic_context *_cxt_,
              operation_info _info_, 
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_);

    PPAbsPath(dynamic_context *_cxt_, 
              operation_info _info_, 
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_);
    
    PPAbsPath(dynamic_context *_cxt_, 
              operation_info _info_,
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_,
              schema_node_xptr _root_);
    virtual ~PPAbsPath();

    bool isDocCollFunCall() const; // true, if PPAbsPath is just wrapping over fn:document/fn:collection call
    void setPathExpr(PathExpr *pe); // to set path expression later on qep construction
};


#endif

