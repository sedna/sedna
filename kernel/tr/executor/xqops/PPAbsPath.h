/*
 * File:  PPAbsPath.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPABSPATH_H
#define _PPABSPATH_H

#include "sedna.h"
#include "PPBase.h"
#include "XPathOnSchema.h"



class PPAbsPath : public PPIterator
{
protected:
    // given parameters
    PathExpr *path_expr;
    counted_ptr<db_entity> db_ent;
    PPOpIn name;
    // obtained parameters and local data
    schema_node *root;
    xptr* merged_seq_arr;	// used for sorting
    int scmnodes_num;


    friend int compare_merged_seq_elem(const void *e1, const void *e2);

    bool determine_root();
    static void create_merged_seq(int &scmnodes_num, xptr*& merged_seq_arr,
                                  schema_node *root, PathExpr *path_expr);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPAbsPath(variable_context *_cxt_, 
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_);
    PPAbsPath(variable_context *_cxt_, 
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_);
    PPAbsPath(variable_context *_cxt_, 
              PathExpr *_path_expr_, 
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_,
              schema_node *_root_);
    virtual ~PPAbsPath();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif

