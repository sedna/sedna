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
    xpath::PathExpression *path_expr;
    counted_ptr<db_entity> db_ent;
    PPOpIn name;

    schema_node_xptr root;
    xptr* merged_seq_arr;
    int scmnodes_num;

    friend int compare_merged_seq_elem(const void *e1, const void *e2);

    bool determine_root();
    void create_merged_seq(int &scmnodes_num, xptr*& merged_seq_arr,
                           schema_node_cptr root, xpath::PathExpression *path_expr);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

    /* Private constructor for copy */
    PPAbsPath(dynamic_context *_cxt_,
              operation_info _info_,
              xpath::PathExpression *_path_expr_,
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_,
              schema_node_xptr _root_);
public:
    PPAbsPath(dynamic_context *_cxt_,
              operation_info _info_,
              xpath::PathExpression *_path_expr_,
              counted_ptr<db_entity> _db_ent_);

    PPAbsPath(dynamic_context *_cxt_,
              operation_info _info_,
              xpath::PathExpression *_path_expr_,
              counted_ptr<db_entity> _db_ent_,
              PPOpIn _name_);

    virtual ~PPAbsPath();

    bool isDocCollFunCall() const;  // true, if PPAbsPath is just wrapping over fn:document/fn:collection call
    void setPathExpr(xpath::PathExpression *pe); // to set path expression later on qep construction

    xpath::PathExpression *getPathExpr()         // use it to get path_expr to use it in some other operation (i.e. when PPAbsPath just a container)
    {
        return path_expr;
    }

    counted_ptr<db_entity> getDocColl() // use it to get document/collection entity (i.e. when PPAbsPath just a container)
    {
        return db_ent;
    }

    // use it to get computed document/collection name (i.e. when PPAbsPath just a container)
    // side effect: after this PPAbsPath won't be storing name-PPOpIn since it gets stripped from it
    PPOpIn getCompName()
    {
        PPOpIn res = name;
        name.op = NULL;

        return res;
    }
};


#endif

