/*
 * File:  PPAbsPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "common/utils.h"

#include "tr/executor/xqops/PPAbsPath.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/metadata.h"

#include "tr/structures/nodeutils.h"

using namespace xpath;

/**
 * The goal of this operation is to provide efficient execution for absolute
 * path expressions (XPath expressions from the root of a document), which
 * could be evaluated using document schema.
 * These expressions must satisfy the following part of the XQuery grammar.
 *

PathExpr    ::=    ("/" RelativePathExpr?) |  ("//" RelativePathExpr)
RelativePathExpr    ::=    StepExpr (("/" |  "//") StepExpr)*

StepExpr    ::=    ForwardStep
ForwardStep    ::=    (ForwardAxis NodeTest) |  AbbreviatedForwardStep

ForwardAxis    ::=    <"child" "::">
                   |  <"descendant" "::">
                   |  <"attribute" "::">
                   |  <"self" "::">
                   |  <"descendant-or-self" "::">

AbbreviatedForwardStep    ::=    "." |  ("@" NameTest) |  NodeTest

NodeTest    ::=    KindTest |  NameTest
NameTest    ::=    QName |  Wildcard
Wildcard    ::=    "*" |  <NCName ":" "*"> |  <"*" ":" NCName>
KindTest    ::=    ProcessingInstructionTest
                |  CommentTest
                |  TextTest
                |  AnyKindTest
ProcessingInstructionTest    ::=    <"processing-instruction" "("> StringLiteral? ")"
CommentTest    ::=    <"comment" "("> ")"
TextTest    ::=    <"text" "("> ")"
AnyKindTest    ::=    <"node" "("> ")"
*/



PPAbsPath::PPAbsPath(dynamic_context *_cxt_,
                     operation_info _info_,
                     xpath::PathExpression *_path_expr_,
                     counted_ptr<db_entity> _db_ent_) : PPIterator(_cxt_, _info_, "PPAbsPath"),
                                                        path_expr(_path_expr_),
                                                        db_ent(_db_ent_),
                                                        name(NULL, 0),
                                                        root(XNULL)
{
}

PPAbsPath::PPAbsPath(dynamic_context *_cxt_,
                     operation_info _info_,
                     xpath::PathExpression *_path_expr_,
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_) : PPIterator(_cxt_, _info_, "PPAbsPath"),
                                      path_expr(_path_expr_),
                                      db_ent(_db_ent_),
                                      name(_name_),
                                      root(XNULL)
{
}

PPAbsPath::PPAbsPath(dynamic_context *_cxt_,
                     operation_info _info_,
                     xpath::PathExpression *_path_expr_,
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_,
                     schema_node_xptr _root_) : PPIterator(_cxt_, _info_, "PPAbsPath"),
                                                path_expr(_path_expr_),
                                                db_ent(_db_ent_),
                                                name(_name_),
                                                root(_root_)
{
}

PPAbsPath::~PPAbsPath()
{
    if (name.op)
    {
        delete name.op;
        name.op = NULL;
    }
}

void PPAbsPath::do_open ()
{
    merged_seq_arr = NULL;
    scmnodes_num = -1;

    if (name.op)
    {
        name.op->open();
        root = XNULL;
    }
}

void PPAbsPath::do_reopen()
{
    delete [] merged_seq_arr;
    merged_seq_arr = NULL;
    scmnodes_num = -1;

    if (name.op)
    {
        name.op->reopen();
        root = XNULL;
    }
}


void PPAbsPath::do_close()
{
    delete [] merged_seq_arr;
    merged_seq_arr = NULL;
    scmnodes_num = -1;
    root = XNULL;

    if (name.op) name.op->close();
}

void PPAbsPath::do_next(tuple &t)
{
    if (root == XNULL && determine_root())
    {
        t.set_eos();
        return;
    }

    if (scmnodes_num < 0)
        create_merged_seq(scmnodes_num, merged_seq_arr, root, path_expr);
    else
        elim_disturb(merged_seq_arr, scmnodes_num, sizeof(xptr), doc_order_merge_cmp);


    xptr res = (scmnodes_num == 0) ? XNULL : merged_seq_arr[0];
    if (res == XNULL)
    {   // reopen
        delete [] merged_seq_arr;
        merged_seq_arr = NULL;
        scmnodes_num = -1;
        root = XNULL;	// there is no need for reopen, because it was called automatically
                        // when we obtained root (eos was reached)

        t.set_eos();
    }
    else
    {
        t.copy(tuple_cell::node(res));
        merged_seq_arr[0] = getNextDescriptorOfSameSort(res);
    }
}

bool PPAbsPath::determine_root()
{
    tuple_cell tc;
    if (name.op)
    {
        tc = get_name_from_PPOpIn(name, "fn:doc() or fn:collection()", "XPath expression", true);
        if (tc.is_eos()) return true;

        tc = tuple_cell::make_sure_light_atomic(tc);
        if (db_ent->name) delete [] db_ent->name;
        db_ent->name = new char[tc.get_strlen_mem() + 1];
        strcpy(db_ent->name, tc.get_str_mem());
    }

    document_type dt = get_document_type(db_ent);

    if (dt == DT_NON_SYSTEM)
        root = get_schema_node(db_ent, "Unknown entity passed to fn:doc() or fn:collection() in XPath expression");
    else
        root = get_system_doc(dt, db_ent->name);

    return false;
}

PPIterator* PPAbsPath::do_copy(dynamic_context *_cxt_)
{
    PPAbsPath *res = NULL;

    if (name.op)
    {
        res = se_new PPAbsPath(_cxt_, info, path_expr, db_ent, name);
        res->name.op = name.op->copy(_cxt_);
    }
    else
    {
        res = se_new PPAbsPath(_cxt_, info, path_expr, db_ent, name, root);
    }

    return res;
}

void PPAbsPath::create_merged_seq(int &scmnodes_num,
                                  xptr*& merged_seq_arr,
                                  schema_node_cptr root,
                                  xpath::PathExpression *path_expr)
{
    t_scmnodes nodes;
    executeAbsPathExpression(root, *path_expr, &nodes, NULL, NULL);
    scmnodes_num = nodes.size();

    // create and fill merged_seq_arr
    merged_seq_arr = se_new xptr[scmnodes_num];
    for (int i = 0; i < scmnodes_num; i++)
    {
        xptr first_blk = getNonemptyBlockLookFore(nodes[i]->bblk);
        if (first_blk == XNULL) merged_seq_arr[i] = XNULL;
        else
        {
            CHECKP(first_blk);
            merged_seq_arr[i] = getFirstBlockNode(first_blk);
        }
    }

    qsort(merged_seq_arr, scmnodes_num, sizeof(xptr), doc_order_merge_cmp);
}

void PPAbsPath::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if (name.op) name.op->accept(v);
    v.pop();
}

// true, if PPAbsPath is just wrapping over fn:document/fn:collection call
bool PPAbsPath::isDocCollFunCall() const
{
    return (path_expr->size() == 0);
}

void PPAbsPath::setPathExpr(xpath::PathExpression *pe)
{
    path_expr = pe;
}
