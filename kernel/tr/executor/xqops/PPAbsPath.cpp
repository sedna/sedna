/*
 * File:  PPAbsPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/xqops/PPAbsPath.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/vmm/vmm.h"
#include "common/utils.h"
#include "tr/crmutils/node_utils.h"
#include "tr/locks/locks.h"
#include "tr/crmutils/crmutils.h"
#include "tr/executor/base/merge.h"
#include "tr/structures/metadata.h"

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
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_) : PPIterator(_cxt_),
                                                        path_expr(_path_expr_),
                                                        db_ent(_db_ent_),
                                                        name(NULL, 0),
                                                        root(XNULL)
{
}

PPAbsPath::PPAbsPath(dynamic_context *_cxt_, 
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_) : PPIterator(_cxt_),
                                      path_expr(_path_expr_),
                                      db_ent(_db_ent_),
                                      name(_name_),
                                      root(XNULL)
{
}

PPAbsPath::PPAbsPath(dynamic_context *_cxt_, 
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_,
                     schema_node_xptr _root_) : PPIterator(_cxt_),
                                            path_expr(_path_expr_),
                                            db_ent(_db_ent_),
                                            name(_name_),
                                            root(_root_)
{
}

PPAbsPath::~PPAbsPath()
{
//    d_printf1("PPAbsPath::~PPAbsPath() begin\n");
    if (name.op)
    {
        delete name.op;
        name.op = NULL;
    }
//    d_printf1("PPAbsPath::~PPAbsPath() end\n");
}

void PPAbsPath::open ()
{
//    d_printf1("PPAbsPath::open () begin\n");
    merged_seq_arr = NULL;
    scmnodes_num = -1;

    if (name.op)
    {
        name.op->open();
        root = XNULL;
    }
//    d_printf1("PPAbsPath::open () end\n");
}


void PPAbsPath::reopen()
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


void PPAbsPath::close ()
{
    delete [] merged_seq_arr;
    merged_seq_arr = NULL;
    scmnodes_num = -1;
    root = XNULL;

    if (name.op) name.op->close();
}

void PPAbsPath::next(tuple &t)
{
    SET_CURRENT_PP(this);

    if (root == XNULL) 
        if (determine_root()) 
        {
            t.set_eos();
            {RESTORE_CURRENT_PP; return;}
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
        merged_seq_arr[0] = getNextDescriptorOfSameSortXptr(res);
    }

    RESTORE_CURRENT_PP;
}

bool PPAbsPath::determine_root()
{
    tuple_cell tc;
    if (name.op)
    {
        tuple t(1);
        name.op->next(t);
        if (t.is_eos()) return true;                                 ///If $uri is the empty sequence, the result is an empty sequence

        tc= atomize(name.get(t));
        if(!is_string_type(tc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:doc (xs_string/derived/promotable is expected).");
        name.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument in fn:doc. Argument contains more than one item.");

        tc = tuple_cell::make_sure_light_atomic(tc);
        if (db_ent->name) delete [] db_ent->name;
        db_ent->name = se_new char[tc.get_strlen_mem() + 1];
        strcpy(db_ent->name, tc.get_str_mem());
    }

	document_type dt = get_document_type(db_ent);

	if (dt == DT_NON_SYSTEM)
        root = get_schema_node(db_ent, "Unknown entity passed to PPAbsPath");
	else
	   	root = get_system_doc(dt, db_ent->name);

	return false;
}

PPIterator* PPAbsPath::copy(dynamic_context *_cxt_)
{
    PPAbsPath *res = NULL;

    if (name.op)
    {
        res = se_new PPAbsPath(_cxt_, path_expr, db_ent, name);
        res->name.op = name.op->copy(_cxt_);
    }
    else
    {
        res = se_new PPAbsPath(_cxt_, path_expr, db_ent, name, root);
    }
    res->set_xquery_line(__xquery_line);

    return res;
}

void PPAbsPath::create_merged_seq(int &scmnodes_num, xptr*& merged_seq_arr,
                                  schema_node_cptr root, PathExpr *path_expr)
{
    t_scmnodes nodes;
    nodes = execute_abs_path_expr(root, path_expr);
    scmnodes_num = nodes.size();

    // create and fill merged_seq_arr
    merged_seq_arr = se_new xptr[scmnodes_num];
    for (int i = 0; i < scmnodes_num; i++) 
    {
        xptr first_blk = getUnemptyBlockFore(nodes[i]->bblk);
        if (first_blk == XNULL) merged_seq_arr[i] = XNULL;
        else
        {
            CHECKP(first_blk);
            merged_seq_arr[i] = GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(first_blk));
        }
    }

    qsort(merged_seq_arr, scmnodes_num, sizeof(xptr), doc_order_merge_cmp);
}

bool PPAbsPath::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    sequence *res_seq = NULL;
    int scmnodes_num = -1;
    xptr *merged_seq_arr = NULL;
    schema_node *root = NULL;

    PPAbsPath *_cur_ = (PPAbsPath *)cur;

    switch (_cur_->db_ent->type)
    {
        case dbe_document	: root = find_document  (_cur_->db_ent->name); break;
        case dbe_collection	: root = find_collection(_cur_->db_ent->name); break;
        default				: throw USER_EXCEPTION2(SE1003, "Unknown entity passed to PPAbsPath");
    }    

    create_merged_seq(scmnodes_num, 
                      merged_seq_arr, 
                      root, 
                      _cur_->path_expr.get());

    res_seq = se_new sequence(1);
    xptr res;
    tuple t(1);

    if (scmnodes_num != 0)
    {
        while ((res = merged_seq_arr[0]) != NULL)
        {
            t.copy(tuple_cell::node(res));
            res_seq->add(t);
            merged_seq_arr[0] = getNextDescriptorOfSameSortXptr(res);
        }

    }
    delete [] merged_seq_arr;

    return strict_op_result(cur, res_seq, cxt, r);
*/
    return true;
}



