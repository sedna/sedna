/*
 * File:  PPAbsPath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPAbsPath.h"
#include "PPUtils.h"
#include "vmm.h"
#include "utils.h"
#include "node_utils.h"
#include "locks.h"
#include "crmutils.h"
#include "merge.h"
#include "metadata.h"

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



PPAbsPath::PPAbsPath(variable_context *_cxt_, 
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_) : PPIterator(_cxt_),
                                                        path_expr(_path_expr_),
                                                        db_ent(_db_ent_),
                                                        name(NULL, 0),
                                                        root(NULL)
{
}

PPAbsPath::PPAbsPath(variable_context *_cxt_, 
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_) : PPIterator(_cxt_),
                                      path_expr(_path_expr_),
                                      db_ent(_db_ent_),
                                      name(_name_),
                                      root(NULL)
{
}

PPAbsPath::PPAbsPath(variable_context *_cxt_, 
                     PathExpr *_path_expr_, 
                     counted_ptr<db_entity> _db_ent_,
                     PPOpIn _name_,
                     schema_node *_root_) : PPIterator(_cxt_),
                                            path_expr(_path_expr_),
                                            db_ent(_db_ent_),
                                            name(_name_),
                                            root(_root_)
{
}

PPAbsPath::~PPAbsPath()
{
//    printf("PPAbsPath::~PPAbsPath() begin\n");
    if (name.op)
    {
        delete name.op;
        name.op = NULL;
    }
//    printf("PPAbsPath::~PPAbsPath() end\n");
}

void PPAbsPath::open ()
{
//    printf("PPAbsPath::open () begin\n");
    merged_seq_arr = NULL;
    scmnodes_num = -1;

    if (name.op)
    {
        name.op->open();
        root = NULL;
    }
//    printf("PPAbsPath::open () end\n");
}


void PPAbsPath::reopen()
{
    delete [] merged_seq_arr;
    merged_seq_arr = NULL;
    scmnodes_num = -1;

    if (name.op)
    {
        name.op->reopen();
        root = NULL;
    }
}


void PPAbsPath::close ()
{
    delete [] merged_seq_arr;
    merged_seq_arr = NULL;
    scmnodes_num = -1;
    root = NULL;

    if (name.op) name.op->close();
}

void PPAbsPath::next(tuple &t)
{
//    printf("++++++++++++\n");
//    path_expr->print();
//    printf("\n");
//    path_expr->print_to_lr();
//    printf("\n");
//    printf("++++++++++++\n");
    

    if (!root) 
        if (determine_root()) 
        {
            t.set_eos();
            return;
        }
       
/*
    printf("PPAbsPath::next 1\n");
    crm_out<<root->bblk.addr;
	CHECKP(root->bblk);
    crm_out<<"\nFIRST="<<((node_blk_hdr*)XADDR(root->bblk))->desc_first <<"\n";
    crm_out<<"\nnode="<<GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(root->bblk)).addr;
	printf("PPAbsPath::next 2\n");
    print_node(GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(root->bblk)), crm_out);
    printf("PPAbsPath::next 3\n");
*/

    if (scmnodes_num < 0)
        create_merged_seq(scmnodes_num, merged_seq_arr, root, path_expr);
    else
        elim_disturb(merged_seq_arr, scmnodes_num, sizeof(xptr), doc_order_merge_cmp);

    //printf("scmnodes_num = %d\n", scmnodes_num);

    xptr res = (scmnodes_num == 0) ? XNULL : merged_seq_arr[0];
    if (res == NULL)
    {   // reopen
        delete [] merged_seq_arr;
        merged_seq_arr = NULL;
        scmnodes_num = -1;
		root = NULL;	// there is no need for reopen, because it was called automatically
						// when we obtained root (eos was reached) 

        t.set_eos();
    }
    else
    {
        t.copy(tuple_cell::node(res));
        merged_seq_arr[0] = getNextDescriptorOfSameSortXptr(res);
    }
}

bool PPAbsPath::determine_root()
{
    tuple_cell tc;
    if (name.op)
    {
        tuple t(1);
        name.op->next(t);
        if (t.is_eos()) return true;

        tc = name.get(t);
        if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
            throw USER_EXCEPTION(FODC0005);

        name.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(FODC0005);
        
        tc = tuple_cell::make_sure_light_atomic(tc);
        if (db_ent->name) delete [] db_ent->name;
        db_ent->name = new char[tc.get_strlen_mem() + 1];
        strcpy(db_ent->name, tc.get_str_mem());
    }

	if (is_document_system(db_ent->name))
		root = get_system_doc(db_ent->name);
	else
	   	root = get_schema_node(db_ent, "Unknown entity passed to PPAbsPath");

	return false;
}

PPIterator* PPAbsPath::copy(variable_context *_cxt_)
{
    PPAbsPath *res = NULL;

    if (name.op)
    {
        res = new PPAbsPath(_cxt_, path_expr, db_ent, name);
        res->name.op = name.op->copy(_cxt_);
    }
    else
    {
        res = new PPAbsPath(_cxt_, path_expr, db_ent, name, root);
    }

    return res;
}

void PPAbsPath::create_merged_seq(int &scmnodes_num, xptr*& merged_seq_arr,
                                  schema_node *root, PathExpr *path_expr)
{
    t_scmnodes nodes;
    nodes = execute_abs_path_expr(root, path_expr);
    scmnodes_num = nodes.size();

    // create and fill merged_seq_arr
    merged_seq_arr = new xptr[scmnodes_num];
    for (int i = 0; i < scmnodes_num; i++) 
    {
        xptr first_blk = nodes[i]->bblk;
        if (first_blk == NULL) merged_seq_arr[i] = XNULL;
        else
        {
            //printf("before CHECKP\n");
            CHECKP(first_blk);
            //printf("after CHECKP\n");
            merged_seq_arr[i] = GETBLOCKFIRSTDESCRIPTORABSOLUTE(XADDR(first_blk));
        }
    }

    qsort(merged_seq_arr, scmnodes_num, sizeof(xptr), doc_order_merge_cmp);
}

bool PPAbsPath::result(PPIterator* cur, variable_context *cxt, void*& r)
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

    res_seq = new sequence(1);
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



