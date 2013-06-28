/*
 * File:  PPSequenceOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSEQUENCEOPS_H
#define _PPSEQUENCEOPS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/SortedSequence.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnEmpty
///////////////////////////////////////////////////////////////////////////////
class PPFnEmpty : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnEmpty(dynamic_context *_cxt_,
              operation_info _info_,
              PPOpIn _child_);
    virtual ~PPFnEmpty();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnExists
///////////////////////////////////////////////////////////////////////////////
class PPFnExists : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnExists(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFnExists();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnItemAt
///////////////////////////////////////////////////////////////////////////////
class PPFnItemAt : public PPIterator
{
protected:
    PPOpIn seq_child;
    PPOpIn pos_child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnItemAt(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq_child_,
               PPOpIn _pos_child_);
    virtual ~PPFnItemAt();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDistinctValues
///////////////////////////////////////////////////////////////////////////////

class PPFnDistinctValues : public PPIterator
{
protected:
    PPOpIn child;
    PPOpIn collation_child;
    ITupleSerializer *serializer;
    SortedSequence *s;
    CollationHandler *handler;
    tuple_cell ret_val;
    bool has_NaN;

private:
    void make_heavy_atomic(xqp_tuple &t);
    int compare_tc(tuple_cell tc1, tuple_cell tc2);

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

    estr txt_data;

public:
    PPFnDistinctValues(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_);

    PPFnDistinctValues(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_,
                       PPOpIn _collation_child_);
    virtual ~PPFnDistinctValues();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnIndexOf
///////////////////////////////////////////////////////////////////////////////
class PPFnIndexOf : public PPIterator
{
protected:
    PPOpIn seq_child;
    PPOpIn srch_child;
    PPOpIn collation_child;

    tuple_cell search_param;
    
    int64_t pos;

    CollationHandler* handler;
    
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnIndexOf(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _seq_child_,
                PPOpIn _srch_child_);

    PPFnIndexOf(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _seq_child_,
                PPOpIn _srch_child_,
                PPOpIn _collation_child_);
    virtual ~PPFnIndexOf();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnReverse
///////////////////////////////////////////////////////////////////////////////
class PPFnReverse : public PPIterator
{
private:
    PPOpIn child;

    sequence *s;
    int pos;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnReverse(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _child_);
    virtual ~PPFnReverse();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnSubsequence
///////////////////////////////////////////////////////////////////////////////
class PPFnSubsequence : public PPIterator
{
protected:
    PPOpIn seq_child;
    PPOpIn start_child;
    PPOpIn length_child;
    
    bool is_length; 		//equal to length_child.op != NULL;
    bool first_time;
    bool need_reopen;

    int64_t current_pos;
    double start_pos;
    double length;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnSubsequence(dynamic_context *_cxt_, 
                    operation_info _info_,
                    PPOpIn _seq_child_,
                    PPOpIn _start_child_);

    PPFnSubsequence(dynamic_context *_cxt_, 
                    operation_info _info_,
                    PPOpIn _seq_child_,
                    PPOpIn _start_child_,
                    PPOpIn _length_child_);

    virtual ~PPFnSubsequence();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnRemove
///////////////////////////////////////////////////////////////////////////////
class PPFnRemove : public PPIterator
{
protected:
    PPOpIn seq_child;
    PPOpIn pos_child;
    
    bool first_time;

    int64_t current_pos;
    int64_t remove_pos;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnRemove(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq_child_,
               PPOpIn _pos_child_);

    virtual ~PPFnRemove();
};



///////////////////////////////////////////////////////////////////////////////
/// PPFnInsertBefore
///////////////////////////////////////////////////////////////////////////////
class PPFnInsertBefore : public PPIterator
{
protected:
    PPOpIn seq_child;
    PPOpIn pos_child;
    PPOpIn ins_child;
    
    bool first_time;
    bool inserted;       //'eos' reached on the inserted sequence child
    bool eos_reached;    //'eos' reached on the seq_child

    int64_t current_pos;
    int64_t insert_pos;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnInsertBefore(dynamic_context *_cxt_, 
                     operation_info _info_,
                     PPOpIn _seq_child_,
                     PPOpIn _pos_child_,
                     PPOpIn _ins_child_);

    virtual ~PPFnInsertBefore();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnZeroOrOne
///////////////////////////////////////////////////////////////////////////////
class PPFnZeroOrOne : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnZeroOrOne(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _child_);
    virtual ~PPFnZeroOrOne();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnOneOrMore
///////////////////////////////////////////////////////////////////////////////
class PPFnOneOrMore : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnOneOrMore(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _child_);
    virtual ~PPFnOneOrMore();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnExactlyOne
///////////////////////////////////////////////////////////////////////////////
class PPFnExactlyOne : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnExactlyOne(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_);
    virtual ~PPFnExactlyOne();
};


#endif
