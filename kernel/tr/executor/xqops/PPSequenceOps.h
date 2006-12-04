/*
 * File:  PPSequenceOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSEQUENCEOPS_H
#define _PPSEQUENCEOPS_H

#include "sedna.h"
#include "PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnEmpty
///////////////////////////////////////////////////////////////////////////////
class PPFnEmpty : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    bool eos_reached;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnEmpty(variable_context *_cxt_,
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

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnExists(variable_context *_cxt_,
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

    void children(PPOpIn &_seq_child_, PPOpIn &_pos_child_) 
    { 
        _seq_child_ = seq_child; 
        _pos_child_ = pos_child;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnItemAt(variable_context *_cxt_, 
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
    sequence *s;
    CollationHandler* handler;
    bool has_NaN;

    void children(PPOpIn &_child_, PPOpIn &_collation_child_) { _child_ = child; _collation_child_ = collation_child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnDistinctValues(variable_context *_cxt_,
                       PPOpIn _child_);
    PPFnDistinctValues(variable_context *_cxt_,
                       PPOpIn _child_,
                       PPOpIn _collation_child_);
    virtual ~PPFnDistinctValues();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnReverse
///////////////////////////////////////////////////////////////////////////////
class PPFnReverse : public PPIterator
{
private:
    PPOpIn child;

    sequence_tmp *s;
    int pos;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnReverse(variable_context *_cxt_,
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

    __int64 current_pos;
    double start_pos;
    double length;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnSubsequence(variable_context *_cxt_, 
                    PPOpIn _seq_child_,
                    PPOpIn _start_child_);

    PPFnSubsequence(variable_context *_cxt_, 
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

    __int64 current_pos;
    __int64 remove_pos;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnRemove(variable_context *_cxt_, 
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

    __int64 current_pos;
    __int64 insert_pos;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnInsertBefore(variable_context *_cxt_, 
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

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnZeroOrOne(variable_context *_cxt_,
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

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnOneOrMore(variable_context *_cxt_,
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

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnExactlyOne(variable_context *_cxt_,
                  PPOpIn _child_);
    virtual ~PPFnExactlyOne();
};



#endif
