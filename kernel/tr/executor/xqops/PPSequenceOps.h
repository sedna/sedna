/*
 * File:  PPSequenceOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSEQUENCEOPS_H
#define _PPSEQUENCEOPS_H

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
/// PPFnEmpty
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
    sequence *s;
    tuple stored_tuple;

    void children(PPOpIn &_child_) { _child_ = child; }

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
    virtual ~PPFnDistinctValues();
};

#endif
