/*
 * File:  PPSequenceTypes.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSEQUENCETYPES_H
#define _PPSEQUENCETYPES_H

#include "sedna.h"
#include "PPBase.h"
#include "SequenceType.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCast
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPCast : public PPIterator
{
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

    // given parameters
    PPOpIn child;

    // obtained parameters and local data
    bool first_time;
    xmlscm_type target_type;
    bool can_be_empty_seq;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPCast(variable_context *_cxt_,
           PPOpIn _child_,
           xmlscm_type _target_type_,
           bool _can_be_empty_seq_);
    virtual ~PPCast();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCastable
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPCastable : public PPIterator
{
protected:

    PPOpIn child;

    bool first_time;
    xmlscm_type target_type;
    bool can_be_empty_seq;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPCastable(variable_context *_cxt_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_);
    virtual ~PPCastable();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPInstanceOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPInstanceOf : public PPIterator
{
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

    // given parameters
    PPOpIn child;
    sequence_type st;

    // obtained parameters and local data
    bool first_time;
    bool eos_reached;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPInstanceOf(variable_context *_cxt_,
                 PPOpIn _child_,
                 const sequence_type& _st_);
    virtual ~PPInstanceOf();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTreat
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPTreat : public PPIterator
{
protected:
    PPOpIn child;
    sequence_type st;

    sequence_tmp *s;
    int pos;

    bool first_time;
    bool eos_reached;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPTreat(variable_context *_cxt_,
            PPOpIn _child_,
            const sequence_type& _st_);
    virtual ~PPTreat();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPTypeswitch
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPTypeswitch : public PPVarIterator
{
protected:
    arr_of_var_dsc var_dscs;
    PPOpIn source_child;
    PPOpIn default_child;
    arr_of_PPOpIn cases;
    tuple source;

    PPOpIn* effective_case;  //The effective case in a typeswitch expression is the first case clause 
                             //such that the value of the operand expression matches the SequenceType 
                             //in the case clause, using the rules of SequenceType matching. 
                             //If the value of the operand expression does not match any SequenceType 
                             //named in a case clause, the value of this pointer is the default clause.

    arr_of_sequence_type types;

    sequence_tmp *s;


    bool first_time;
    bool eos_reached;
    bool need_reopen;

    inline void reinit_consumer_table();

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
    
    PPTypeswitch(variable_context *_cxt_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_, 
                 const arr_of_sequence_type& _types_,
                 arr_of_PPOpIn _cases_,
                 PPOpIn _default_child_);
    
    virtual ~PPTypeswitch();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next(tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
};

#endif

