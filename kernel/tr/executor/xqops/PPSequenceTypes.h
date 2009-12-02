/*
 * File:  PPSequenceTypes.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSEQUENCETYPES_H
#define _PPSEQUENCETYPES_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/SequenceType.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPCast
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPCast : public PPIterator
{
protected:
    PPOpIn child;

    bool first_time;
    xmlscm_type target_type;
    bool can_be_empty_seq;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPCast(dynamic_context *_cxt_,
           operation_info _info_,
           PPOpIn _child_,
           xmlscm_type _target_type_,
           bool _can_be_empty_seq_);
    virtual ~PPCast();
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPCastable(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_);
    virtual ~PPCastable();
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPInstanceOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPInstanceOf : public PPIterator
{
protected:
    PPOpIn child;
    sequence_type st;

    bool first_time;
    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPInstanceOf(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_,
                 const sequence_type& _st_);
    virtual ~PPInstanceOf();
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

    sequence *s;
    int pos;

    bool first_time;
    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPTreat(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _child_,
            const sequence_type& _st_);
    virtual ~PPTreat();
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

    sequence *s;


    bool first_time;
    bool eos_reached;
    bool need_reopen;

    inline void reinit_consumer_table();

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
    virtual var_c_id do_register_consumer(var_dsc dsc);
    virtual void do_next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void do_reopen(var_dsc dsc, var_c_id id);
    virtual void do_close (var_dsc dsc, var_c_id id);

public:
    PPTypeswitch(dynamic_context *_cxt_,
                 operation_info _info_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_, 
                 const arr_of_sequence_type& _types_,
                 arr_of_PPOpIn _cases_,
                 PPOpIn _default_child_);
    
    virtual ~PPTypeswitch();
};

#endif

