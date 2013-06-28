/*
 * File:  PPAggrFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAGGRFUNCS_H
#define _PPAGGRFUNCS_H


#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnMaxMin
///////////////////////////////////////////////////////////////////////////////

class PPFnMaxMin : public PPIterator
{
private:
    PPOpIn child;
    PPOpIn collation;
    CollationHandler* handler;
    int i; // 0 means fn:max, 1 means fn:min
    const char* function_name;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnMaxMin(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_);
    PPFnMaxMin(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_,
               PPOpIn _collation_);
    virtual ~PPFnMaxMin();
    inline const char* get_function_name() { return function_name; }
    
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnSumAvg
///////////////////////////////////////////////////////////////////////////////

class PPFnSumAvg : public PPIterator
{
private:
    PPOpIn child;
    PPOpIn zero;
    bool first_time;
    int i; // 0 means fn:sum, 1 means fn:avg
    const char* function_name;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnSumAvg(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_);
    PPFnSumAvg(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_,
               PPOpIn _zero_);
    virtual ~PPFnSumAvg();
    inline const char* get_function_name() { return function_name; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnCount
///////////////////////////////////////////////////////////////////////////////

class PPFnCount : public PPIterator
{
private:
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
    PPFnCount(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFnCount();
};


#endif

