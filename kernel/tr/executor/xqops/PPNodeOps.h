/*
 * File:  PPNodeOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __NODEOPS_H
#define __NODEOPS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnName
///////////////////////////////////////////////////////////////////////////////
class PPFnName : public PPIterator
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
    PPFnName(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _child_);
    virtual ~PPFnName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalName
///////////////////////////////////////////////////////////////////////////////
class PPFnLocalName : public PPIterator
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
    PPFnLocalName(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _child_);
    virtual ~PPFnLocalName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUri
///////////////////////////////////////////////////////////////////////////////
class PPFnNamespaceUri : public PPIterator
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
    PPFnNamespaceUri(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_);
    virtual ~PPFnNamespaceUri();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNumber
///////////////////////////////////////////////////////////////////////////////
class PPFnNumber : public PPIterator
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
    PPFnNumber(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFnNumber();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnRoot
///////////////////////////////////////////////////////////////////////////////
class PPFnRoot : public PPIterator
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
    PPFnRoot(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _child_);
    virtual ~PPFnRoot();
};


#endif
