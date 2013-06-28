/*
 * File:  PPFnAccessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNACCESSORS_H
#define _PPFNACCESSORS_H

#include "common/sedna.h"
#include "tr/executor/xqops/PPAccessors.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeName
///////////////////////////////////////////////////////////////////////////////
class PPFnNodeName : public PPIterator
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
    PPFnNodeName(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_);
    virtual ~PPFnNodeName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNilled
///////////////////////////////////////////////////////////////////////////////
class PPFnNilled : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnNilled(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFnNilled();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnString
///////////////////////////////////////////////////////////////////////////////
class PPFnString : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnString(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFnString();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnData
///////////////////////////////////////////////////////////////////////////////
class PPFnData : public PPIterator
{
protected:
    PPOpIn child;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnData(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _child_);
    virtual ~PPFnData();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnBaseURI
///////////////////////////////////////////////////////////////////////////////
class PPFnBaseURI : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnBaseURI(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _child_);
    virtual ~PPFnBaseURI();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDocumentURI
///////////////////////////////////////////////////////////////////////////////
class PPFnDocumentURI : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnDocumentURI(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _child_);
    virtual ~PPFnDocumentURI();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnStaticBaseUri
///////////////////////////////////////////////////////////////////////////////
class PPFnStaticBaseUri : public PPIterator
{
protected:
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnStaticBaseUri(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPFnStaticBaseUri();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDefaultCollation
///////////////////////////////////////////////////////////////////////////////
class PPFnDefaultCollation : public PPIterator
{

protected:
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnDefaultCollation(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPFnDefaultCollation();
};

#endif
