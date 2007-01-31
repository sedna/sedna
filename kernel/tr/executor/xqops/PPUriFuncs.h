/*
 * File:  PPUriFuncs.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPURIFUNCS_H
#define _PPURIFUNCS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnUriEncoding
///////////////////////////////////////////////////////////////////////////////
class PPFnUriEncoding : public PPIterator
{

/// Exact type of the function to be evaluated.
public: 
enum uri_function_type {
     ENCODE_FOR_URI,        //fn:encode-for-uri()
     IRI_TO_URI,            //fn:iri-to-uri()
     ESCAPE_HTML_URI        //fn:escape-html-uri()
};

private:
    PPOpIn child;
    uri_function_type type;
    bool first_time;

    const char* error();

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnUriEncoding(dynamic_context *_cxt_,
                    PPOpIn _child_,
                    uri_function_type _type_);
    virtual ~PPFnUriEncoding();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveUri
///////////////////////////////////////////////////////////////////////////////
class PPFnResolveUri : public PPIterator
{
private:
    PPOpIn relative;
    PPOpIn base;

    bool first_time;
    bool is_base_static;
    bool need_reopen;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnResolveUri(dynamic_context *_cxt_,
                   PPOpIn _relative_);
    PPFnResolveUri(dynamic_context *_cxt_,
                   PPOpIn _relative_,
                   PPOpIn _base_);
    virtual ~PPFnResolveUri();
};


#endif
