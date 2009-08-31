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

protected:
    PPOpIn child;
    uri_function_type type;
    bool first_time;

    const char* error();

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnUriEncoding(dynamic_context *_cxt_,
                    operation_info _info_, 
                    PPOpIn _child_,
                    uri_function_type _type_);
    virtual ~PPFnUriEncoding();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveUri
///////////////////////////////////////////////////////////////////////////////
class PPFnResolveUri : public PPIterator
{
protected:
    PPOpIn relative;
    PPOpIn base;

    bool first_time;
    bool is_base_static;
    bool need_reopen;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnResolveUri(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _relative_);
    PPFnResolveUri(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _relative_,
                   PPOpIn _base_);
    virtual ~PPFnResolveUri();
};


#endif
