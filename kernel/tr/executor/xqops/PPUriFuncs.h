/*
 * File:  PPUriFuncs.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPURIFUNCS_H
#define _PPURIFUNCS_H

#include "sedna.h"
#include "PPBase.h"

enum uri_function_type {
    ENCODE_FOR_URI,
    IRI_TO_URI,
    ESCAPE_HTML_URI
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnUriEncoding
///////////////////////////////////////////////////////////////////////////////
class PPFnUriEncoding : public PPIterator
{
private:
    PPOpIn child;
    uri_function_type type;

    const char* error();

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnUriEncoding(variable_context *_cxt_,
                    PPOpIn _child_,
                    uri_function_type _type_);
    virtual ~PPFnUriEncoding();
};


#endif