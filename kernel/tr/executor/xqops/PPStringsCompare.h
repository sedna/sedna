/*
 * File:  PPStringsCompare.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSTRINGSCOMPARE_H
#define _PPSTRINGSCOMPARE_H

#include "sedna.h"
#include "PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnCompare
///////////////////////////////////////////////////////////////////////////////
class PPFnCompare : public PPIterator
{
protected:
    PPOpIn str1_child;
    PPOpIn str2_child;
    PPOpIn collation_child;
    bool is_codepoint_equal;
    bool first_time;
    
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnCompare(dynamic_context *_cxt_, 
                PPOpIn _str1_child_,
                PPOpIn _str2_child_,
                bool _is_codepoint_equal_ = false);

    PPFnCompare(dynamic_context *_cxt_, 
                PPOpIn _str1_child_,
                PPOpIn _str2_child_,
                PPOpIn _collation_child_);

    virtual ~PPFnCompare();
};

#endif
