/*
 * File:  PPStringsCompare.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSTRINGSCOMPARE_H
#define _PPSTRINGSCOMPARE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


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
    
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnCompare(dynamic_context *_cxt_, 
                operation_info _info_,
                PPOpIn _str1_child_,
                PPOpIn _str2_child_,
                bool _is_codepoint_equal_ = false);

    PPFnCompare(dynamic_context *_cxt_, 
                operation_info _info_,
                PPOpIn _str1_child_,
                PPOpIn _str2_child_,
                PPOpIn _collation_child_);

    virtual ~PPFnCompare();
};

#endif
