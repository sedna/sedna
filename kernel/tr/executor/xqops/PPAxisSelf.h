/*
 * File:  PPAxisSelf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISSELF_H
#define _PPAXISSELF_H

#include "PPBase.h"
#include "XPath.h"

///////////////////////////////////////////////////////////////////////////////
/// PPAxisSelf
///////////////////////////////////////////////////////////////////////////////
class PPAxisSelf : public PPIterator
{
protected:
    
    // given parameters
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

    // obtained parameters and local data
    
    void children(PPOpIn &_child_) { _child_ = child; }

    


public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) ; 

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPAxisSelf(variable_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
    virtual ~PPAxisSelf();
};

#endif
