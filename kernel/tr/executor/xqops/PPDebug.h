/*
 * File:  PPDebug.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPDEBUG_H
#define __PPDEBUG_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDebug : public PPIterator
{
protected:
    PPOpIn child;
    
    /// information about child operation
    str_counted_ptr child_name;  
    str_counted_ptr child_info;  

    /// calls counter
    int cc;  

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPDebug(dynamic_context *_cxt_,
            const PPOpIn &_children_,
            const str_counted_ptr &_child_name_);
    
    PPDebug(dynamic_context *_cxt_,
            const PPOpIn &_children_,
            const str_counted_ptr &_child_name_,            
            const str_counted_ptr &_child_info_);

    virtual ~PPDebug();
};


#endif