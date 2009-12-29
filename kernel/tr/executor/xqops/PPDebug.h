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
    
    se_ostream *dostr;
    
    /* Information about child operation */
    str_counted_ptr child_name;  
    str_counted_ptr child_info;  

    /* Calls counter */
    int cc;  

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPDebug(dynamic_context *_cxt_,
            operation_info _info_,
            const PPOpIn &_children_,
            const str_counted_ptr &_child_name_);
    
    PPDebug(dynamic_context *_cxt_,
            operation_info _info_,
            const PPOpIn &_children_,
            const str_counted_ptr &_child_name_,
            const str_counted_ptr &_child_info_);

    virtual ~PPDebug();
};


#endif
