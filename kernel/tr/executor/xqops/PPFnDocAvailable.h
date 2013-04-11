/*
 * File:  PPFnDocAvailable.h
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNDOCAVAILABLE_H
#define _PPFNDOCAVAILABLE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPFnDocAvailable : public PPIterator
{
protected:
    PPOpIn col_name_op;
    PPOpIn doc_name_op;
    bool first_time;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDocAvailable(dynamic_context *_cxt_, 
                     operation_info _info_,
                     PPOpIn _doc_name_op_);
    PPFnDocAvailable(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _col_name_op_,
                     PPOpIn _doc_name_op_);
    virtual ~PPFnDocAvailable();
};



#endif
