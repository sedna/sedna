/*
 * File:  PPExcept.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPEXCEPT_H
#define _PPEXCEPT_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPExcept : public PPIterator
{
protected:
    PPOpIn child1;
    PPOpIn child2;

    // If true then first and second child are PPDDO or some
    // other operation which returns nodes in document order
    // otherwise children are PPSXptrs which return xptrs sorted
    // by raw value.
    bool doc_order;

    bool tug_first, tug_second;
    bool need_reopen_second;

    xptr xptr1, xptr2;
    tuple_cell tc1, tc2;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPExcept(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _child1_,
             PPOpIn _child2_,
             bool _doc_order);
    virtual ~PPExcept();
    inline bool is_document_order() { return doc_order; }
};


#endif
