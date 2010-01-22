/*
 * File:  PPAxisSelf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISSELF_H
#define _PPAXISSELF_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"

class PPAxisSelf : public PPIterator
{
protected:
    
    /* given parameters */
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPAxisSelf(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_,
               NodeTestType _nt_type_,
               NodeTestData _nt_data_);
    virtual ~PPAxisSelf();
    
    inline const NodeTestType& get_node_test_type() { return nt_type; }
    inline const NodeTestData& get_node_test_data() { return nt_data; }
};

#endif
