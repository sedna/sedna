/*
 * File:  PPAxisParent.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISPARENT_H
#define _PPAXISPARENT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"

class PPAxisParent : public PPIterator
{
protected:
    typedef void (PPAxisParent::*t_next_fun)(tuple &t);

    /* given parameters */
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

    /* obtained parameters and local data */
    xptr cur;
    t_next_fun next_fun;

    void next_processing_instruction	(tuple &t);
    void next_comment					(tuple &t);
    void next_text						(tuple &t);
    void next_node						(tuple &t);
    void next_qname						(tuple &t);
    void next_attribute					(tuple &t);
    void next_document					(tuple &t);
    void next_wildcard_star				(tuple &t);
    void next_wildcard_ncname_star		(tuple &t);
    void next_wildcard_star_ncname		(tuple &t);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) {
        (this->*next_fun)(t);
    }
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPAxisParent(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_,
                 NodeTestType _nt_type_,
                 NodeTestData _nt_data_);
    virtual ~PPAxisParent();
    
    inline const NodeTestType& get_node_test_type() { return nt_type; }
    inline const NodeTestData& get_node_test_data() { return nt_data; }
};

#endif
