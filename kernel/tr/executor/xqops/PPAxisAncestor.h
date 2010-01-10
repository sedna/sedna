/*
* File:  PPAxisAncestor.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/


#ifndef _PPAXISANCESTOR_H
#define _PPAXISANCESTOR_H

#include <vector>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"

///////////////////////////////////////////////////////////////////////////////
/// PPAxisAncestor
///////////////////////////////////////////////////////////////////////////////

class PPAxisAncestor : public PPIterator
{
protected:
    typedef void (PPAxisAncestor::*t_next_fun)(tuple &t);

    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;
    bool self;

    xptr cur;
    t_next_fun next_fun;	

    void init_function();

    virtual void next_processing_instruction    (tuple &t);
    virtual void next_comment                   (tuple &t);
    virtual void next_text                      (tuple &t);
    virtual void next_node                      (tuple &t);
    virtual void next_qname                     (tuple &t);
    virtual void next_document                  (tuple &t);
    virtual void next_attribute                 (tuple &t);
    virtual void next_wildcard_star             (tuple &t);
    virtual void next_wildcard_ncname_star      (tuple &t);
    virtual void next_wildcard_star_ncname      (tuple &t);

    PPAxisAncestor(dynamic_context *_cxt_,
        operation_info _info_,
        PPOpIn _child_,
        NodeTestType _nt_type_,
        NodeTestData _nt_data_,
        bool _self_);

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
    PPAxisAncestor(dynamic_context *_cxt_,
        operation_info _info_,
        PPOpIn _child_,
        NodeTestType _nt_type_,
        NodeTestData _nt_data_);
    virtual ~PPAxisAncestor();
};


///////////////////////////////////////////////////////////////////////////////
/// PPAxisAncestorOrSelf
///////////////////////////////////////////////////////////////////////////////

class PPAxisAncestorOrSelf : public PPAxisAncestor
{
public:
    PPAxisAncestorOrSelf(dynamic_context *_cxt_,
        operation_info _info_,
        PPOpIn _child_,
        NodeTestType _nt_type_,
        NodeTestData _nt_data_);
private:
    virtual void do_accept (PPVisitor &v);
};
#endif
