/*
 * File:  PPAxisChild.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISCHILD_H
#define _PPAXISCHILD_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"
#include "tr/executor/base/merge.h"

class PPAxisChild : public PPIterator
{
protected:
    typedef void (PPAxisChild::*t_next_fun)(tuple &t);

    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

    xptr cur;
    t_next_fun next_fun;
    RelChildAxisMerge merge;

    void next_processing_instruction    (tuple &t);
    void next_comment                   (tuple &t);
    void next_text                      (tuple &t);
    void next_node                      (tuple &t);
    void next_qname                     (tuple &t);
    void next_wildcard_star             (tuple &t);
    void next_wildcard_ncname_star      (tuple &t);
    void next_wildcard_star_ncname      (tuple &t);
    void next_eos                       (tuple &t);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) {
        (this->*next_fun)(t);
    }

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPAxisChild(dynamic_context *_cxt_,
                operation_info _info_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
    virtual ~PPAxisChild();
};

#endif
