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

///////////////////////////////////////////////////////////////////////////////
/// PPAxisChild
///////////////////////////////////////////////////////////////////////////////
class PPAxisChild : public PPIterator
{
protected:
    typedef void (PPAxisChild::*t_next_fun)(tuple &t);

    // given parameters
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

    // obtained parameters and local data
    xptr cur;
    t_next_fun next_fun;
    RelChildAxisMerge merge;

    void children(PPOpIn &_child_) { _child_ = child; }

    void next_processing_instruction	(tuple &t);
    void next_comment					(tuple &t);
    void next_text						(tuple &t);
    void next_node						(tuple &t);
    void next_string					(tuple &t);
    void next_qname						(tuple &t);
    void next_wildcard_star				(tuple &t);
    void next_wildcard_ncname_star		(tuple &t);
    void next_wildcard_star_ncname		(tuple &t);
    void next_function_call				(tuple &t);
    void next_var_name					(tuple &t);

    static sequence *next_processing_instruction_s	(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_comment_s					(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_text_s					(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_node_s					(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_string_s					(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_qname_s					(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_wildcard_star_s			(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_wildcard_ncname_star_s	(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_wildcard_star_ncname_s	(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_function_call_s			(sequence *data_seq, PPAxisChild* cur_op);
    static sequence *next_var_name_s				(sequence *data_seq, PPAxisChild* cur_op);


public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) { SET_XQUERY_LINE(__xquery_line); (this->*next_fun)(t); UNDO_XQUERY_LINE;}

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPAxisChild(dynamic_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
    virtual ~PPAxisChild();
};

#endif
