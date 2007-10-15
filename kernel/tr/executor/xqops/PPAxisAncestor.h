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
#include "tr/crmutils/node_utils.h"
///////////////////////////////////////////////////////////////////////////////
/// PPAxisAncestor
///////////////////////////////////////////////////////////////////////////////
class PPAxisAncestor : public PPIterator
{
protected:
    typedef void (PPAxisAncestor::*t_next_fun)(tuple &t);

    // given parameters
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;
	bool self;
	
    // obtained parameters and local data
    xptr cur;
    t_next_fun next_fun;	
	
	void init_function();
    void children(PPOpIn &_child_) { _child_ = child; }

    virtual void next_processing_instruction	(tuple &t);
    virtual void next_comment					(tuple &t);
    virtual void next_text						(tuple &t);
    virtual void next_node						(tuple &t);
    virtual void next_qname						(tuple &t);
	virtual void next_string					(tuple &t);
   // virtual void next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun);
    virtual void next_wildcard_star				(tuple &t);
    virtual void next_wildcard_ncname_star		(tuple &t);
    virtual void next_wildcard_star_ncname		(tuple &t);
    virtual void next_function_call				(tuple &t);
	void next_var_name					(tuple &t);
	PPAxisAncestor(dynamic_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_,bool _self_);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) 
	{ 
		SET_XQUERY_LINE(__xquery_line);
		(this->*next_fun)(t); 
		UNDO_XQUERY_LINE;
	}

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPAxisAncestor(dynamic_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
    virtual ~PPAxisAncestor();
};
class PPAxisAncestorOrSelf : public PPAxisAncestor
{
	public:
	PPAxisAncestorOrSelf(dynamic_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
};
#endif
