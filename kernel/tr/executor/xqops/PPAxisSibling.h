/*
 * File:  PPAxisSibling.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPAXISSIBLING_H
#define _PPAXISSIBLING_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/xptrChanneledMerge.h"
///////////////////////////////////////////////////////////////////////////////
/// PPAxisFP
///////////////////////////////////////////////////////////////////////////////

class PPAxisSibling : public PPIterator
{
protected:
    typedef void (PPAxisSibling::*t_next_fun)(tuple &t);

    // given parameters
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;

    // obtained parameters and local data
    xptr cur;
	bool is_col;
    t_next_fun next_fun;   
	std::map<schema_node_xptr,std::vector<schema_node_xptr> > desc_sch;	
	xptrChanneledMerge* merge_tree;

    void children(PPOpIn &_child_) { _child_ = child; }
	void next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun);
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

    bool following;

	

public:
	virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) { SET_CURRENT_PP(this); (this->*next_fun)(t); RESTORE_CURRENT_PP;}

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPAxisSibling(dynamic_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_,bool _following_);
    virtual ~PPAxisSibling();
};

#endif

