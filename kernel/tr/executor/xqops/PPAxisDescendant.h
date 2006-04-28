/*
 * File:  PPAxisDescendant.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISDESC_H
#define _PPAXISDESC_H

#include <vector>

#include "sedna.h"

#include "PPBase.h"
#include "XPath.h"
#include "node_utils.h"
#include "xptrChanneledMerge.h"

///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendant
///////////////////////////////////////////////////////////////////////////////
class PPAxisDescendant : public PPIterator
{
protected:
    typedef void (PPAxisDescendant::*t_next_fun)(tuple &t);

    // given parameters
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;
	bool self;
	
    // obtained parameters and local data
    xptr cur;
    t_next_fun next_fun;
	std::vector<xptr> descstack;
	std::map<schema_node*,std::vector<schema_node*> > desc_sch;
	xptrChanneledMerge* merge_tree;
	std::vector<schema_node*> * curvect;
	//int curpos;
	xptr ancestor;
	void init_function();
    void children(PPOpIn &_child_) { _child_ = child; }

    virtual void next_processing_instruction	(tuple &t);
    virtual void next_comment					(tuple &t);
    virtual void next_text						(tuple &t);
    virtual void next_node						(tuple &t);
    virtual void next_qname						(tuple &t);
    virtual void next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun);
    virtual void next_wildcard_star				(tuple &t);
    virtual void next_wildcard_ncname_star		(tuple &t);
    virtual void next_wildcard_star_ncname		(tuple &t);
    virtual void next_function_call				(tuple &t);
    
/*
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

*/
	PPAxisDescendant(variable_context *_cxt_,
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
		(this->*next_fun)(t); 
	}

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPAxisDescendant(variable_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
    virtual ~PPAxisDescendant();
};
class PPAxisDescendantOrSelf : public PPAxisDescendant
{
	public:
	PPAxisDescendantOrSelf(variable_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
};
class PPAxisDescendantAttr: public PPAxisDescendant
{
protected:	
	void next_node						(tuple &t);
	void next_text						(tuple &t);
	void next_qname						(tuple &t);
	void next_wildcard_star				(tuple &t);
	void next_wildcard_ncname_star		(tuple &t);
    void next_wildcard_star_ncname		(tuple &t);
public:
	PPAxisDescendantAttr(variable_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_);
};
#endif
