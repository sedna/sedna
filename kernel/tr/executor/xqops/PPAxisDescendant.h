/*
 * File:  PPAxisDescendant.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAXISDESC_H
#define _PPAXISDESC_H

#include <vector>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPath.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/xptrChanneledMerge.h"

///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendant
///////////////////////////////////////////////////////////////////////////////
class PPAxisDescendant : public PPIterator
{
protected:
    typedef void (PPAxisDescendant::*t_next_fun)(tuple &t);

    /* given parameters */
    PPOpIn child;
    NodeTestType nt_type;
    NodeTestData nt_data;
	bool self;
	
    /* obtained parameters and local data */
    xptr cur;
    t_next_fun next_fun;
	std::vector<xptr> descstack;
	std::map<schema_node_xptr,std::vector<schema_node_xptr> > desc_sch;
	xptrChanneledMerge* merge_tree;
	std::vector<schema_node_xptr> * curvect;

	xptr ancestor;
	void init_function();

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
    
	PPAxisDescendant(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_,
                     NodeTestType _nt_type_,
                     NodeTestData _nt_data_,bool _self_);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) {
        (this->*next_fun)(t); 
	}

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPAxisDescendant(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_,
                     NodeTestType _nt_type_,
                     NodeTestData _nt_data_);
    virtual ~PPAxisDescendant();
};


///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendantOrSelf
///////////////////////////////////////////////////////////////////////////////


class PPAxisDescendantOrSelf : public PPAxisDescendant
{
public:
	PPAxisDescendantOrSelf(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _child_,
                           NodeTestType _nt_type_,
                           NodeTestData _nt_data_);
};


///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendantAttr
///////////////////////////////////////////////////////////////////////////////

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
	PPAxisDescendantAttr(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_);
};
#endif
