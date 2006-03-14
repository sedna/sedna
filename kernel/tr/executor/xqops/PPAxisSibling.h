#ifndef _PPAXISSIBLING_H
#define _PPAXISSIBLING_H
#include "PPBase.h"
#include "XPath.h"
#include "node_utils.h"
#include "xptrChanneledMerge.h"
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
	std::map<schema_node*,std::vector<schema_node*> > desc_sch;	
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
    virtual void next   (tuple &t) { (this->*next_fun)(t); }

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPAxisSibling(variable_context *_cxt_,
                PPOpIn _child_,
                NodeTestType _nt_type_,
                NodeTestData _nt_data_,bool _following_);
    virtual ~PPAxisSibling();
};

#endif