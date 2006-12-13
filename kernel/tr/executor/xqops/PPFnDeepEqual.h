/*
 * File:  PPFnDeepEqual.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPFNDEEPEQUAL_H
#define _PPFNDEEPEQUAL_H

#include "sedna.h"
#include "PPBase.h"

class PPFnDeepEqual : public PPIterator
{
	private:
    
    CollationHandler* handler;
protected:
    // given parameters
    PPOpIn child1;
    PPOpIn child2;
	PPOpIn collation;
	bool first_time;
    bool eos_reached1;
	bool eos_reached2;

    void children(PPOpIn& _child1_, PPOpIn& _child2_) { _child1_ = child1; _child2_ = child2; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFnDeepEqual(dynamic_context *_cxt_,
             PPOpIn _child1_,
             PPOpIn _child2_);
	PPFnDeepEqual(variable_context *_cxt_,
             PPOpIn _child1_,
             PPOpIn _child2_,
			 PPOpIn _collation_);
    virtual ~PPFnDeepEqual();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
	bool are_nodes_deep_equal(xptr& node1,xptr& node2);
 bool are_elements_deep_equal(xptr& node1,xptr& node2);
 bool are_documents_deep_equal(xptr& node1,xptr& node2);
bool are_attributes_equal(xptr& node1,xptr& node2,schema_node* scm1,schema_node* scm2);
 bool are_text_nodes_equal(xptr& node1,xptr& node2);
 bool are_pi_equal(xptr& node1,xptr& node2);
};


#endif
