/*
 * File:  PPFnDeepEqual.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPFNDEEPEQUAL_H
#define _PPFNDEEPEQUAL_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPFnDeepEqual : public PPIterator
{
protected:
    PPOpIn child1;
    PPOpIn child2;
	PPOpIn collation;
	bool first_time;
    bool eos_reached1;
	bool eos_reached2;
	CollationHandler* handler;

	bool are_nodes_deep_equal(xptr& node1,xptr& node2);
    bool are_elements_deep_equal(xptr& node1,xptr& node2,schema_node_cptr scm1,schema_node_cptr scm2);
    bool are_documents_deep_equal(xptr& node1,xptr& node2);
    bool are_attributes_equal(xptr& node1,xptr& node2,schema_node_cptr scm1,schema_node_cptr scm2);
    bool are_text_nodes_equal(xptr& node1,xptr& node2);
    bool are_pi_equal(xptr& node1,xptr& node2);

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDeepEqual(dynamic_context *_cxt_, 
                  operation_info _info_,
                  PPOpIn _child1_,
                  PPOpIn _child2_);

	PPFnDeepEqual(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _child1_,
                  PPOpIn _child2_,
                  PPOpIn _collation_);

    virtual ~PPFnDeepEqual();
};


#endif
