/*
 * File:  PPSpaceSequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSPACESEQUENCE_H
#define __PPSPACESEQUENCE_H

#include "common/sedna.h"
#include "tr/executor/xqops/PPSequence.h"

class PPSpaceSequence : public PPSequence
{
protected:
	static tuple_cell space_tup;
	xqp_tuple int_tup;
	bool space;
	bool isAtomized;

private:
	virtual PPIterator* do_copy(dynamic_context *_cxt_);
	virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

public:    
    PPSpaceSequence(dynamic_context *_cxt_,
                    operation_info _info_,
                    const arr_of_PPOpIn &_children_, 
                    bool _IsAtomized_);
    virtual ~PPSpaceSequence();
    inline bool is_atomized() { return isAtomized; }
};

#endif
