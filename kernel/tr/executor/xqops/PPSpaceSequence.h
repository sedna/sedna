/*
 * File:  PPSpaceSequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSPACESEQUENCE_H
#define __PPSPACESEQUENCE_H

//#include "PPBase.h"
#include "PPSequence.h"

class PPSpaceSequence : public PPSequence
{
private:
	static tuple_cell space_tup;
	tuple int_tup;
	bool space;

public:
	virtual PPIterator* copy(variable_context *_cxt_);
	virtual void next   (tuple &t);
    PPSpaceSequence(variable_context *_cxt_,
               const arr_of_PPOpIn &_children_);
    virtual ~PPSpaceSequence();
};


#endif
