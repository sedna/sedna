/*
 * File:  PPSpaceSequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSPACESEQUENCE_H
#define __PPSPACESEQUENCE_H

#include "sedna.h"
#include "PPSequence.h"

class PPSpaceSequence : public PPSequence
{
private:
	static tuple_cell space_tup;
	tuple int_tup;
	bool space;
	bool isAtomized;

public:
	virtual PPIterator* copy(dynamic_context *_cxt_);
	virtual void next   (tuple &t);
    PPSpaceSequence(dynamic_context *_cxt_,
               const arr_of_PPOpIn &_children_, bool _IsAtomized_);
    virtual ~PPSpaceSequence();
};


#endif
