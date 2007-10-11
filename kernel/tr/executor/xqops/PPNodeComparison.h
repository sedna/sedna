/*
 * File:  PPNodeComparison.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPNODECOMP_H
#define _PPNODECOMP_H

#include "common/sedna.h"
#include "tr/executor/base/PPUtils.h"

class PPNodeComparison : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn seq1;
	PPOpIn seq2;
    bool first_time;
    int type;
    void children(PPOpIn &_seq1_,PPOpIn &_seq2_) { _seq1_ = seq1; _seq2_ = seq2;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
	
    PPNodeComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_, int _type_);
    virtual ~PPNodeComparison();
	////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR General Comparisons
    ////////////////////////////////////////////////////////////////////////////
    static PPNodeComparison* PPEQNodeComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPNodeComparison* PPLTNodeComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPNodeComparison* PPGTNodeComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPNodeComparison* PPANNodeComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	
};
#endif
