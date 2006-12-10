/*
 * File:  PPSubsMatch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSUBSMATCH_H
#define _PPSUBSMATCH_H

#include "sedna.h"

#include "tuple.h"
#include "PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
/// PPSubsMatch
///////////////////////////////////////////////////////////////////////////////
typedef __int16 subsmatch_type;

// Abstract base types
#define sm_contains				0

class PPSubsMatch : public PPIterator
{
protected:
   // template <class a, class b> typedef void (PPSubsMatch::*t_comp_fun)(a& it1,b& it2,int l1,int l2,tuple &t);

    // given parameters
    PPOpIn seq1;
	PPOpIn seq2;
	subsmatch_type smt;
    bool first_time;
    // obtained parameters and local data
    int comp_fun;
	
	void children(PPOpIn &_seq1_,PPOpIn &_seq2_) { _seq1_ = seq1; _seq2_ = seq2;}
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) ;

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPSubsMatch(variable_context *_cxt_,
		PPOpIn _seq1_, PPOpIn _seq2_ , subsmatch_type _smt_);
    virtual ~PPSubsMatch();
	////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR Substring Matching
    ////////////////////////////////////////////////////////////////////////////
	template <class a, class b> static  void contains (a& it1, b& it2,int l1,int l2,tuple &t);
	template <class a, class b> static  int  contains (a& it1, b& it2,int l1,int l2);
	static PPSubsMatch* PPFnContains(variable_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_)
	{
	 return new PPSubsMatch(_cxt_,_seq1_,_seq2_,sm_contains);
	}
};

#endif
