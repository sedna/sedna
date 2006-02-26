/*
 * File:  PPPatMatch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPPATMATCH_H
#define _PPPATMATCH_H
#include "tuple.h"
#include "PPUtils.h"

typedef __int16 patmatch_type;
// Abstract base types
#define pm_match 0
#define pm_replace 1
class PPPatMatch : public PPIterator
{
protected:
    typedef void (PPPatMatch::*t_comp_fun)(tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);

    // given parameters
    PPOpIn seq1;
	PPOpIn seq2;
	PPOpIn seq3;
	PPOpIn seq4;
	int ch_cnt;
	patmatch_type pmt;
    bool first_time;
    // obtained parameters and local data
    t_comp_fun comp_fun;
	void cf_choice(void);
	void matches (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void replace (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void children(PPOpIn &_seq1_,PPOpIn &_seq2_,PPOpIn &_seq3_,PPOpIn &_seq4_) 
	{
	   _seq1_ = seq1; 
	   _seq2_ = seq2;
	   _seq3_ = seq3; 
	   _seq4_ = seq4;

   }
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual void next   (tuple &t) ;

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
	virtual strict_fun res_fun () { return result; };
    PPPatMatch(variable_context *_cxt_,
		PPOpIn _seq1_, PPOpIn _seq2_ , patmatch_type _pmt_);
	PPPatMatch(variable_context *_cxt_,
		PPOpIn _seq1_, PPOpIn _seq2_ , PPOpIn _seq3_ ,patmatch_type _pmt_);
	PPPatMatch(variable_context *_cxt_,
		PPOpIn _seq1_, PPOpIn _seq2_ , PPOpIn _seq3_ , PPOpIn _seq4_ , patmatch_type _pmt_);
    virtual ~PPPatMatch();
	////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR Pattern Matching
    ////////////////////////////////////////////////////////////////////////////
	static PPPatMatch* PPFnMatch(variable_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_)
	{
		return new PPPatMatch(_cxt_,_seq1_,_seq2_,pm_match);
	}
};

#endif

