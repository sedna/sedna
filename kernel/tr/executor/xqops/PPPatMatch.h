/*
 * File:  PPPatMatch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPPATMATCH_H
#define _PPPATMATCH_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"

typedef __int16 patmatch_type;

#define pm_match 0
#define pm_replace 1
#define pm_tokenize 2

class PPPatMatch : public PPIterator
{
private:
	TokenizerResult* tknzr;
protected:
    typedef void (PPPatMatch::*t_comp_fun)(tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);

    PPOpIn seq1;
	PPOpIn seq2;
	PPOpIn seq3;
	PPOpIn seq4;
	int ch_cnt;
	patmatch_type pmt;
    bool first_time;

    t_comp_fun comp_fun;
	void cf_choice(void);
	void matches (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void tokenize (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void replace (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPPatMatch(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq1_,
               PPOpIn _seq2_,
               patmatch_type _pmt_);
               
	PPPatMatch(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq1_,
               PPOpIn _seq2_,
               PPOpIn _seq3_,
               patmatch_type _pmt_);

	PPPatMatch(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq1_,
               PPOpIn _seq2_,
               PPOpIn _seq3_,
               PPOpIn _seq4_,
               patmatch_type _pmt_);

    virtual ~PPPatMatch();
	
    /* Factories for Pattern Matching */
	static PPPatMatch* PPFnMatch(dynamic_context *_cxt_, 
                                 operation_info _info_,
                                 PPOpIn _seq1_,
                                 PPOpIn _seq2_)	{
		return se_new PPPatMatch(_cxt_, _info_, _seq1_,_seq2_,pm_match);
	}
};

#endif

