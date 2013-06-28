/*
 * File:  PPPatMatch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPPATMATCH_H
#define _PPPATMATCH_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"

class PPPatMatch : public PPIterator
{
public:
    enum patmatch_type
    {
        PM_MATCH,
        PM_REPLACE,
        PM_TOKENIZE
    };
    static inline const char* patmatch_type2c_string(patmatch_type pm)
    {
        switch(pm)
        {
        case PM_MATCH: return "fn:match()";
        case PM_REPLACE: return "fn:replace()";
        case PM_TOKENIZE: return "fn:tokenize()";
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in match function type to string conversion (pattern match).");
        }
    }

private:
	TokenizerResult* tknzr;
    typedef void (PPPatMatch::*t_comp_fun)(xqp_tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);

    PPOpIn seq1;
	PPOpIn seq2;
	PPOpIn seq3;
	PPOpIn seq4;
	int ch_cnt;
	patmatch_type pmt;
    bool first_time;

    t_comp_fun comp_fun;
	void cf_choice(void);
	void matches (xqp_tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void tokenize (xqp_tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);
	void replace (xqp_tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4);

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
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
    
    inline patmatch_type get_function_type() const { return pmt; }
};

#endif /* _PPPATMATCH_H */

