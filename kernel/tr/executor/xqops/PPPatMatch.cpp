/*
 * File:  PPPatMatch.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPPatMatch.h"
#include "tr/strings/e_string.h"
#include "tr/strings/strings.h"
#include "tr/executor/base/visitor/PPVisitor.h"


void PPPatMatch::cf_choice(void)
{
    switch (pmt)
	{
		case PM_MATCH:    comp_fun = &PPPatMatch::matches; break;
		case PM_REPLACE:  comp_fun = &PPPatMatch::replace; break;
		case PM_TOKENIZE: comp_fun = &PPPatMatch::tokenize; break;
		default: throw USER_EXCEPTION2(SE1003, "Impossible case in pattern matching choise function");
	}
}

static inline const char* get_argument_name(int arg)
{
    switch (arg)
	{
		case 1: return "first";
		case 2: return "second";
		case 3: return "third";
		case 4: return "fourth";
		default: throw USER_EXCEPTION2(SE1003, "Impossible case in pattern matching get argument name function");
	}
}


PPPatMatch::PPPatMatch(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _seq1_, 
                       PPOpIn _seq2_,
                       patmatch_type _pmt_): PPIterator(_cxt_, _info_, "PPPatMatch"), 
                                             tknzr(NULL),
                                             seq1(_seq1_), 
                                             seq2(_seq2_),
                                             ch_cnt(2),
                                             pmt(_pmt_)
{
	cf_choice();
}


PPPatMatch::PPPatMatch(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _seq1_, 
                       PPOpIn _seq2_,
                       PPOpIn _seq3_,
                       patmatch_type _pmt_): PPIterator(_cxt_, _info_, "PPPatMatch"),
                                             tknzr(NULL),
                                             seq1(_seq1_), 
                                             seq2(_seq2_), 
                                             seq3(_seq3_),
                                             ch_cnt(3),
                                             pmt(_pmt_)
{
	cf_choice();
}

PPPatMatch::PPPatMatch(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _seq1_,                      
                       PPOpIn _seq2_,
                       PPOpIn _seq3_,
                       PPOpIn _seq4_,
                       patmatch_type _pmt_): PPIterator(_cxt_, _info_, "PPPatMatch"),
                                             tknzr(NULL),
                                             seq1(_seq1_), 
                                             seq2(_seq2_), 
                                             seq3(_seq3_), 
                                             seq4(_seq4_),
                                             ch_cnt(4),
                                             pmt(_pmt_)
{
	cf_choice();
}

PPPatMatch::~PPPatMatch()
{
	delete seq1.op;
	seq1.op = NULL;
	
	delete seq2.op;
	seq2.op = NULL;
	
	if (ch_cnt > 2)
	{
		delete seq3.op;
		seq3.op = NULL;
		if (ch_cnt > 3)
		{
			delete seq4.op;
			seq4.op = NULL;
		}
	}
}

void PPPatMatch::do_open ()
{
    seq1.op->open();
	seq2.op->open();

	if (ch_cnt > 2)
	{
		seq3.op->open();
		if (ch_cnt > 3)
		{
			seq4.op->open();
		}
	}

	if (tknzr!=NULL)
	{
		delete tknzr;
		tknzr = NULL;
	}
    first_time = true;
}

void PPPatMatch::do_reopen ()
{
    seq1.op->reopen();
	seq2.op->reopen();
	
	if (ch_cnt > 2)
	{
		seq3.op->reopen();
		if (ch_cnt > 3)
		{
			seq4.op->reopen();
		}
	}

	if (tknzr!=NULL)
	{
		delete tknzr;
		tknzr = NULL;
	}
    first_time = true;
}

void PPPatMatch::do_close ()
{
    seq1.op->close();
	seq2.op->close();
	
	if (ch_cnt>2)
	{
		seq3.op->close();
		if (ch_cnt>3)
		{
			seq4.op->close();
		}
	}
	
	if (tknzr!=NULL)
	{
		delete tknzr;
		tknzr = NULL;
	}
}

PPIterator* PPPatMatch::do_copy(dynamic_context *_cxt_)
{
	PPPatMatch *res ;
	switch (ch_cnt)
	{
	case 2:
		res = new PPPatMatch(_cxt_,info,seq1,seq2,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		break;
	case 3:
		res = new PPPatMatch(_cxt_,info,seq1,seq2,seq3,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		res->seq3.op = seq3.op->copy(_cxt_);
		break;
	case 4:
		res = new PPPatMatch(_cxt_,info,seq1,seq2,seq3,seq4,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		res->seq3.op = seq3.op->copy(_cxt_);
		res->seq4.op = seq4.op->copy(_cxt_);
		break;
    default:
        throw USER_EXCEPTION2(SE1003, "Unexpected number of children in pattern matching operation");
	}
    return res;
}

void PPPatMatch::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq1.op->accept(v);
    seq2.op->accept(v);
    if(ch_cnt > 2) seq3.op->accept(v);
    if(ch_cnt > 3) seq4.op->accept(v);
    v.pop();
}


static inline 
tuple_cell check_string_argument(tuple &t, PPOpIn &seq, bool is_empty_allowed, PPPatMatch::patmatch_type pmt, int arg_num)
{
    seq.op->next(t);
    if(t.is_eos()) 
    {
        if(is_empty_allowed) return EMPTY_STRING_TC;
        else throw XQUERY_EXCEPTION2(XPTY0004, (std::string("Invalid arity of the ") + get_argument_name(arg_num) + " of " + PPPatMatch::patmatch_type2c_string(pmt) + ". Argument contains empty sequence.").c_str());
    }
    tuple_cell res = atomize(seq.get(t));
    if(!is_string_type(res.get_atomic_type())) 
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string("Invalid type of the ") + get_argument_name(arg_num) + " of " + PPPatMatch::patmatch_type2c_string(pmt) + " (xs_string/derived/promotable is expected).").c_str());
    seq.op->next(t);
    if(!t.is_eos())
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string("Invalid arity of the ") + get_argument_name(arg_num) + " of " + PPPatMatch::patmatch_type2c_string(pmt) + ". Argument contains more than one item.").c_str());
    return res;
}

void PPPatMatch::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        
        tuple_cell t1c, t2c, t3c, t4c;
        
        t1c = check_string_argument(t, seq1, true,  pmt, 1);
		t2c = check_string_argument(t, seq2, false, pmt, 2);
		t3c.set_eos();
		t4c.set_eos();

		if (ch_cnt > 2)
		{
			t3c = check_string_argument(t, seq3, false,  pmt, 3);
			if (ch_cnt > 3)	t4c = check_string_argument(t, seq4, false,  pmt, 4);
		}
		
		(this->*comp_fun)(t, &t1c, &t2c, &t3c, &t4c);
		
		if (tknzr != NULL)
		{
			tknzr->get_next_result(t);
			if (t.is_eos())
			{
				delete tknzr;
				tknzr = NULL;
				first_time = true;
			}
		}
	}
    else 
    {
		if (tknzr != NULL)
		{
			tknzr->get_next_result(t);
			if (t.is_eos())
			{
				delete tknzr;
				tknzr = NULL;
				first_time = true;
			}
		}
		else
		{
			first_time = true;
			t.set_eos();
		}
    }
}

void PPPatMatch::tokenize (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4)
{
	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tflags = tuple_cell::eos();

	if(!t3->is_eos()) tflags = tuple_cell::make_sure_light_atomic(*t3);

	tknzr = charset_handler->tokenize(t1, &tc, &tflags);
}

void PPPatMatch::matches (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4)
{
	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tflags = tuple_cell::eos();

	if (!t3->is_eos()) tflags = tuple_cell::make_sure_light_atomic(*t3);
	
	charset_handler->matches(t, t1, &tc, &tflags);
}

void PPPatMatch::replace (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4)
{
	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tf=tuple_cell::make_sure_light_atomic(*t3);
	tuple_cell tflags = tuple_cell::eos();

	if (!t4->is_eos()) tflags = tuple_cell::make_sure_light_atomic(*t4);
	
	charset_handler->replace(t, t1, &tc, &tf, &tflags);
}
