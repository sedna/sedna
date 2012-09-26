/*
 * File:  PPSubsMatch.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSubsMatch.h"
#include "tr/strings/e_string.h"
#include "common/errdbg/d_printf.h"
#include "tr/strings/e_string_iterator.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPSubsMatch::PPSubsMatch(dynamic_context *_cxt_,
                         operation_info _info_,
						 PPOpIn _seq1_, 
						 PPOpIn _seq2_,
						 subsmatch_type _smt_):	PPIterator(_cxt_, _info_, "PPSubsMatch"), 
						                        seq1(_seq1_), 
						                        seq2(_seq2_),
						                        smt(_smt_)
{
    switch (smt)
    {
        case SM_CONTAINS: comp_fun = 0; break;
        default:          throw USER_EXCEPTION2(SE1003, "Imposible type of function in PPSubsMatch::PPSubsMatch().");
    }
}

template <class a, class b> void PPSubsMatch::contains(a& it1, a&it1end, b& it2, b& it2end, tuple &t)
{
	int res = PPSubsMatch::contains<a,b>(it1,it2,it1end-it1,it2end-it2);
	if (res<0)
	{
		t.copy(tuple_cell::atomic(false));
		return;
	}
	else
	{
		t.copy(tuple_cell::atomic(true));
		return;
	}
}

template <class b> void PPSubsMatch::contains(b &it2, b &it2end, const tuple_cell *tcptr1, tuple &t)
{
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(PPSubsMatch::contains, tcptr1, it2, it2end, t);
}

PPSubsMatch::~PPSubsMatch()
{
	delete seq1.op;
	seq1.op = NULL;
	delete seq2.op;
	seq2.op = NULL;
}

void PPSubsMatch::do_open ()
{
    seq1.op->open();
	seq2.op->open();
    first_time = true;
 }

void PPSubsMatch::do_reopen()
{
    seq1.op->reopen();
	seq2.op->reopen();
    first_time = true;
}

void PPSubsMatch::do_close()
{
    seq1.op->close();
	seq2.op->close();
}

void PPSubsMatch::error(const char* msg)
{
    switch (smt)
    {
        case SM_CONTAINS: throw XQUERY_EXCEPTION2(XPTY0004, (std::string(msg) + " in fn:contains().").c_str());
        default:          throw USER_EXCEPTION2(SE1003, "Imposible type of function in PPSubsMatch::error().");
    }
}


void PPSubsMatch::do_next (tuple &t)
{
    if (first_time)
    {

		first_time = false;
		tuple t1(seq1.ts);
		seq1.op->next(t1);
		tuple t2(seq2.ts);
		seq2.op->next(t2);

		tuple_cell t1c= t1.cells[0];
		if (t1.is_eos()) t1c.set_eos();
		if (!t1c.is_eos())
		{
			t1c= atomize(t1c);
			if (t1c.get_atomic_type()==xs_untypedAtomic)
				t1c.set_xtype(xs_string);
			else
				if (!is_string_type(t1c.get_atomic_type())) 
				    error("Invalid type of the first argument (xs_string/derived/promotable is expected)");
		}
		else
		{
			t1c = EMPTY_STRING_TC;
		}


		tuple_cell t2c= t2.cells[0];
		if (t2.is_eos()) t2c.set_eos();
		if (!t2c.is_eos())
		{
			t2c= atomize(t2c);          
			if (t2c.get_atomic_type()==xs_untypedAtomic)
				t2c.set_xtype(xs_string);
			else		
				if (!is_string_type(t2c.get_atomic_type()))
					error("Invalid type of the second argument (xs_string/derived/promotable is expected)");
		}
		else
		{
			t2c = EMPTY_STRING_TC;
		}

		switch (this->comp_fun)
		{
            case 0:
			    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(PPSubsMatch::contains, &t2c, &t1c, t);
			    break;
		}

		if (!t1c.is_eos())
		{
			seq1.op->next(t1);
			if (!t1.is_eos())
				error("Invalid arity of the first argument. Argument contains more than one item");
		}
		
		if (!t2c.is_eos())
		{
			seq2.op->next(t2);
			if (!t2.is_eos())
				error("Invalid arity of the second argument. Argument contains more than one item");
		}
	}
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPSubsMatch::do_copy(dynamic_context *_cxt_)
{
	PPSubsMatch *res ;
	res = new PPSubsMatch(_cxt_, info, seq1, seq2, smt);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}

void PPSubsMatch::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq1.op->accept(v);
    seq2.op->accept(v);
    v.pop();
}
