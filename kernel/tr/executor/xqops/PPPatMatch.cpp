/*
 * File:  PPPatMatch.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPPatMatch.h"
#include "e_string.h"
#include "strings.h"


void PPPatMatch::cf_choice(void)
{
switch (pmt)
	{
	case pm_match: comp_fun = &PPPatMatch::matches;break;
	case pm_replace: comp_fun = &PPPatMatch::replace;break;
	case pm_tokenize: 
		{
			comp_fun = &PPPatMatch::tokenize;break;
			tknzr=NULL;
		}
		break;
	}
}
PPPatMatch::PPPatMatch(variable_context *_cxt_,
						 PPOpIn _seq1_, PPOpIn _seq2_,patmatch_type _pmt_):
			PPIterator(_cxt_), seq1(_seq1_) , seq2(_seq2_),pmt(_pmt_),ch_cnt(2)
{
	cf_choice();
}
PPPatMatch::PPPatMatch(variable_context *_cxt_,
						 PPOpIn _seq1_, PPOpIn _seq2_,PPOpIn _seq3_,patmatch_type _pmt_):
			PPIterator(_cxt_), seq1(_seq1_) , seq2(_seq2_), seq3(_seq3_),pmt(_pmt_),ch_cnt(3),tknzr(NULL)
{
	cf_choice();
}
PPPatMatch::PPPatMatch(variable_context *_cxt_,
						 PPOpIn _seq1_, PPOpIn _seq2_,PPOpIn _seq3_,PPOpIn _seq4_,patmatch_type _pmt_):
			PPIterator(_cxt_), seq1(_seq1_) , seq2(_seq2_), seq3(_seq3_), seq4(_seq4_),pmt(_pmt_),ch_cnt(4),tknzr(NULL)
{
	cf_choice();
}

PPPatMatch::~PPPatMatch()
{
	delete seq1.op;
	seq1.op = NULL;
	delete seq2.op;
	seq2.op = NULL;
	if (ch_cnt>2)
	{
		delete seq3.op;
		seq3.op = NULL;
		if (ch_cnt>3)
		{
			delete seq4.op;
			seq4.op = NULL;
		}
	}
}
void PPPatMatch::open  ()
{
    seq1.op->open();
	seq2.op->open();
	if (ch_cnt>2)
	{
		seq3.op->open();
		if (ch_cnt>3)
		{
			seq4.op->open();
		}
	}
	if (tknzr!=NULL) delete tknzr;
    first_time = true;
 }
void PPPatMatch::reopen  ()
{
    seq1.op->reopen();
	seq2.op->reopen();
	if (ch_cnt>2)
	{
		seq3.op->reopen();
		if (ch_cnt>3)
		{
			seq4.op->reopen();
		}
	}
	if (tknzr!=NULL) delete tknzr;
    first_time = true;
 }
void PPPatMatch::close  ()
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
	if (tknzr!=NULL) delete tknzr;
}
bool PPPatMatch::result(PPIterator* cur, variable_context *cxt, void*& r)
{
 return true;
}
PPIterator* PPPatMatch::copy(variable_context *_cxt_)
{
	PPPatMatch *res ;
	switch (ch_cnt)
	{
	case 2:
		res = new PPPatMatch(_cxt_, seq1,seq2,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		break;
	case 3:
		res = new PPPatMatch(_cxt_, seq1,seq2,seq3,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		res->seq3.op = seq3.op->copy(_cxt_);
		break;
	case 4:
		res = new PPPatMatch(_cxt_, seq1,seq2,seq3,seq4,pmt);
		res->seq1.op = seq1.op->copy(_cxt_);
		res->seq2.op = seq2.op->copy(_cxt_);
		res->seq3.op = seq3.op->copy(_cxt_);
		res->seq4.op = seq4.op->copy(_cxt_);
		break;
	}
    return res;
}
void PPPatMatch::next  (tuple &t)
{
    //d_printf1("1\n");
    if (first_time)
    {

		first_time = false;
		seq1.op->next(t);
		
		//Preliminary node analysis
		tuple_cell t1c= t.cells[0];
		if (t.is_eos())t1c.set_eos();
		if (!t1c.is_eos())
		{
			t1c= atomize(t1c);
			if (t1c.get_atomic_type()==xs_untypedAtomic)
				t1c.set_xtype(xs_string);
			else
				if (!is_string_type(t1c.get_atomic_type()))
			throw USER_EXCEPTION(XPTY0004);
		}
		seq2.op->next(t);
		tuple_cell t2c= t.cells[0];
		if (t.is_eos())t2c.set_eos();
		if (!t2c.is_eos())
		{
			t2c= atomize(t2c);          
			if (t2c.get_atomic_type()==xs_untypedAtomic)
				t2c.set_xtype(xs_string);
			else		
				if (!is_string_type(t2c.get_atomic_type()))
					throw USER_EXCEPTION(XPTY0004);
		}
		tuple_cell t3c;
		tuple_cell t4c;
		if (ch_cnt>2)
		{
			seq3.op->next(t);
			t3c= t.cells[0];
			if (t.is_eos())t3c.set_eos();
			if (!t3c.is_eos())
			{
				t3c= atomize(t3c);          
				if (t3c.get_atomic_type()==xs_untypedAtomic)
				t3c.set_xtype(xs_string);
				else		
				if (!is_string_type(t3c.get_atomic_type()))
					throw USER_EXCEPTION(XPTY0004);
			}
			if (ch_cnt>3)
			{
				seq4.op->next(t);
				t4c= t.cells[0];
				if (t.is_eos())t4c.set_eos();
				if (!t4c.is_eos())
				{
					t4c= atomize(t4c);          
					if (t4c.get_atomic_type()==xs_untypedAtomic)
						t4c.set_xtype(xs_string);
					else		
						if (!is_string_type(t4c.get_atomic_type()))
							throw USER_EXCEPTION(XPTY0004);
				}
			}
		}
		
		//apply function
		//(this->*comp_fun)(t,&t1c,&t2c,&t3c,&t4c);

		if (!t1c.is_eos())
		{
			seq1.op->next(t);
			if (!t.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
		if (!t2c.is_eos())
		{
			seq2.op->next(t);
			if (!t.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
		if (!t3c.is_eos())
		{
			seq3.op->next(t);
			if (!t.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
		if (!t4c.is_eos())
		{
			seq4.op->next(t);
			if (!t.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
		//apply function
		(this->*comp_fun)(t,&t1c,&t2c,&t3c,&t4c);
		if (tknzr!=NULL)
		{
			tknzr->get_next_result(t);
			if (t.is_eos())
			{
				delete tknzr;
				first_time = true;
			}
		}
	}
    else 
    {
		if (tknzr!=NULL)
		{
			tknzr->get_next_result(t);
			if (t.is_eos())
			{
				delete tknzr;
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
	//Preliminary work
	//1.if t1 is empty sequence
	
	if (t1->is_eos())
	{
		t1 = &EMPTY_STRING_TC;
	}
	//2. light atomization of second parameter
	if (t2->is_eos())
		throw USER_EXCEPTION(XPTY0004);

	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tflags = tuple_cell::eos();

	if (t3 != NULL && !t3->is_eos())
		tflags = tuple_cell::make_sure_light_atomic(*t3);
	
	//3. tokenizer
	tknzr=charset_handler->tokenize( t1, &tc, &tflags);
}
void PPPatMatch::matches (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4)
{
	//Preliminary work
	//1.if t1 is empty sequence
	
	if (t1->is_eos())
	{
		t1 = &EMPTY_STRING_TC;
	}
	//2. light atomization of second parameter
	if (t2->is_eos())
		throw USER_EXCEPTION(XPTY0004);

	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tflags = tuple_cell::eos();

	if (t3 != NULL && !t3->is_eos())
		tflags = tuple_cell::make_sure_light_atomic(*t3);
	
	//3. matcher
	charset_handler->matches(t, t1, &tc, &tflags);
}
void PPPatMatch::replace (tuple &t,tuple_cell *t1,tuple_cell *t2,tuple_cell *t3,tuple_cell *t4)
{
	//Preliminary work
	//1.if t1 is empty sequence
	if (t1->is_eos())
	{
		t1 = &EMPTY_STRING_TC;
	}
	//2. light atomization of second parameter
	if (t2->is_eos())
		throw USER_EXCEPTION(XPTY0004);

	tuple_cell tc=tuple_cell::make_sure_light_atomic(*t2);
	tuple_cell tf=tuple_cell::make_sure_light_atomic(*t3);
	tuple_cell tflags = tuple_cell::eos();

	if (t4 != NULL && !t4->is_eos())
		tflags = tuple_cell::make_sure_light_atomic(*t4);
	
	charset_handler->replace(t, t1, &tc, &tf, &tflags);
}
