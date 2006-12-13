/*
 * File:  PPGeneralComparison.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPGeneralComparison.h"
#include "comparison_operations.h"
#include "casting_operations.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPGeneralComparison
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR General Comparisons
  ////////////////////////////////////////////////////////////////////////////

PPGeneralComparison* PPGeneralComparison::PPGTGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPLMGeneralComparison(_cxt_,_seq1_,_seq2_,true);
}
PPGeneralComparison* PPGeneralComparison::PPLTGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPLMGeneralComparison(_cxt_,_seq1_,_seq2_,false);
}
PPGeneralComparison* PPGeneralComparison::PPGEGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPLMGeneralComparison(_cxt_,_seq1_,_seq2_,true,false);
}
PPGeneralComparison* PPGeneralComparison::PPLEGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPLMGeneralComparison(_cxt_,_seq1_,_seq2_,false,false);
}
PPGeneralComparison* PPGeneralComparison::PPEQGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPEQLGeneralComparison(_cxt_,_seq1_,_seq2_);
}
PPGeneralComparison* PPGeneralComparison::PPNEGeneralComparison(dynamic_context *_cxt_, 
																PPOpIn _seq1_, PPOpIn _seq2_)
{ 
	return new PPNEQGeneralComparison(_cxt_,_seq1_,_seq2_);
}
PPGeneralComparison::PPGeneralComparison(dynamic_context *_cxt_,PPOpIn _seq1_, PPOpIn _seq2_): PPIterator(_cxt_),
                                    seq1(_seq1_),seq2(_seq2_)
{
}

PPNEQGeneralComparison::PPNEQGeneralComparison(dynamic_context *_cxt_, 
											 PPOpIn _seq1_, PPOpIn _seq2_): PPGeneralComparison(_cxt_,_seq1_,_seq2_)
{

}
PPEQLGeneralComparison::PPEQLGeneralComparison(dynamic_context *_cxt_, 
											 PPOpIn _seq1_, PPOpIn _seq2_): PPGeneralComparison(_cxt_,_seq1_,_seq2_)
{

}
void PPGeneralComparison::generalNodePrepare(tuple_cell& cell1, tuple_cell& cell2)
{
	if (cell1.get_atomic_type()==xs_untypedAtomic && cell2.get_atomic_type()==xs_untypedAtomic)
	{
		cell1=cast_primitive_to_xs_string(cell1);
		cell2=cast_primitive_to_xs_string(cell2);
		return; 
	}
	if (cell1.get_atomic_type()==xs_untypedAtomic && is_numeric_type(cell2.get_atomic_type()))
	{
		cell1=cast(cell1, xs_double);
		return; 
	}
	if (cell2.get_atomic_type()==xs_untypedAtomic && is_numeric_type(cell1.get_atomic_type()))
	{
		cell2=cast(cell2, xs_double);
		return; 
	}
	
	if (cell1.get_atomic_type()==xs_untypedAtomic && cell2.get_atomic_type()!=xs_untypedAtomic)
	{
		cell1=cast(cell1,cell2.get_atomic_type());
		return;
	}

	if (cell2.get_atomic_type()==xs_untypedAtomic && cell1.get_atomic_type()!=xs_untypedAtomic)
	{
		cell2=cast(cell2,cell1.get_atomic_type());
		return;
	}
}

PPGeneralComparison::~PPGeneralComparison()
{
	delete seq1.op;
	seq1.op = NULL;
	delete seq2.op;
	seq2.op = NULL;
}
	
void PPGeneralComparison::open  ()
{
    seq1.op->open();
	seq2.op->open();
    first_time = true;
    eos_reached1 = true;
	eos_reached2 = true;
}

void PPGeneralComparison::reopen()
{
    seq1.op->reopen();
	seq2.op->reopen();
    first_time = true;
    eos_reached1 = true;
	eos_reached2 = true;
}

void PPGeneralComparison::close ()
{
    seq1.op->close();
	seq2.op->close();
}

void PPGeneralComparison::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
		
		
		//Result
		//t.copy(tuple_cell::node(removeIndirection(indir)));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPGeneralComparison::copy(dynamic_context *_cxt_)
{
	PPGeneralComparison *res ;
	res = new PPGeneralComparison(_cxt_, seq1,seq2);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}
bool PPGeneralComparison::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
 return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPLMGeneralComparison
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
xmlscm_type PPLMGeneralComparison::fill_minimums(tuple_cell value)
{
	xmlscm_type ret_type=xs_untyped;
	if (!is_fixed_size_type(value.get_atomic_type()))
	{
		if (min_str)
		{
			if (value_comp_lt(value,min_str_cell).get_xs_boolean()) 
			{
				min_str_cell=value;
				ret_type=xs_string;
			}
		}
		else
		{
			min_str_cell=value;
			min_str=true;
			ret_type=xs_string;
		}
		try
		{ 
			tuple_cell num_val=cast(value, xs_double);
			if (min_ut_num)
			{
				if (value_comp_lt(num_val,min_ut_num_cell).get_xs_boolean()) 
				{
					min_ut_num_cell=num_val;
					ret_type=xs_string;				
				}
			}
			else
			{
				min_ut_num_cell=num_val;
				min_ut_num=true;
				ret_type=xs_string;
			}
		}
		catch(...)
		{
		 if (max_num)
			 throw USER_EXCEPTION(FORG0001);
			min_num_cast=true;
		}
		try
		{ 
			tuple_cell num_val=cast(value,xs_date);
			if (min_ut_dat)
			{
				if (value_comp_lt(num_val,min_ut_dat_cell).get_xs_boolean()) 
				{
					min_ut_dat_cell=num_val;
					ret_type=xs_string;
				}
			}
			else
			{
				min_ut_dat_cell=num_val;
				min_ut_dat=true;
				ret_type=xs_string;
			}
		}
		catch(...)
		{
			if (max_dat)
			 throw USER_EXCEPTION(FORG0001);
			min_dat_cast=true;
		}
		return ret_type;
	}
	if (is_numeric_type(value.get_atomic_type()))
	{
		if (min_num)
		{
			if (value_comp_lt(value,min_num_cell).get_xs_boolean()) 
			{
				min_num_cell=value;
				ret_type=xs_double;
			}
		}
		else
		{
			min_num_cell=value;
			min_num=true;
			ret_type=xs_double;
		}
		return ret_type;
	}
	if (value.get_type()==xs_date)
	{
		if (min_dat)
		{
			if (value_comp_lt(value,min_dat_cell).get_xs_boolean()) 
			{
				min_dat_cell=value;
				ret_type=xs_date;
			}
		}
		else
		{
			min_dat_cell=value;
			min_dat=true;
			ret_type=xs_date;
		}
		return ret_type;
	}
	return ret_type;
}
xmlscm_type PPLMGeneralComparison::fill_maximums(tuple_cell value)
{
	xmlscm_type ret_type=xs_untyped;
	if (!is_fixed_size_type(value.get_atomic_type()))
	{
		if (max_str)
		{
			if (value_comp_gt(value,max_str_cell).get_xs_boolean()) 
			{
				max_str_cell=value;
				ret_type=xs_string;
			}
		}
		else
		{
			max_str_cell=value;
			max_str=true;
			ret_type=xs_string;
		}
		try
		{ 
			tuple_cell num_val=cast(value, xs_double);
			if (max_ut_num)
			{
				if (value_comp_gt(num_val,max_ut_num_cell).get_xs_boolean()) 
				{
					max_ut_num_cell=num_val;
					ret_type=xs_string;				
				}
			}
			else
			{
				max_ut_num_cell=num_val;
				max_ut_num=true;
				ret_type=xs_string;
			}
		}
		catch(...)
		{
			if (min_num)
			 throw USER_EXCEPTION(FORG0001);
			max_num_cast=true;
		}
		try
		{ 
			tuple_cell num_val=cast(value,xs_date);
			if (max_ut_dat)
			{
				if (value_comp_gt(num_val,max_ut_dat_cell).get_xs_boolean()) 
				{
					max_ut_dat_cell=num_val;
					ret_type=xs_string;
				}
			}
			else
			{
				max_ut_dat_cell=num_val;
				max_ut_dat=true;
				ret_type=xs_string;
			}
		}
		catch(...)
		{
			if (min_dat)
			 throw USER_EXCEPTION(FORG0001);
			max_dat_cast=true;
		}
		return ret_type;
	}
	if (is_numeric_type(value.get_atomic_type()))
	{
		if (max_num)
		{
			if (value_comp_gt(value,max_num_cell).get_xs_boolean()) 
			{
				max_num_cell=value;
				ret_type=xs_double;
			}
		}
		else
		{
			max_num_cell=value;
			max_num=true;
			ret_type=xs_double;
		}
		return ret_type;
	}
	if (value.get_type()==xs_date)
	{
		if (max_dat)
		{
			if (value_comp_gt(value,max_dat_cell).get_xs_boolean()) 
			{
				max_dat_cell=value;
				ret_type=xs_date;
			}
		}
		else
		{
			max_dat_cell=value;
			max_dat=true;
			ret_type=xs_date;
		}
		return ret_type;
	}
	return ret_type;	
}
bool PPLMGeneralComparison::compare_minmax(xmlscm_type type_info, bool min_changed)
{
	if (type_info==xs_untyped) return false;
	if (min_changed)
	{
		switch (type_info)
		{
		case xs_string:
			if (min_str && max_str && value_comp_lt(min_str_cell,max_str_cell).get_xs_boolean()) return true;
			if (min_ut_num && max_num && value_comp_lt(min_ut_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_ut_dat && max_dat && value_comp_lt(min_ut_dat_cell,max_dat_cell).get_xs_boolean()) return true;
            break;
		case xs_double:
			if (min_num && max_num && value_comp_lt(min_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_num && max_ut_num && value_comp_lt(min_num_cell,max_ut_num_cell).get_xs_boolean()) return true;
			break;
		case xs_date:
			if (min_dat && max_dat && value_comp_lt(min_dat_cell,max_dat_cell).get_xs_boolean()) return true;
			if (min_dat && max_ut_dat && value_comp_lt(min_dat_cell,max_ut_dat_cell).get_xs_boolean()) return true;
			break;
		}
	}
	else
	{
		switch (type_info)
		{
		case xs_string:
			if (min_str && max_str && value_comp_lt(min_str_cell,max_str_cell).get_xs_boolean()) return true;
			if (min_num && max_ut_num && value_comp_lt(min_num_cell,max_ut_num_cell).get_xs_boolean()) return true;
			if (min_dat && max_ut_dat && value_comp_lt(min_dat_cell,max_ut_dat_cell).get_xs_boolean()) return true;
			break;
		case xs_double:
			if (min_ut_num && max_num && value_comp_lt(min_ut_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_num && max_num && value_comp_lt(min_num_cell,max_num_cell).get_xs_boolean()) return true;
			break;
		case xs_date:
			if (min_ut_dat && max_dat && value_comp_lt(min_ut_dat_cell,max_dat_cell).get_xs_boolean()) return true;
           	if (min_dat && max_dat && value_comp_lt(min_dat_cell,max_dat_cell).get_xs_boolean()) return true;
				break;
		}
	}
	return false;
}
bool PPLMGeneralComparison::compare_minmax_le(xmlscm_type type_info, bool min_changed)
{
	if (type_info==xs_untyped) return false;
	if (min_changed)
	{
		switch (type_info)
		{
		case xs_string:
			if (min_str && max_str && value_comp_le(min_str_cell,max_str_cell).get_xs_boolean()) return true;
			if (min_ut_num && max_num && value_comp_le(min_ut_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_ut_dat && max_dat && value_comp_le(min_ut_dat_cell,max_dat_cell).get_xs_boolean()) return true;
            break;
		case xs_double:
			if (min_num && max_num && value_comp_le(min_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_num && max_ut_num && value_comp_le(min_num_cell,max_ut_num_cell).get_xs_boolean()) return true;
			break;
		case xs_date:
			if (min_dat && max_dat && value_comp_le(min_dat_cell,max_dat_cell).get_xs_boolean()) return true;
			if (min_dat && max_ut_dat && value_comp_le(min_dat_cell,max_ut_dat_cell).get_xs_boolean()) return true;
			break;
		}
	}
	else
	{
		switch (type_info)
		{
		case xs_string:
			if (min_str && max_str && value_comp_le(min_str_cell,max_str_cell).get_xs_boolean()) return true;
			if (min_num && max_ut_num && value_comp_le(min_num_cell,max_ut_num_cell).get_xs_boolean()) return true;
			if (min_dat && max_ut_dat && value_comp_le(min_dat_cell,max_ut_dat_cell).get_xs_boolean()) return true;
			break;
		case xs_double:
			if (min_ut_num && max_num && value_comp_le(min_ut_num_cell,max_num_cell).get_xs_boolean()) return true;
			if (min_num && max_num && value_comp_le(min_num_cell,max_num_cell).get_xs_boolean()) return true;
			break;
		case xs_date:
			if (min_ut_dat && max_dat && value_comp_le(min_ut_dat_cell,max_dat_cell).get_xs_boolean()) return true;
           	if (min_dat && max_dat && value_comp_le(min_dat_cell,max_dat_cell).get_xs_boolean()) return true;
				break;
		}
	}
	return false;
}

PPLMGeneralComparison::PPLMGeneralComparison(dynamic_context *_cxt_, 
					 PPOpIn _seq1_, PPOpIn _seq2_, bool _more_): PPGeneralComparison(_cxt_,_seq1_,_seq2_),
					 more(_more_)
{
	strict=true;
}
PPLMGeneralComparison::PPLMGeneralComparison(dynamic_context *_cxt_, 
					 PPOpIn _seq1_, PPOpIn _seq2_, bool _more_,bool _strict_): PPGeneralComparison(_cxt_,_seq1_,_seq2_),
					 more(_more_),strict(_strict_)
{
}
PPIterator* PPLMGeneralComparison::copy(dynamic_context *_cxt_)
{
	PPLMGeneralComparison *res ;
	res = new PPLMGeneralComparison(_cxt_, seq1,seq2,more);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}

 /*void PPLMGeneralComparison::next   (tuple &t)
^ ^ ^ ^ ^ ^ ^
{
	if (first_time)
    {
        first_time = false;
		PPOpIn* _seq1_,*_seq2_;
		if (more)
		{
			_seq2_=&seq1;
			_seq1_=&seq2;			
		}
		else
		{
			_seq1_=&seq1;
			_seq2_=&seq2;			
		}
		// SET_SIGNS_INIT_SECTION
		min_ut_num=false;
		min_ut_dat=false;
		min_num=false;
		min_dat=false;
		min_str=false;
		max_ut_num=false;
		max_ut_dat=false;
		max_num=false;
		max_dat=false;
		max_str=false;
		min_dat_cast=false;
		min_num_cast=false;
		max_dat_cast=false;
		max_num_cast=false;
				
		//Main cycle
		if (!eos_reached1) _seq1_->op->reopen();
		if (!eos_reached2) _seq2_->op->reopen();
		eos_reached2 = false;
		eos_reached1 = false;
		tuple cont(_seq1_->ts);
		//first values from both sequences
        _seq1_->op->next(cont);
		if (cont.is_eos())
		{
			eos_reached1 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		tuple_cell resf=getAtomizedCell(cont);
		xmlscm_type in_type=fill_minimums(resf);
		_seq2_->op->next(cont);
		if (cont.is_eos())
		{
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		tuple_cell ress=getAtomizedCell(cont);
		fill_maximums(ress);
		
		while (true)
		{
			if ((strict &&compare_minmax(in_type,true)) ||(!strict &&compare_minmax_le(in_type,true)))
			{
				t.copy(tuple_cell::atomic(true));
				return;
			}
			_seq1_->op->next(cont);
			if (cont.is_eos()) 
			{
				eos_reached1 = true;			
				break;
			}
			resf=getAtomizedCell(cont);
			in_type=fill_minimums(resf);
		}
		while (true)
		{
			_seq2_->op->next(cont);
			if (cont.is_eos()) 
			{
				eos_reached2 = true;
				break;
			}
			resf=getAtomizedCell(cont);
			in_type=fill_maximums(resf);
			if ((strict &&compare_minmax(in_type,false)) ||(!strict &&compare_minmax_le(in_type,false)))
			{
				t.copy(tuple_cell::atomic(true));
				return;
			}
		}
		if (
			(min_dat_cast && max_dat)||
			(min_num_cast && max_num)||
			(max_dat_cast && min_dat)||
			(max_num_cast && min_num)||
			(min_num && max_dat)||
			(min_dat && max_num)
			)
		throw USER_EXCEPTION(XP0021);
		t.copy(tuple_cell::atomic(false));
		return;
		//t.copy(tuple_cell::node(removeIndirection(indir)));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
v v v v v v v
}*/

void PPNEQGeneralComparison::next   (tuple &t)
{
	if (first_time)
    {
        first_time = false;
		if (!eos_reached1) seq1.op->reopen();
		if (!eos_reached2) seq2.op->reopen();
		eos_reached2 = false;
		eos_reached1 = false;
		tuple cont1(seq1.ts);
		tuple cont2(seq2.ts);
		tuple * cont[2]={&cont1,&cont2};
		PPOpIn * seq[2]={&seq1,&seq2};
		bool seq_end[2]={false,false};
		bool two_diffs_exist[2]={false,false};
		bool uv_exist[2]={false,false};
		bool switcher=true;
		tuple_cell seq_str_val[2];
		seq[0]->op->next(*cont[0]);
		if ((*cont[0]).is_eos())
		{
			eos_reached1 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		seq[1]->op->next(*cont[1]);
		if ((*cont[1]).is_eos())
		{
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		tuple_cell res[2];
		res[0]=getAtomizedCell(*cont[0]);
		if (res[0].get_atomic_type()==xs_untypedAtomic) 
		{
			seq_str_val[0]=res[0];
			uv_exist[0]=true;
		}
		res[1]=getAtomizedCell(*cont[1]);
		if (res[1].get_atomic_type()==xs_untypedAtomic) 
		{
			seq_str_val[1]=res[1];
			uv_exist[1]=true;
		}
		generalNodePrepare(res[0],res[1]);
		if (value_comp_ne(res[0],res[1]).get_xs_boolean())
		{
			t.copy(tuple_cell::atomic(true));
			return;
		}
		int pr=0;
		while(true)
		{
			(switcher)?pr=0:pr=1;
			switcher=!switcher;
			if (!seq_end[pr] )
			{
				seq[pr]->op->next(*cont[pr]);
				if (cont[pr]->is_eos())
				{
					(pr==0)? eos_reached1 = true: eos_reached2 = true;
					seq_end[pr]=true;
					if (seq_end[1-pr])
					{
						t.copy(tuple_cell::atomic(false));
						return;
					}
				}
				else
				{
					res[pr]=getAtomizedCell(*cont[pr]);
					if (res[pr].get_atomic_type()==xs_untypedAtomic) 
					{
						if (!uv_exist[pr])
						{
							seq_str_val[pr]=res[pr];
							uv_exist[pr]=true;
						}
						else
						{
							if (value_comp_ne(cast(seq_str_val[pr], xs_string),cast(res[pr], xs_string)).get_xs_boolean())
								two_diffs_exist[pr]=true;
						}
						if (
							(two_diffs_exist[pr]&& uv_exist[1-pr])||
							(two_diffs_exist[1-pr]&& uv_exist[pr])
							)
						{
							t.copy(tuple_cell::atomic(true));
							return;
						}
						if (uv_exist[1-pr])
						{
							if (value_comp_ne(cast(seq_str_val[pr], xs_string),cast(seq_str_val[1-pr], xs_string)).get_xs_boolean())
							{
								t.copy(tuple_cell::atomic(true));
								return;
							}
						}
					}
					generalNodePrepare(res[0],res[1]);
					if (value_comp_ne(res[0],res[1]).get_xs_boolean())
					{
						t.copy(tuple_cell::atomic(true));
						return;
					}
				}
			}
		}
	}
	else 
    {
        first_time = true;
        t.set_eos();
    }
}
void PPEQLGeneralComparison::next   (tuple &t)
{
	if (first_time)
    {
		first_time = false;
		//INSERT CODE HERE
		if (!eos_reached1) seq1.op->reopen();
		if (!eos_reached2) seq2.op->reopen();
		eos_reached2 = false;
		eos_reached1 = false;
		tuple cont1(seq1.ts);
		tuple cont2(seq2.ts);
		seq1.op->next(cont1);
		sequence seq(1);
		tuple at_tup(1);
		seq2.op->next(cont2);
		if (cont2.is_eos())
		{
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		tuple_cell res1=getAtomizedCell(cont2);
		while (!cont1.is_eos())
		{
			tuple_cell res=getAtomizedCell(cont1);
			at_tup.cells[0]=res;
			generalNodePrepare(res,res1);
			if (value_comp_eq(res,res1).get_xs_boolean())
			{
				t.copy(tuple_cell::atomic(true));
				return;
			}
			seq.add(at_tup);
			seq1.op->next(cont1);
		}
		eos_reached1 = true;
		if (seq.size()<1)
		{
			t.copy(tuple_cell::atomic(false));
			return;
		}
		seq2.op->next(cont2);
		while (!cont2.is_eos())
		{
			tuple_cell res1=getAtomizedCell(cont2);
			sequence::iterator it=seq.begin();
			do
			{
				tuple_cell res2=(*it).cells[0];
				generalNodePrepare(res1,res2);
				if (value_comp_eq(res1,res2).get_xs_boolean())
				{
					t.copy(tuple_cell::atomic(true));
					return;
				}
				it++;
			}
			while (it!=seq.end());
			seq2.op->next(cont2);
		}
		eos_reached2 = true;
		t.copy(tuple_cell::atomic(false));
		return;

	}
	else 
    {
        first_time = true;
        t.set_eos();
    }
}
void PPLMGeneralComparison::next   (tuple &t)
{
 	if (first_time)
    {
		first_time = false;
		tuple_cell (*comp_op) (const tuple_cell&,const tuple_cell&);
		if (strict)
		{
			if (more)
				comp_op=value_comp_gt;
			else
				comp_op=value_comp_lt;
		}
		else
		{
			if (more)
				comp_op=value_comp_ge;
			else
				comp_op=value_comp_le;
		}
		//INSERT CODE HERE
		if (!eos_reached1) seq1.op->reopen();
		if (!eos_reached2) seq2.op->reopen();
		eos_reached2 = false;
		eos_reached1 = false;
		tuple cont1(seq1.ts);
		tuple cont2(seq2.ts);
		seq1.op->next(cont1);
		sequence seq(1);
		tuple at_tup(1);
		seq2.op->next(cont2);
		if (cont2.is_eos())
		{
			eos_reached2 = true;
			t.copy(tuple_cell::atomic(false));
			return;
		}
		tuple_cell res1=getAtomizedCell(cont2);
		while (!cont1.is_eos())
		{
			tuple_cell res=getAtomizedCell(cont1);
			at_tup.cells[0]=res;
			generalNodePrepare(res,res1);
			if (comp_op(res,res1).get_xs_boolean())
			{
				t.copy(tuple_cell::atomic(true));
				return;
			}
			seq.add(at_tup);
			seq1.op->next(cont1);
		}
		eos_reached1 = true;
		if (seq.size()<1)
		{
			t.copy(tuple_cell::atomic(false));
			return;
		}
		seq2.op->next(cont2);
		while (!cont2.is_eos())
		{
			tuple_cell res1=getAtomizedCell(cont2);
			sequence::iterator it=seq.begin();
			do
			{
				tuple_cell res2=(*it).cells[0];
				generalNodePrepare(res1,res2);
				if (comp_op(res2,res1).get_xs_boolean())
				{
					t.copy(tuple_cell::atomic(true));
					return;
				}
				it++;
			}
			while (it!=seq.end());
			seq2.op->next(cont2);
		}
		eos_reached2 = true;
		t.copy(tuple_cell::atomic(false));
		return;

	}
	else 
    {
        first_time = true;
        t.set_eos();
    }
}
PPIterator* PPEQLGeneralComparison::copy(dynamic_context *_cxt_)
{
	PPEQLGeneralComparison *res ;
	res = new PPEQLGeneralComparison(_cxt_, seq1,seq2);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}
