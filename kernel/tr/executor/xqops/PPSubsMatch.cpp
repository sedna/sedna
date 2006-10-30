/*
 * File:  PPSubsMatch.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPSubsMatch.h"
#include "e_string.h"
#include "d_printf.h"
#include "e_string_iterator.h"
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPSubstringMatch
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPSubsMatch::PPSubsMatch(variable_context *_cxt_,
						 PPOpIn _seq1_, PPOpIn _seq2_,subsmatch_type _smt_):
			PPIterator(_cxt_), seq1(_seq1_) , seq2(_seq2_),smt(_smt_)
{
 switch (smt)
 {
 case sm_contains: comp_fun = 0;
	 break;
 }
}
inline void*  create_iterator(tuple_cell& t, int&l)
{
	if (t.is_eos()) 
	{
		l=0;
		return NULL;
	}
	else
		if (t.is_light_atomic())
		{
			l=t.get_strlen_mem();
			return t.get_str_mem();
		}
		else
		{
			l=t.get_strlen_vmm();
			return new e_string_iterator_first (l,t.get_str_vmm());
		}
	
}
template<class a, class b> int memcmp(a& it1,b& it2,int len)
{
	a i1 = it1;
	b i2 = it2;
	for (int i = 0; i < len; ++i, ++i1, ++i2)
	{
		if (*i1!=*i2)return (*i1-*i2);
	}
	return 0;
}
template <class a, class b> void PPSubsMatch::contains(a& it1,b& it2,int l1,int l2,tuple &t)
{
	//2 nd argument empty
	if (l2==0)
	{
		t.copy(tuple_cell::atomic(true));
		return;
	}
	//1 st argument empty
	if (l1==0)
	{
		t.copy(tuple_cell::atomic(false));
		return;
	}
	//first case
	
	int j;
	/*
	for (int i=0;i<len1-len2;i++)
	{
	 j=0;
	 while (c1[i+j]==c2[j]) 
	 {
		 if (++j==len2)
		 {
			t.copy(tuple_cell::atomic(true));
			return;
		 }
	 }
	}*/
	if (l2>l1)
	{
		t.copy(tuple_cell::atomic(false));
		return;
	}
	//KARP_RABIN
	int d, hx, hy, i;
	for (d = i = 1; i < l2; ++i)
		d = (d<<1);
	a i1 = it1;
	b i2 = it2;
	for (hy = hx = i = 0; i < l2; ++i, ++i1, ++i2)
	{
		hx = ((hx<<1) + *i2);
		hy = ((hy<<1) + *i1);
	}
   
   j = 0;
   i1 = it1;
   a i3=it1+l2;
   while (j < l1-l2) 
   {
      if (hx == hy && memcmp<b,a>(it2, i1, l2) == 0)
	  {
		t.copy(tuple_cell::atomic(true));
		return;
	  }
	  hy = ((((hy) - (*i1)*d) << 1) + (*i3));      
      ++j;
	  ++i1;	  
	  ++i3;
   }
   if (hx == hy && memcmp<b,a>(it2, i1, l2) == 0)
	  {
		t.copy(tuple_cell::atomic(true));
		return;
	  }
   //KNUTH-PLATT
	/*
	int i, kmpNext[10];
   //prefase
	i = 0;
	j = kmpNext[0] = -1;
	while (i < len2) 
	{
		while (j > -1 && c2[i] != c2[j])
			j = kmpNext[j];
		i++;
		j++;
		if (c2[i] == c2[j])
			kmpNext[i] = kmpNext[j];
		else
			kmpNext[i] = j;
   }
   //algorithm
	i = j = 0;
   while (j < len1) 
   {
      while (i > -1 && c2[i] != c1[j])
         i = kmpNext[i];
      i++;
      j++;
      if (i >= len2) 
	  {
         t.copy(tuple_cell::atomic(false));
		 return;
      }
   }
*/


   t.copy(tuple_cell::atomic(false));
   return;
	//second case (unrealized)- really big strings
}
PPSubsMatch::~PPSubsMatch()
{
	delete seq1.op;
	seq1.op = NULL;
	delete seq2.op;
	seq2.op = NULL;
}
void PPSubsMatch::open  ()
{
    seq1.op->open();
	seq2.op->open();
    first_time = true;
 }
void PPSubsMatch::reopen()
{
    seq1.op->reopen();
	seq2.op->reopen();
    first_time = true;
}
void PPSubsMatch::close ()
{
    seq1.op->close();
	seq2.op->close();
}
void PPSubsMatch::next  (tuple &t)
{
    //d_printf1("1\n");
    if (first_time)
    {

		first_time = false;
		tuple t1(seq1.ts);
		seq1.op->next(t1);
		tuple t2(seq2.ts);
		seq2.op->next(t2);
		//Preliminary node analysis
		tuple_cell t1c= t1.cells[0];
		if (t1.is_eos())t1c.set_eos();
		if (!t1c.is_eos())
		{
			t1c= atomize(t1c);
			if (t1c.get_atomic_type()==xs_untypedAtomic)
				t1c.set_xtype(xs_string);
			else
				if (!is_string_type(t1c.get_atomic_type()))
			throw USER_EXCEPTION(XPTY0004);
		}
		tuple_cell t2c= t2.cells[0];
		if (t2.is_eos())t2c.set_eos();
		if (!t2c.is_eos())
		{
			t2c= atomize(t2c);          
			if (t2c.get_atomic_type()==xs_untypedAtomic)
				t2c.set_xtype(xs_string);
			else		
				if (!is_string_type(t2c.get_atomic_type()))
					throw USER_EXCEPTION(XPTY0004);
		}
		bool mark=false;
		// memory mapping
	/*	if (t1c.is_heavy_atomic())
		{
			if (E_STR_NOT_IN_ONE_BLOCK(t1c))
			{
				//REALLY BIG STRING TO HEAP MEMORY
				char* tmp=new char[t1c.get_strlen_vmm()+1];
				tmp[t1c.get_strlen_vmm()]='\0';
				copy_text(tmp, t1c.get_str_vmm(), t1c.get_strlen_vmm());
				t1c=tuple_cell::atomic(xs_string,tmp);
			}
			else
			{
				CHECKP(t1c.get_str_vmm());
				mark=true;
			}
		}
		if (t2c.is_heavy_atomic())
		{
			if (mark)
			{
				// memory copiing in case when both arguments in xptr address space
				char* tmp=new char[t2c.get_strlen_vmm()+1];
				tmp[t2c.get_strlen_vmm()]='\0';
				copy_text(tmp, t2c.get_str_vmm(), t2c.get_strlen_vmm());
				t2c=tuple_cell::atomic(xs_string,tmp);
				CHECKP(t1c.get_str_vmm());
			}
			else
			{
				if (E_STR_NOT_IN_ONE_BLOCK(t2c))
				{
					//REALLY BIG STRING TO HEAP MEMORY
					char* tmp=new char[t2c.get_strlen_vmm()+1];
					tmp[t2c.get_strlen_vmm()]='\0';
					copy_text(tmp, t2c.get_str_vmm(), t2c.get_strlen_vmm());
					t2c=tuple_cell::atomic(xs_string,tmp);
				}			
				else
				CHECKP(t2c.get_str_vmm());
			}
		}
*/
		int len1,len2;
		void * it1=create_iterator(t1c,len1);
		void * it2=create_iterator(t2c,len2);
    	//apply function
		switch (this->comp_fun)
		{
		case 0:
			if (t1c.is_heavy_atomic())
			{
				if (t2c.is_heavy_atomic())
					contains<e_string_iterator,e_string_iterator>(*((e_string_iterator*)it1),*((e_string_iterator*)it2),len1,len2,t);
				else
					contains<e_string_iterator,char*>(*((e_string_iterator*)it1),(char*&)it2,len1,len2,t);
			}
			else
			{
				if (t2c.is_heavy_atomic())
					this->contains<char*,e_string_iterator>((char*&)it1,*((e_string_iterator*)it2),len1,len2,t);
				else
					this->contains<char*,char*>((char*&)it1,(char*&)it2,len1,len2,t);
			}

			break;
		}

	//	(this->*comp_fun)(t1c,t2c,t);
		if (len1>0 && t1c.is_heavy_atomic()) delete it1;
		if (len2>0 && t2c.is_heavy_atomic()) delete it2;
		if (!t1c.is_eos())
		{
			seq1.op->next(t1);
			if (!t1.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
		if (!t2c.is_eos())
		{
			seq2.op->next(t2);
			if (!t2.is_eos())
				throw USER_EXCEPTION(XPTY0004);
		}
	}
    else 
    {
        first_time = true;
        t.set_eos();
    }
}
bool PPSubsMatch::result(PPIterator* cur, variable_context *cxt, void*& r)
{
 return true;
}
PPIterator* PPSubsMatch::copy(variable_context *_cxt_)
{
	PPSubsMatch *res ;
	res = new PPSubsMatch(_cxt_, seq1,seq2,smt);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}
