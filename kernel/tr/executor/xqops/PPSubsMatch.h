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
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPSubsMatch(dynamic_context *_cxt_, PPOpIn _seq1_, PPOpIn _seq2_, subsmatch_type _smt_);
    virtual ~PPSubsMatch();
	
	////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR Substring Matching
    ////////////////////////////////////////////////////////////////////////////
    template <class a, class b> static  void contains (a& it1, b& it2,int l1,int l2,tuple &t);
    template <class a, class b> static  int  contains (a& it1, b& it2,int l1,int l2);
	
	static PPSubsMatch* PPFnContains(dynamic_context *_cxt_, 
                                     PPOpIn _seq1_, 
                                     PPOpIn _seq2_)
	{
        return new PPSubsMatch(_cxt_,_seq1_,_seq2_,sm_contains);
	}
};


///////////////////////////////////////////////////////////////////////////////
/// PPSubsMatch::contains Implementation
///////////////////////////////////////////////////////////////////////////////

template<class a, class b> int memcmp(a& it1, b& it2,int len)
{
	a i1 = it1;
	b i2 = it2;
	for (int i = 0; i < len; ++i, ++i1, ++i2)
	{
		if (*i1!=*i2)return (*i1-*i2);
	}
	return 0;
}

template <class a, class b>  int  PPSubsMatch::contains (a& it1, b& it2,int l1,int l2)
{
    if (l2==0) return 0;
    if (l1==0) return -1;

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
    if (l2>l1) return -1;

	//KARP_RABIN
    int d, hx, hy, i;

    for (d = i = 1; i < l2; ++i) d = (d<<1);

    a i1 = it1;
    b i2 = it2;
    for (hy = hx = i = 0; i < l2; ++i, ++i1, ++i2)
    {
        hx = ((hx<<1) + *i2);
        hy = ((hy<<1) + *i1);
    }

    j = 0;
    i1 = it1;
    it1 += l2;
    a i3=it1;
    
    while (j < l1-l2) 
    {
        if (hx == hy && memcmp<b,a>(it2, i1, l2) == 0)
        {
            return j;
        }
        hy = ((((hy) - (*i1)*d) << 1) + (*i3));      
        ++j;
        ++i1;	  
        ++i3;
    }
    
    if (hx == hy && memcmp<b,a>(it2, i1, l2) == 0) return j;
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
    return -1;
   //second case (unrealized)- really big strings
}


#endif
