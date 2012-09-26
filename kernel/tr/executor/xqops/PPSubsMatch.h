/*
 * File:  PPSubsMatch.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSUBSMATCH_H
#define _PPSUBSMATCH_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"


///////////////////////////////////////////////////////////////////////////////
/// PPSubsMatch
///////////////////////////////////////////////////////////////////////////////

class PPSubsMatch : public PPIterator
{
public:
    enum subsmatch_type
    {
        SM_CONTAINS
    };
    
    static inline const char* subsmatch_type2c_string(subsmatch_type pm)
    {
        switch(pm)
        {
        case SM_CONTAINS: return "fn:contains()";
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in match function type to string conversion (substring match).");
        }
    }

private:
    PPOpIn seq1;
	PPOpIn seq2;
	subsmatch_type smt;
    bool first_time;
    int comp_fun;
	
	void error(const char* msg);

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPSubsMatch(dynamic_context *_cxt_, 
                operation_info _info_,
                PPOpIn _seq1_, 
                PPOpIn _seq2_, 
                subsmatch_type _smt_);

    virtual ~PPSubsMatch();
	
    /* Factories for Substring Matching */
	template <class a, class b> static void contains(a& it1, a&it1end, b& it2, b& it2end, tuple &t);
	template <class b> static void contains (b &it2, b &it2end, const tuple_cell *tcptr1, tuple &t);
    template <class a, class b> static  int  contains (a& it1, b& it2,str_off_t l1,str_off_t l2);
	
	static PPSubsMatch* PPFnContains(dynamic_context *_cxt_, 
                                     operation_info _info_,
                                     PPOpIn _seq1_, 
                                     PPOpIn _seq2_)	{
        return new PPSubsMatch(_cxt_,_info_,_seq1_,_seq2_,SM_CONTAINS);
	}
    
    inline subsmatch_type get_function_type() const { return smt; }
};


///////////////////////////////////////////////////////////////////////////////
/// PPSubsMatch::contains Implementation
///////////////////////////////////////////////////////////////////////////////

template<class a, class b> int memcmpt(a& it1, b& it2,str_off_t len)
{
	a i1 = it1;
	b i2 = it2;
	for (int i = 0; i < len; ++i, ++i1, ++i2)
	{
		if (*i1!=*i2)return (*i1-*i2);
	}
	return 0;
}

template <class a, class b>  int  PPSubsMatch::contains (a& it1, b& it2,str_off_t l1,str_off_t l2)
{
    if (l2==0) return 0;
    if (l1==0) return -1;

    int j;
    if (l2>l1) return -1;

	/* KARP-RABIN Algorithm */
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
    /* We sure here that a::off_t is enough, since l2 <= l1 */
    it1 += (typename a::off_t)l2;
    a i3=it1;
    
    while (j < l1-l2) 
    {
        if (hx == hy && memcmpt<b,a>(it2, i1, l2) == 0)
        {
            return j;
        }
        hy = ((((hy) - (*i1)*d) << 1) + (*i3));      
        ++j;
        ++i1;	  
        ++i3;
    }
    
    if (hx == hy && memcmpt<b,a>(it2, i1, l2) == 0) return j;
    return -1;
}


#endif
