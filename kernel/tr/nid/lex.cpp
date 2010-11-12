/*
 * File:  lex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include <string.h>
#include "tr/nid/numb_scheme.h"
#include "tr/nid/lex.h"
#include "tr/nid/nid.h"
#include "tr/nid/nidalloc.h"
#include "common/errdbg/d_printf.h"

t_prefix NULLPREFIX = t_prefix(NULL,0);

/*************************************************************
 None of the lex_ functions free memory occupied by operands
 *************************************************************/

void lex_print(t_prefix p) {
	for (int i=0; i<p.size; i++)
		d_printf2("%d=", (int)(unsigned char)p.prefix[i]);
}

/*	compute "p*factor"
	assume operands in ALPHABET_SIZE numeration
	result not shorter than "p"
 */
t_prefix lex_multiply(t_prefix p, int factor) {
	unsigned char	*t, *r;
	int				it, ir;
	int				tmp;
	int				carry=0;		/* carry value */
	t_prefix		result;

	if (!p.size) return NULLPREFIX;
	result.prefix = (unsigned char*)nid_alloc();
	result.size=p.size;
	for(t=(unsigned char*)p.prefix+p.size-1, r=(unsigned char*)result.prefix+result.size-1; t>=(unsigned char*)p.prefix; t--,r--) {
		it =*t;
		tmp = (it)*factor + carry;
		ir=tmp%ALPHABET_SIZE;
		carry=tmp/ALPHABET_SIZE;
		*r=(unsigned char)ir;
	}
	if (carry) {
		unsigned char* tmpc;
		tmpc = (unsigned char*)nid_alloc();
		tmpc[0]=carry;
		memcpy(tmpc+1, result.prefix, result.size);
		nid_free(result.prefix);
		result.prefix=tmpc;
		result.size++;
	}
	return result;
}

/*	compute "p/divisor"
	assume numbers in ALPHABET_SIZE numeration
 */
t_prefix lex_divide(t_prefix p, int divisor) {
	unsigned char	*t, *r;
	int				it, ir;
	int				tmp;
	int				remainder=0;		/* remainder value */
	t_prefix		result;

	if (!p.size || !divisor) return NULLPREFIX;
	result.prefix = (unsigned char*)nid_alloc();
	result.size = p.size;
	for(t=(unsigned char*)p.prefix, r=(unsigned char*)result.prefix; t-(unsigned char*)p.prefix<p.size; t++,r++) {
		it=*t;
		tmp = (it + ALPHABET_SIZE*remainder);
		ir = tmp/divisor;
		remainder = tmp%divisor;
		*r=(unsigned char)ir;
	}
	return result;
}

/*	compute "p1+p2"
	assume numbers in ALPHABET_SIZE numeration
 */
t_prefix lex_sum(t_prefix p1, t_prefix p2) {
	unsigned char	*s1, *s2, *r;	/* long, short operand and result corr-ly */
	int				is1, is2, ir;
	int				tmp;
	t_prefix		result;
	int				overflow=0;
	int				size;			/* the size of biggest */
	int				size_diff;

	if (p1.size > p2.size) {
		size = p1.size;
		size_diff = p1.size-p2.size;
		s1=(unsigned char*)p1.prefix+p1.size-1;
		s2=(unsigned char*)p2.prefix+p2.size-1;
	} else {
		size = p2.size;
		size_diff = p2.size-p1.size;
		s1=(unsigned char*)p2.prefix+p2.size-1;
		s2=(unsigned char*)p1.prefix+p1.size-1;
	}
	result.prefix = (unsigned char*)nid_alloc();
	result.size = size;
	for(r=(unsigned char*)result.prefix+size-1; r-(unsigned char*)result.prefix>=size_diff; s1--, s2--, r--) {
		is1=*s1;
		is2=*s2;
		tmp = is1 + is2 + overflow;
		ir = tmp%ALPHABET_SIZE;
		overflow = tmp/ALPHABET_SIZE;
		*r=(unsigned char)ir;
	}
	if (overflow) {
		for(;r>=(unsigned char*)result.prefix;r--,s1--) {
			is1=*s1;
			tmp = is1 + overflow;
			ir = tmp%ALPHABET_SIZE;
			overflow = tmp/ALPHABET_SIZE;
			*r=(unsigned char)ir;
		}
		if (overflow) {
			unsigned char* tmpc;
			tmpc = (unsigned char*)nid_alloc();
			tmpc[0]=overflow;
			memcpy(tmpc+1, result.prefix, size);
			nid_free(result.prefix);
			result.prefix=tmpc;
			result.size++;
		}
	}
	else {
		for(;r>=(unsigned char*)result.prefix;r--,s1--)
			*r = *s1;
	}
	return result;
}

/*	compute "the_big-the_small"
	assume numbers in ALPHABET_SIZE numeration
	assume "the_small <= the_big" (value)
 */
t_prefix lex_subtract(t_prefix the_big, t_prefix the_small) {
	unsigned char	*s1, *s2, *r;
	int				is1, is2, ir;
	t_prefix		result;
	int				carry=0;
	int				size;

	if (the_big.size != the_small.size)
		lex_allign(the_big, the_small);
	result.prefix = (unsigned char*)nid_alloc();
	size = the_small.size;
	result.size = size;
	for
	(s1=(unsigned char*)the_small.prefix+size-1, s2=(unsigned char*)the_big.prefix+size-1, r=(unsigned char*)result.prefix+size-1;
	 r>=(unsigned char*)result.prefix;
	 s1--, s2--, r--
    ) {
		is1=*s1;
		is2=*s2;
		if (is2 < is1+carry) {
			ir = is2 + ALPHABET_SIZE - is1 - carry;
			*r=(unsigned char)ir;
			carry = 1;
		} else {
			ir = is2 - is1 - carry;
			*r=(unsigned char)ir;
			carry = 0;
		}
	}
	return result;
}

/*
	lexicographic prefix comparison
 */
int	lex_cmp(t_prefix p1, t_prefix p2) {
	shft	i=0;
	while(i<p1.size && i<p2.size) {
		if (p1.prefix[i]^p2.prefix[i]) {
			if (p1.prefix[i] > p2.prefix[i]) return 1;
			else return -1;
		}
		i++;
	}
	if (p1.size > p2.size) return 1;
	else if (p2.size > p1.size)	return -1;
	return 0;
}

/*
	lexicographic prefix comparison
 */
bool lex_ispref(t_prefix p1, t_prefix p2) {
	int	i=p1.size-1;
	while(i>-1) 
	{
		if (p1.prefix[i]^p2.prefix[i]) 
			 return false;
		i--;
	}
	return true;
}

/*	
	add extra symbol to prefix tail ALPHABET_SIZE*p
 */
void lex_extend(t_prefix& pre, fnumber p) {
	int		tmp=ALPHABET_SIZE-2;
	
	/* prevent extension with the first symbol of the alphabet */
	if (!p.getx())
		tmp=1;
	else
		tmp = (tmp*p.getx())/p.gety();	/* 0 <= tmp < ALPHABET_SIZE-1 because 0<p<1 */
	/* extend with symbol no less than 2 */
	if (tmp <= 1) tmp = 2;
	pre.prefix[pre.size]=tmp;
	pre.size++;
}

/*	take next lexicographically bigger value than "p"
	incrementing last int. If result exceeds "b", extend
	"p" with additional int
	assume "p<b" (no check)
 */
t_prefix lex_next(t_prefix p, t_prefix b) {
	t_prefix	tmp1;
	t_prefix	result;
	int			rc;
	t_prefix	one;

	if (p.size != b.size)
		lex_allign(p, b);
	/* prepare "one" number */
	one.prefix = (unsigned char*)nid_alloc();
	for (int i=0; i<p.size-1; i++)	one.prefix[i]=0;
	one.prefix[p.size-1]=1;
	one.size = p.size;
	/* sum "p" and "one" puting sum into "result" */
	result = lex_sum(one, p);
	nid_free(one.prefix);
	if ((result.size != b.size) || (rc = lex_cmp(b, result)) <= 0) {
		/* extend prefix with new symbol with same proportion on new level */
		nid_free(result.prefix);
		lex_extend(p, fnumber(0,1));
		result = p;
	}
	return result;
}

/*	compute "the_small + (the_big - the_small)*p"
	assume prefixes in ALPHABET_SIZE numeration
	assume "the_small<the_big" (no check)
 */
t_prefix lex_between(t_prefix the_small, t_prefix the_big, fnumber p) {
	t_prefix	tmp1, tmp2;
	t_prefix	result;
	int			rc;

	if (the_small.size != the_big.size)
		lex_allign(the_small, the_big);
	tmp1 = lex_subtract(the_big, the_small);
//lex_print(tmp1);
	tmp2 = lex_multiply(tmp1, p.getx());
//lex_print(tmp2);
	//delete[] tmp1.head;
	nid_free(tmp1.prefix);
	tmp1 = lex_divide(tmp2, p.gety());
	//delete[] tmp2.prefix;
	nid_free(tmp2.prefix);
//lex_print(tmp1);
	result = lex_sum(the_small, tmp1);
	//delete[] tmp1.prefix;
	nid_free(tmp1.prefix);
	rc = lex_cmp(result, the_small);
	if (rc == 0) {
		/* extend prefix with new symbol with same proportion on new level */
		lex_extend(result, p);
	} else if (rc < 0) {
//		lex_print(the_small);
//		lex_print(the_big);
//		lex_print(result);
		throw SYSTEM_EXCEPTION("[lex_between()] bad operation result 2");
	}
	return result;
}

/*	compute "(the_small + the_big)/2"
	assume numbers in ALPHABET_SIZE numeration
	assume strings are of equal size
	assume "the_small<the_big" (no check)
	result size as of "the_small" if no extension happens
	result size 1 more than of "the_small" is extension happens
 */
t_prefix lex_middle(t_prefix the_small, t_prefix the_big) {
	t_prefix	tmp1;
	t_prefix	result;
	int			rc;

	if (the_small.size != the_big.size)
		lex_allign(the_small, the_big);
	tmp1 = lex_sum(the_small, the_big);
	result = lex_divide(tmp1, 2);
	nid_free(tmp1.prefix);
	rc = lex_cmp(result, the_small);
	if (rc == 0) {
		/* extend prefix with new symbol with same proportion on new level */
		lex_extend(result, fnumber(1,2));
	} else if (rc < 0) {
		throw SYSTEM_EXCEPTION("[lex_middle()] bad operation result 3");
	}
	return result;
}

/*
	makes two prefixes of equal size stuffing the tail of shotest with minimal symbol (1)
*/
void lex_allign(t_prefix &the_small, t_prefix &the_big) {
	/* here we use the fact that prefixes live in our memory, which is big enough */
	if (the_small.size < the_big.size) {
		for (int i=the_small.size; i<the_big.size; i++)
			the_small.prefix[i]=1;
		the_small.size = the_big.size;
	} else if (the_small.size > the_big.size) {
		for (int i=the_big.size; i<the_small.size; i++)
			the_big.prefix[i]=1;
		the_big.size = the_small.size;
	}
}

/*
	cut prefix up to the "size" from the left side
*/
void lex_cut_to_size(t_prefix& p, int size) {
	if (p.size <= size) return;
	unsigned char*	tmp = p.prefix;
	p.prefix = (unsigned char*)nid_alloc();
	memcpy(p.prefix, tmp+(p.size-size), size);
	p.size = size;
	nid_free(tmp);
}
