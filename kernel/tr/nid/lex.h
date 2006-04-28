/*
 * File:  lex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LEX_H
#define _LEX_H

#include "sedna.h"
#include "crmutils.h"
#include "nid.h"

/**********************************************************************
	Functions for lexicographic arithmetics
 **********************************************************************/
void		lex_print(t_prefix p);
bool lex_ispref(t_prefix p1, t_prefix p2);
t_prefix	lex_multiply(t_prefix p, int factor);
t_prefix	lex_divide(t_prefix p, int divisor);
t_prefix	lex_sum(t_prefix p1, t_prefix p2);
t_prefix	lex_subtract(t_prefix the_big, t_prefix the_small);
void		lex_cut_to_size(t_prefix& p, int size);
int			lex_cmp(t_prefix p1, t_prefix p2);
void		lex_extend(t_prefix& pre, fnumber p);
t_prefix	lex_next(t_prefix p, t_prefix b);
t_prefix	lex_between(t_prefix the_small, t_prefix the_big, fnumber p);
t_prefix	lex_middle(t_prefix the_small, t_prefix the_big);
void		lex_allign(t_prefix &the_small, t_prefix &the_big);

#endif