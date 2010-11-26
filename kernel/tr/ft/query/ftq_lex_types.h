/*
 * File:  ftq_lex_types.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FTQ_LEX_TYPES_H
#define _FTQ_LEX_TYPES_H

struct ftq_token
{
	enum token_type
	{
		END = 0,
		WORD = 258,
		_ERROR_,
	};
};

typedef ftq_token::token_type ftq_token_type;

#endif
