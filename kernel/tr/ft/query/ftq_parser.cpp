/*
 * File:  ft_parser.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/query/ftq_parser.h"
#include "tr/ft/query/ftq_lex_types.h"
#include "tr/ft/query/ftq_lexer.h"
#include "tr/strings/utf8.h"
#include "tr/ft/ft_norm.h"

#include <vector>

FtQuery *ft_parse_query(str_cursor *cur, ftc_index_t idx)
{
	yyscan_t scanner;
	str_cursor_reader reader(cur);
	std::vector<FtQuery *> ops;
	FtStemmer *stemmer = ftc_get_stemmer(idx);
	ftq_token_type tok;

	ftq_lex_init_extra(&reader, &scanner);

	while ( (tok = (ftq_token_type)ftq_lex(scanner)) != ftq_token::END)
	{
		char *tok_text = ftq_get_text(scanner);
		int tok_leng = ftq_get_leng(scanner);

		if (tok != ftq_token::WORD)
			throw USER_EXCEPTION2(SE3022, "unexpected token in query");

		FtQueryTerm *q = new FtQueryTerm(idx);
		const char *p = tok_text;
		int len = tok_leng, word_len = 0;
		bool overfl = false;
		int ch;
		while (true)
		{
			ch = utf8_getch(&p, &len);
			if (ch != UTF8_EOF && ft_norm_char(&ch))
			{
				if (!overfl)
					overfl = !CharsetHandler_utf8::utf8_putch(ch, q->term_buf, &word_len, FT_MAX_WORD_LENGTH);
			}
			else
			{
				if (word_len > 0)
				{
					U_ASSERT(word_len <= FT_MAX_WORD_LENGTH);
					q->term_buf[word_len] = 0;
					if (stemmer != NULL)
					{
						const char *stemmed_term = stemmer->stem_word(q->term_buf, word_len);
						strncpy(q->term_buf, stemmed_term, FT_MAX_WORD_LENGTH);
						q->term_buf[FT_MAX_WORD_LENGTH] = '\x0';
					}
					ops.push_back(q);
					if (ch == UTF8_EOF)
						q = NULL;
					else
						q = new FtQueryTerm(idx);
				}
			}
			if (ch == UTF8_EOF)
				break;
		}
		//may happen if token ends with non-word characters
		if (q != NULL)
			delete q;
	}

	ftq_lex_destroy(scanner);

	if (ops.size() == 0)
		throw USER_EXCEPTION2(SE3022, "empty query");
	if (ops.size() == 1)
		return ops[0];
	else
	{
		FtQueryAnd *q = new FtQueryAnd(idx, ops.size());
		for (int i = 0; i < ops.size(); i++)
			q->set_operand(i, ops[i]);
		return q;
	}
}