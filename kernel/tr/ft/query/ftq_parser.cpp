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

struct ft_parser_state
{
	yyscan_t scanner;
	ftc_index_t ftc_idx;
	FtStemmer *stemmer;
};

//parse a WORD token from lexer into a set FtQueryTerm-s and append them to a vector
template <typename T>
static void ft_parse_term(struct ft_parser_state *ps, const char *tok_text, int tok_leng, std::vector<T*> *vec)
{
	FtQueryTerm *q = new FtQueryTerm(ps->ftc_idx);
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
				if (ps->stemmer != NULL)
				{
					const char *stemmed_term = ps->stemmer->stem_word(q->term_buf, word_len);
					strncpy(q->term_buf, stemmed_term, FT_MAX_WORD_LENGTH);
					q->term_buf[FT_MAX_WORD_LENGTH] = '\x0';
				}
				vec->push_back(q);
				if (ch == UTF8_EOF)
					q = NULL;
				else
					q = new FtQueryTerm(ps->ftc_idx);
			}
		}
		if (ch == UTF8_EOF)
			break;
	}
	//may happen if token ends with non-word characters
	if (q != NULL)
		delete q;
}

FtQuery *ft_parse_phrase(struct ft_parser_state *ps, ftq_token_type start_tok)
{
	std::vector<FtQueryTerm *> terms;
	while (true)
	{
		ftq_token_type tok = (ftq_token_type)ftq_lex(ps->scanner);
		char *tok_text = ftq_get_text(ps->scanner);
		int tok_leng = ftq_get_leng(ps->scanner);

		//we allow pharase to end abruptly
		if (tok == start_tok || tok == ftq_token::END)
		{
			if (terms.size() == 0)
				return NULL;
			if (terms.size() == 1)
				return terms[0];

			FtQueryPhrase *q = new FtQueryPhrase(terms.size());
			for (int i = 0; i < terms.size(); i++)
				q->set_term(i, terms[i]);
			return q;
		}
		if (tok != ftq_token::WORD)
			throw USER_EXCEPTION2(SE3022, "unexpected token in query");

		ft_parse_term(ps, tok_text, tok_leng, &terms);
	}
}

FtQuery *ft_parse_query(str_cursor *cur, ftc_index_t idx)
{
	ft_parser_state ps;
	str_cursor_reader reader(cur);
	std::vector<FtQuery *> ops;
	ps.ftc_idx = idx;
	ps.stemmer = ftc_get_stemmer(idx);
	ftq_token_type tok;

	ftq_lex_init_extra(&reader, &ps.scanner);

	while ( (tok = (ftq_token_type)ftq_lex(ps.scanner)) != ftq_token::END)
	{
		char *tok_text = ftq_get_text(ps.scanner);
		int tok_leng = ftq_get_leng(ps.scanner);

		if (tok == ftq_token::QUOT || tok == ftq_token::APOS)
		{
			FtQuery *q = ft_parse_phrase(&ps, tok);
			if (q != NULL)
				ops.push_back(q);
			continue;
		}

		if (tok != ftq_token::WORD)
			throw USER_EXCEPTION2(SE3022, "unexpected token in query");

		ft_parse_term(&ps, tok_text, tok_leng, &ops);
	}

	ftq_lex_destroy(ps.scanner);

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
