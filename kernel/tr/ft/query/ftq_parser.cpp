/*
 * File:  ft_parser.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/query/ftq_parser.h"
#include "tr/ft/query/ftq_lex_types.h"
#include "tr/ft/query/ftq_lexer.h"
#include "tr/strings/utf8.h"
#include "tr/ft/ft_norm.h"
#include "common/counted_ptr.h"

#include <vector>
#include <deque>
#include <stdlib.h>

class Token
{
public:
	ftq_token_type type;
	char *text;
	int leng;

	~Token()
	{
		delete[] text;
	}
};

typedef counted_ptr<Token> token_ptr;

class Scanner
{
private:
	yyscan_t yyscanner;
	std::deque<token_ptr> tokens;
public:
	Scanner() : tokens() {}

	void init(str_cursor_reader *reader)
	{
		ftq_lex_init_extra(reader, &this->yyscanner);
	}
	void destroy()
	{
		ftq_lex_destroy(this->yyscanner);
	}

	void get_next()
	{
		while (true)
		{
			Token *tok = new Token();

			tok->type = (ftq_token_type)ftq_lex(this->yyscanner);
			tok->leng = ftq_get_leng(this->yyscanner);
			tok->text = new char[tok->leng+1];
			memcpy(tok->text, ftq_get_text(this->yyscanner), tok->leng);
			tok->text[tok->leng] = '\x0';

			//ignore stuff that lexer failed to recognize
			if (tok->type != ftq_token::_ERROR_)
			{
				tokens.push_back(token_ptr(tok));
				return;
			}
		}
	}

	token_ptr peek()
	{
		if (tokens.size() < 1)
			get_next();
		return tokens.front();
	}
	token_ptr peekn(size_t ind)
	{
		while (tokens.size() < ind+1)
			get_next();
		return tokens[ind];
	}

	token_ptr next()
	{
		if (tokens.size() < 1)
			get_next();
		token_ptr tok = tokens.front();
		tokens.pop_front();
		return tok;
	}
};

struct ft_parser_state
{
	Scanner scanner;

	ftc_index_t ftc_idx;
	FtStemmer *stemmer;
};

FtQueryTermBase *create_term(ftc_index_t idx, const char *in_tag, bool stem)
{
	if (in_tag == NULL)
		return new FtQueryTerm(idx, stem);

	FtQueryTermInElement *q = new FtQueryTermInElement(idx, stem);

	int in_tag_len = strlen(in_tag);
	if (in_tag_len >= FT_MAX_WORD_LENGTH)
		in_tag_len = FT_MAX_WORD_LENGTH-1;

	memcpy(q->opentag_buf, in_tag, in_tag_len);
	q->opentag_buf[in_tag_len] = FT_TAG_OPEN_MARKER;
	q->opentag_buf[in_tag_len+1] = '\x0';

	memcpy(q->closetag_buf, in_tag, in_tag_len);
	q->closetag_buf[in_tag_len] = FT_TAG_CLOSE_MARKER;
	q->closetag_buf[in_tag_len+1] = '\x0';

	return q;
}


//parse a WORD token from lexer into a set FtQueryTerm-s and append them to a vector
template <typename T>
static void ft_parse_term(struct ft_parser_state *ps, token_ptr tok, std::vector<T*> *vec, const char *in_tag, bool stem, double boost)
{
	FtQueryTermBase *q = create_term(ps->ftc_idx, in_tag, stem);
	q->set_boost((ft_float)boost);
	const char *p = tok->text;
	int len = tok->leng, word_len = 0;
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
				//FIXME: move this to FtQueryTerm*
				if (ps->stemmer != NULL && (stem || ftc_get_fts_data(q->get_ftc_idx())->stem_type != ftst_both))
				{
					const char *stemmed_term = ps->stemmer->stem_word(q->term_buf, word_len);
					strncpy(q->term_buf, stemmed_term, FT_MAX_WORD_LENGTH);
					q->term_buf[FT_MAX_WORD_LENGTH] = '\x0';
				}
				vec->push_back(q);
				if (ch == UTF8_EOF)
					q = NULL;
				else
				{
					q = create_term(ps->ftc_idx, in_tag, stem);
					q->set_boost((ft_float)boost);
				}
				word_len = 0;
				overfl = false;
			}
		}
		if (ch == UTF8_EOF)
			break;
	}
	//may happen if token ends with non-word characters
	if (q != NULL)
		delete q;
}

FtQuery *ft_parse_phrase(struct ft_parser_state *ps, ftq_token_type start_tok, const char *in_tag, double boost)
{
	std::vector<FtQueryTermBase *> terms;
	while (true)
	{
		token_ptr tok = ps->scanner.next();

		//treat keywords as words inside phrase
		if (tok->type == ftq_token::WORD || tok->type == ftq_token::NUMBER || tok->type == ftq_token::CONTAINS || tok->type == ftq_token::OR)
		{
			if (ps->scanner.peek()->type == ftq_token::TILDE_MOD)
			{
				ft_parse_term(ps, tok, &terms, in_tag, true, boost);
				ps->scanner.next();
			}
			else if (ps->scanner.peek()->type == ftq_token::BOOST_MOD)
			{
				token_ptr b_tok = ps->scanner.next();
				U_ASSERT(b_tok->text[0] == ':');
				int mod = atoi(b_tok->text+1);

				ft_parse_term(ps, tok, &terms, in_tag, true, boost * mod);
			}
			else
				ft_parse_term(ps, tok, &terms, in_tag, false, boost);
		}

		//we allow pharase to end abruptly
		if (tok->type == start_tok || tok->type == ftq_token::END)
		{
			if (terms.size() == 0)
				return NULL;
			if (terms.size() == 1)
				return terms[0];

			FtQueryPhrase *q = new FtQueryPhrase(terms.size());
			for (size_t i = 0; i < terms.size(); i++)
				q->set_term(i, terms[i]);
			return q;
		}

		//other token types are ignored
	}
}

FtQuery* ft_parse_query_or(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok, double boost);
FtQuery* ft_parse_query_single(struct ft_parser_state *ps, char *in_tag, double boost)
{
	token_ptr tok = ps->scanner.peek();

	if (tok->type == ftq_token::QUOT || tok->type == ftq_token::APOS)
	{
		ps->scanner.next();
		FtQuery *q = ft_parse_phrase(ps, tok->type, in_tag, boost);
		return q;
	}

	if (tok->type == ftq_token::WORD || tok->type == ftq_token::NUMBER)
	{
		if (ps->scanner.peekn(1)->type == ftq_token::CONTAINS)
		{
			token_ptr tag = tok;
			ps->scanner.next(); //tag
			tok = ps->scanner.next(); //contains
			return ft_parse_query_single(ps, tag->text, boost);
		}
		else if (ps->scanner.peekn(2)->type == ftq_token::CONTAINS && ps->scanner.peekn(1)->type == ftq_token::BOOST_MOD)
		{
			token_ptr tag = tok;
			ps->scanner.next(); //tag
			token_ptr b_tok = ps->scanner.next();
			U_ASSERT(b_tok->text[0] == ':');
			int mod = atoi(b_tok->text+1);
			tok = ps->scanner.next(); //contains
			return ft_parse_query_single(ps, tag->text, boost * mod);
		}
		else
			return ft_parse_phrase(ps, tok->type, in_tag, boost);
	}

	if (tok->type == ftq_token::BR_OPEN)
	{
		ps->scanner.next();
		FtQuery *q = ft_parse_query_or(ps, in_tag, ftq_token::BR_CLOSE, boost);
		tok = ps->scanner.peek();
		if (tok->type == ftq_token::BR_CLOSE)
			ps->scanner.next();
		return q;
	}

	throw USER_EXCEPTION2(SE3022, "unexpected token in query");
}

FtQuery* ft_parse_query_and(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok, double boost)
{
	std::vector<FtQuery *> ops;
	token_ptr tok;

	while (true)
	{
		token_ptr tok = ps->scanner.peek();
		if (tok->type == ftq_token::END || tok->type == ftq_token::OR || tok->type == end_tok)
			break;

		FtQuery *q = ft_parse_query_single(ps, in_tag, boost);
		if (q != NULL)
			ops.push_back(q);
	}

	if (ops.size() == 0)
		return NULL;
	if (ops.size() == 1)
		return ops[0];
	else
	{
		FtQueryAnd *q = new FtQueryAnd(ps->ftc_idx, ops.size());
		for (size_t i = 0; i < ops.size(); i++)
			q->set_operand(i, ops[i]);
		return q;
	}
}

FtQuery* ft_parse_query_or(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok, double boost)
{
	std::vector<FtQuery *> ops;
	token_ptr tok;
	bool first = true;

	while (true)
	{
		token_ptr tok = ps->scanner.peek();
		if (tok->type == ftq_token::END || tok->type == end_tok)
			break;

		if (!first)
		{
			if (tok->type != ftq_token::OR)
				throw USER_EXCEPTION2(SE3022, "unexpected token");
			tok = ps->scanner.next();
		}
		first = false;

		FtQuery *q = ft_parse_query_and(ps, in_tag, end_tok, boost);
		if (q != NULL)
			ops.push_back(q);
	}

	if (ops.size() == 0)
		return NULL;
	if (ops.size() == 1)
		return ops[0];
	else
	{
		FtQueryOr *q = new FtQueryOr(ps->ftc_idx, ops.size());
		for (size_t i = 0; i < ops.size(); i++)
			q->set_operand(i, ops[i]);
		return q;
	}
}


FtQuery *ft_parse_query(str_cursor *cur, ftc_index_t idx)
{
	ft_parser_state ps;
	str_cursor_reader reader(cur);
	ps.ftc_idx = idx;
	ps.stemmer = ftc_get_stemmer(idx);

	ps.scanner.init(&reader);

	FtQuery *q = ft_parse_query_or(&ps, NULL, ftq_token::END, 1);

	ps.scanner.destroy();

	return q;
}
