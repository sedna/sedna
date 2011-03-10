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
	token_ptr peekn(int ind)
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

FtQueryTermBase *create_term(ftc_index_t idx, const char *in_tag)
{
	if (in_tag == NULL)
		return new FtQueryTerm(idx);

	FtQueryTermInElement *q = new FtQueryTermInElement(idx);

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
static void ft_parse_term(struct ft_parser_state *ps, token_ptr tok, std::vector<T*> *vec, const char *in_tag)
{
	FtQueryTermBase *q = create_term(ps->ftc_idx, in_tag);
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
					q = create_term(ps->ftc_idx, in_tag);
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

FtQuery *ft_parse_phrase(struct ft_parser_state *ps, ftq_token_type start_tok, const char *in_tag)
{
	std::vector<FtQueryTermBase *> terms;
	while (true)
	{
		token_ptr tok = ps->scanner.next();


		if (tok->type == ftq_token::WORD || tok->type == ftq_token::NUMBER)
			ft_parse_term(ps, tok, &terms, in_tag);

		//we allow pharase to end abruptly
		if (tok->type == start_tok || tok->type == ftq_token::END)
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
		if (tok->type != ftq_token::WORD && tok->type != ftq_token::NUMBER)
			throw USER_EXCEPTION2(SE3022, "unexpected token in query");
	}
}

FtQuery* ft_parse_query_or(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok);
FtQuery* ft_parse_query_single(struct ft_parser_state *ps, char *in_tag)
{
	token_ptr tok = ps->scanner.peek();

	if (tok->type == ftq_token::QUOT || tok->type == ftq_token::APOS)
	{
		ps->scanner.next();
		FtQuery *q = ft_parse_phrase(ps, tok->type, in_tag);
		return q;
	}

	if (tok->type == ftq_token::WORD || tok->type == ftq_token::NUMBER)
	{
		if (ps->scanner.peekn(1)->type == ftq_token::CONTAINS)
		{
			token_ptr tag = tok;
			ps->scanner.next(); //tag
			tok = ps->scanner.next(); //contains
			return ft_parse_query_single(ps, tag->text);
		}
		else
			return ft_parse_phrase(ps, tok->type, in_tag);
	}

	if (tok->type == ftq_token::BR_OPEN)
	{
		ps->scanner.next();
		FtQuery *q = ft_parse_query_or(ps, in_tag, ftq_token::BR_CLOSE);
		tok = ps->scanner.peek();
		if (tok->type == ftq_token::BR_CLOSE)
			ps->scanner.next();
		return q;
	}

	throw USER_EXCEPTION2(SE3022, "unexpected token in query");
}

FtQuery* ft_parse_query_and(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok)
{
	std::vector<FtQuery *> ops;
	token_ptr tok;

	while (true)
	{
		token_ptr tok = ps->scanner.peek();
		if (tok->type == ftq_token::END || tok->type == ftq_token::OR || tok->type == end_tok)
			break;

		FtQuery *q = ft_parse_query_single(ps, in_tag);
		if (q != NULL)
			ops.push_back(q);
	}

	if (ops.size() == 0)
		throw USER_EXCEPTION2(SE3022, "empty query");
	if (ops.size() == 1)
		return ops[0];
	else
	{
		FtQueryAnd *q = new FtQueryAnd(ps->ftc_idx, ops.size());
		for (int i = 0; i < ops.size(); i++)
			q->set_operand(i, ops[i]);
		return q;
	}
}

FtQuery* ft_parse_query_or(struct ft_parser_state *ps, char *in_tag, ftq_token_type end_tok)
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

		FtQuery *q = ft_parse_query_and(ps, in_tag, end_tok);
		if (q != NULL)
			ops.push_back(q);
	}

	if (ops.size() == 0)
		throw USER_EXCEPTION2(SE3022, "empty query");
	if (ops.size() == 1)
		return ops[0];
	else
	{
		FtQueryOr *q = new FtQueryOr(ps->ftc_idx, ops.size());
		for (int i = 0; i < ops.size(); i++)
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

	FtQuery *q = ft_parse_query_or(&ps, NULL, ftq_token::END);

	ps.scanner.destroy();

	return q;
}
