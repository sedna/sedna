/*
 * File:  ft_index.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"
#include "tr/ft/ft_index.h"
#include "tr/btree/btree.h"
#include "expat.h"
#include "tr/ft/ft_cache.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/tr_globals.h"
#include "tr/ft/ft_norm.h"
#include "tr/strings/utf8.h"

//for expat
#define SEPARATOR '>'

struct ft_parse_data
{
	char word_buf[FT_MAX_WORD_LENGTH+1]; //+1 is because ftc_add_word wants a null terminated string
	int word_len;
	int word_ind;
	int word_count;
	bool overfl; //next char of current word could not fit into word_buf
	ftc_index_t cur_idx;
	ftc_doc_t cur_doc;
	ft_index_op_t op;
	FtStemmer *stemmer;
};

static void process_word(struct ft_parse_data *parse_data)
{
	if (parse_data->word_len > 0)
	{
		const char *word = parse_data->word_buf;
		parse_data->word_buf[parse_data->word_len] = 0;

		if (parse_data->stemmer != NULL)
			word = parse_data->stemmer->stem_word(word, parse_data->word_len);

		ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, word, parse_data->word_ind);

		parse_data->word_len = 0;
		parse_data->overfl = false;
		parse_data->word_ind++;
		parse_data->word_count++;
	}
}

static void p_start(void *state, const char *el, const char **attr)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;
	//tags should break words
	process_word(parse_data);

	U_ASSERT(parse_data->word_len == 0);
	size_t len = strlen(el);
	if (len > FT_MAX_WORD_LENGTH-1)
		len = FT_MAX_WORD_LENGTH-1;
	memcpy(parse_data->word_buf, el, len);
	parse_data->word_buf[len] = FT_TAG_OPEN_MARKER;
	parse_data->word_buf[len+1] = '\x0';
	ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, parse_data->word_ind);
}

static void p_end(void *state, const char *el)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;
	//tags should break words
	process_word(parse_data);

	U_ASSERT(parse_data->word_len == 0);
	size_t len = strlen(el);
	if (len > FT_MAX_WORD_LENGTH-1)
		len = FT_MAX_WORD_LENGTH-1;
	memcpy(parse_data->word_buf, el, len);
	parse_data->word_buf[len] = FT_TAG_CLOSE_MARKER;
	parse_data->word_buf[len+1] = '\x0';
	ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, parse_data->word_ind);
}

//assumes that s contains full characters (i.e. multibyte chars are not splited between calls)
static void p_data(void *state, const char *s, int len)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;
	int ch;
	while ((ch=utf8_getch(&s, &len)) != UTF8_EOF)
	{
		if (ft_norm_char(&ch))
		{
			if (!parse_data->overfl)
				parse_data->overfl = !CharsetHandler_utf8::utf8_putch(ch, parse_data->word_buf, &parse_data->word_len, FT_MAX_WORD_LENGTH);
		}
		else
		{
			process_word(parse_data);
		}
	}
}

static void p_finish(void *state)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;

	//it's actually impossible now, because we always index one node, so data should end with a close tag
	process_word(parse_data);
}


void ft_index_update(ft_index_op_t op, xptr acc, op_str_buf *text_buf, struct FtsData *ft_data, ftc_index_t ftc_idx)
{
	U_ASSERT(op == ft_insert || op == ft_delete);
	//TODO: reuse parser&parse_data for parsing multiple documents (or don't use expat and reuse parse_data)
	//XML_Parser p = XML_ParserCreateNS(NULL, SEPARATOR);
	XML_Parser p = XML_ParserCreate(NULL);
	//FIXME: check exception & rollback
	if (!p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);

	struct ft_parse_data *parse_data = (struct ft_parse_data *)malloc(sizeof(struct ft_parse_data));
	parse_data->word_len = 0;
	parse_data->word_ind = 0;
	parse_data->word_count = 0;
	parse_data->overfl = false;
	parse_data->cur_idx = ftc_idx;
	U_ASSERT(op == ft_insert);
	parse_data->cur_doc = ftc_get_doc(ftc_idx, acc);
	parse_data->op = op;
	parse_data->stemmer = ftc_get_stemmer(ftc_idx);

	XML_SetUserData(p, parse_data);
	XML_SetReturnNSTriplet(p,XML_TRUE);
	XML_SetElementHandler(p, p_start, p_end);
	XML_SetCharacterDataHandler(p, p_data);

	str_cursor *cur = text_buf->get_cursor();

	char buf[PAGE_SIZE]; //FIXME?
	int len;

	do
	{
		len = cur->copy_blk(buf);
		if (XML_Parse(p, buf, len, len > 0 ? XML_FALSE : XML_TRUE) == XML_STATUS_ERROR)
		{
			char tmp[256];
			//FIXME!!!!: buf, message, cleanup
			sprintf(tmp, "in ftsearch\nline %d:\n%s\n",
				XML_GetCurrentLineNumber(p),
				XML_ErrorString(XML_GetErrorCode(p)));
			text_buf->free_cursor(cur);
			free(parse_data);
			XML_ParserFree(p);
			throw USER_EXCEPTION2(SE2005, tmp);
		}
	} while (len > 0);
	text_buf->free_cursor(cur);

	ftc_finalize_doc(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_count);

	free(parse_data);
	XML_ParserFree(p);
}

void ft_index_delete_doc(ftc_index_t ftc_idx, xptr acc)
{
	ftc_del_doc(ftc_idx, acc);
}

void ft_idx_delete(struct FtsData *ft_data)
{
	for (int i = 0; i < ft_data->npartitions; i++)
		ft_delete_partition(&ft_data->partitions[i]);
	//XXX: move to ftstorage
	if (ft_data->doc_stats != XNULL)
		bt_drop(ft_data->doc_stats);
}

