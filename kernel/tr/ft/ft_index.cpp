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
#include <wctype.h>

#include <map>
//TODO: store tags separately, with 2 indexes, consider to store all tags (may be needed for nested contains)
//FIXME: get rid of stl here

//for expat
#define SEPARATOR '>'

struct tag_info
{
	int d;
	int start_ind;
};
class ft_parse_data
{
public:
	char word_buf[FT_MAX_WORD_LENGTH+1]; //+1 is because ftc_add_word wants a null terminated string
	int word_len;
	int word_ind;
	int word_count;
	bool overfl; //next char of current word could not fit into word_buf
	ftc_index_t cur_idx;
	ftc_doc_t cur_doc;
	ft_index_op_t op;
	FtStemmer *stemmer;
	FtsData *ft_data;
	std::map<std::string,tag_info> tagmap;

	ft_parse_data() {}
};

static void process_word(ft_parse_data *parse_data)
{
	if (parse_data->word_len > 0)
	{
		const char *word = parse_data->word_buf;
		parse_data->word_buf[parse_data->word_len] = 0;

		if (parse_data->stemmer != NULL)
		{
			word = parse_data->stemmer->stem_word(word, parse_data->word_len);
			if (parse_data->ft_data->stem_type == ftst_both)
			{
				int l = parse_data->word_len;
				if (l >= FT_MAX_WORD_LENGTH)
					l = FT_MAX_WORD_LENGTH-1;
				parse_data->word_buf[l] = FT_NOSTEM_MARKER;
				parse_data->word_buf[l+1] = '\x0';
				ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, parse_data->word_ind);
			}
		}

		ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, word, parse_data->word_ind);

		parse_data->word_len = 0;
		parse_data->overfl = false;
		parse_data->word_ind++;
		parse_data->word_count++;
	}
}

static void p_start(void *state, const char *el, const char **attr)
{
	ft_parse_data *parse_data = (ft_parse_data *)state;
	//tags should break words
	process_word(parse_data);

	std::map<std::string,tag_info>::iterator it = parse_data->tagmap.find(el);
	if (it == parse_data->tagmap.end())
	{
		tag_info ti;
		ti.start_ind = parse_data->word_ind;
		ti.d = 1;
		parse_data->tagmap.insert(std::pair<std::string, tag_info>(el, ti));
	}
	else
	{
		it->second.d++;
	}
}

static void p_end(void *state, const char *el)
{
	ft_parse_data *parse_data = (ft_parse_data *)state;
	//tags should break words
	process_word(parse_data);

	std::map<std::string,tag_info>::iterator it = parse_data->tagmap.find(el);

	if (it->second.d > 1)
		it->second.d--;
	else
	{
		int start_ind = it->second.start_ind;
		parse_data->tagmap.erase(it);

		U_ASSERT(parse_data->word_len == 0);
		U_ASSERT(parse_data->word_ind >= start_ind);

		if (parse_data->word_ind > start_ind)
		{
			size_t len = strlen(el);
			if (len > FT_MAX_WORD_LENGTH-1)
				len = FT_MAX_WORD_LENGTH-1;
			memcpy(parse_data->word_buf, el, len);
			parse_data->word_buf[len] = FT_TAG_OPEN_MARKER;
			parse_data->word_buf[len+1] = '\x0';
			ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, start_ind);

			if (len > FT_MAX_WORD_LENGTH-1)
				len = FT_MAX_WORD_LENGTH-1;
			memcpy(parse_data->word_buf, el, len);
			parse_data->word_buf[len] = FT_TAG_CLOSE_MARKER;
			parse_data->word_buf[len+1] = '\x0';
			ftc_upd_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, parse_data->word_ind);
		}
	}
}

//assumes that s contains full characters (i.e. multibyte chars are not splited between calls)
static void p_data(void *state, const char *s, int len)
{
	ft_parse_data *parse_data = (ft_parse_data *)state;
	int ch;
	while ((ch=utf8_getch(&s, &len)) != UTF8_EOF)
	{
		if (ft_norm_char(&ch))
		{
			if (ch != -1 && !parse_data->overfl)
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
	ft_parse_data *parse_data = (ft_parse_data *)state;

	//it's actually impossible now, because we always index one node, so data should end with a close tag
	process_word(parse_data);
}


void ft_index_update(ft_index_op_t op, xptr acc, op_str_buf *text_buf, ftc_index_t ftc_idx)
{
	U_ASSERT(op == ft_insert || op == ft_delete);
	//TODO: reuse parser&parse_data for parsing multiple documents (or don't use expat and reuse parse_data)
	//XML_Parser p = XML_ParserCreateNS(NULL, SEPARATOR);
	XML_Parser p = XML_ParserCreate(NULL);
	//FIXME: check exception & rollback
	if (!p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);

	ft_parse_data *parse_data = new ft_parse_data();
	parse_data->word_len = 0;
	parse_data->word_ind = 0;
	parse_data->word_count = 0;
	parse_data->overfl = false;
	parse_data->cur_idx = ftc_idx;
	U_ASSERT(op == ft_insert);
	parse_data->cur_doc = ftc_get_doc(ftc_idx, acc);
	parse_data->op = op;
	parse_data->stemmer = ftc_get_stemmer(ftc_idx);
	parse_data->ft_data = ftc_get_fts_data(ftc_idx);

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

	delete parse_data;
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

//FIXME: highlighting code is currently duplicated here and in FTsearch.cpp
#define HL_BUF 256
struct ft_parse_data_hl
{
	ft_word_ind_t *inds;
	size_t inds_count;
	bool hl_fragment;
	stmt_str_buf *out_str;
	char out_buf[HL_BUF];
	int out_buf_cnt;

	size_t cur_ind;
	int word_ind;

	int ht0;
	int last_eff_ch;
	bool hit;
	bool in_word;
	int word_len; //if !in_word - value is undefined
	bool done;
	bool in_fragment;
};

static const char *opentag_str = "\xEE\xA0\x81";
static const char *closetag_str = "\xEE\xA0\x82";
static const char *quotinattr_str = "\xEE\xA0\x83";

static void hl_flush(struct ft_parse_data_hl *parse_data)
{
	if (parse_data->out_buf_cnt > 0)
	{
		parse_data->out_str->append(parse_data->out_buf, parse_data->out_buf_cnt);
		parse_data->out_buf_cnt = 0;
	}
}
static void hl_clear(struct ft_parse_data_hl *parse_data)
{
	parse_data->out_buf_cnt = 0;
	parse_data->out_str->clear();
}
static void hl_puts(struct ft_parse_data_hl *parse_data, const char *s)
{
	hl_flush(parse_data);
	parse_data->out_str->append(s);
}
static void hl_put_attr_value(struct ft_parse_data_hl *parse_data, const char *s, const char *quot_in_attr)
{
	hl_flush(parse_data);
	const char *p = strchr(s, '"');
	while (p != NULL)
	{
		if (p > s)
			parse_data->out_str->append(s, p-s);
		parse_data->out_str->append(quot_in_attr);
		s = p + 1;
		p = strchr(s, '"');
	}
	if (*s != '\x0')
		parse_data->out_str->append(s);
}
static void hl_putch(struct ft_parse_data_hl *parse_data, int ch)
{
	if (parse_data->out_buf_cnt >= HL_BUF-10) //FIXME
		hl_flush(parse_data);

	bool no_overfl = CharsetHandler_utf8::utf8_putch(ch, parse_data->out_buf, &parse_data->out_buf_cnt, HL_BUF);
	U_ASSERT(no_overfl);
}

static void hl_word_end(struct ft_parse_data_hl *parse_data)
{
	if (parse_data->in_word && parse_data->hit && parse_data->word_len == 0)
		return; //FIXME: now we include words that only consist of ignored word chars with the following hit
	if (parse_data->hit && (!parse_data->hl_fragment || parse_data->in_fragment))
	{
		hl_puts(parse_data, opentag_str);
		hl_puts(parse_data, "/hit");
		hl_puts(parse_data, closetag_str);
	}
	parse_data->hit = false;
	if (parse_data->in_word)
	{
		if (parse_data->word_len > 0)
			parse_data->word_ind++;
		parse_data->in_word = false;
	}
}

static void hl_start(void *state, const char *el, const char **attr)
{
	struct ft_parse_data_hl *parse_data = (struct ft_parse_data_hl *)state;
	hl_word_end(parse_data);
	if (!parse_data->hl_fragment)
	{
		hl_puts(parse_data, opentag_str);
		hl_puts(parse_data, el);
		while (*attr != NULL)
		{
			hl_puts(parse_data, " ");
			hl_puts(parse_data, *attr);
			attr++;
			hl_puts(parse_data, "=\"");
			hl_put_attr_value(parse_data, *attr, quotinattr_str);
			hl_puts(parse_data, "\"");
			attr++;
		}

		hl_puts(parse_data, closetag_str);
	}
}

static void hl_end(void *state, const char *el)
{
	struct ft_parse_data_hl *parse_data = (struct ft_parse_data_hl *)state;
	hl_word_end(parse_data);
	if (!parse_data->hl_fragment)
	{
		hl_puts(parse_data, opentag_str);
		hl_puts(parse_data, "/");
		hl_puts(parse_data, el);
		hl_puts(parse_data, closetag_str);
	}
}

static const int hl_min_words_before = 10;
static const int hl_max_words_before = 20;
static const int hl_min_words_after = 10;
static const int hl_max_words_after = 20;

static void hl_fragment_handle_word_start(struct ft_parse_data_hl *parse_data, int ch)
{
	if ((parse_data->last_eff_ch == '.' && iswupper(ch)) || parse_data->last_eff_ch == 0)
	{ //sentence start
		if (parse_data->word_ind >= parse_data->ht0 - hl_max_words_before &&
			(parse_data->word_ind <= parse_data->ht0 - hl_min_words_before || (parse_data->word_ind <= parse_data->ht0 && !parse_data->in_fragment)))
		{
			hl_clear(parse_data);
			parse_data->in_fragment = true;
		}
		else
		{
			if (parse_data->word_ind >= parse_data->ht0 + hl_min_words_after)
			{
				//cut here
				if (parse_data->in_fragment)
				{
					parse_data->in_fragment = false;
					parse_data->done = true;
				}
			}
		}
	}
	else
	{//not sentence start
		if (parse_data->word_ind <= parse_data->ht0)
		{
			if (parse_data->word_ind >= parse_data->ht0 - hl_max_words_before && !parse_data->in_fragment)
			{
				//no need to call hl_clear - output must be clear here anyway
				parse_data->in_fragment = true;
				hl_puts(parse_data, "...");
			}
		}
		else
		{
			if (parse_data->word_ind >= parse_data->ht0 + hl_max_words_after)
			{
				//cut here
				if (parse_data->in_fragment)
				{
					parse_data->in_fragment = false;
					parse_data->done = true;
					hl_puts(parse_data, "...");
				}
			}
		}
	}
}

//assumes that s contains full characters (i.e. multibyte chars are not splited between calls)
static void hl_data(void *state, const char *s, int len)
{
	struct ft_parse_data_hl *parse_data = (struct ft_parse_data_hl *)state;

	int ch, ch_orig;
	while ((ch=utf8_getch(&s, &len)) != UTF8_EOF)
	{
		ch_orig = ch; //FIXME: no need to change case here
		if (ft_norm_char(&ch))
		{
			if (!parse_data->in_word)
			{
				//word start
				if (parse_data->hl_fragment)
					hl_fragment_handle_word_start(parse_data, ch_orig);
				if (parse_data->cur_ind < parse_data->inds_count && parse_data->word_ind == parse_data->inds[parse_data->cur_ind])
				{
					if (!parse_data->hl_fragment || parse_data->in_fragment)
					{
						hl_puts(parse_data, opentag_str);
						hl_puts(parse_data, "hit");
						hl_puts(parse_data, closetag_str);
					}
					parse_data->hit = true;
					parse_data->cur_ind++;
				}
				parse_data->in_word = true;
				parse_data->word_len = 0;
			}
			if (ch != -1)
				parse_data->word_len++;
			if (!parse_data->hl_fragment || parse_data->in_fragment)
				hl_putch(parse_data, ch_orig);
		}
		else
		{
			hl_word_end(parse_data);
			if (!parse_data->hl_fragment || parse_data->in_fragment)
				hl_putch(parse_data, ch_orig);
		}
		if (!iswspace(ch_orig))
			parse_data->last_eff_ch = ch_orig;
	}
}

void ft_highlight(op_str_buf *in_buf, stmt_str_buf *out_buf, ft_word_ind_t *inds, size_t inds_count, bool hl_fragment)
{
	struct ft_parse_data_hl *parse_data = (struct ft_parse_data_hl *)malloc(sizeof(struct ft_parse_data_hl));
	parse_data->inds = inds;
	parse_data->inds_count = inds_count;
	parse_data->hl_fragment = hl_fragment;
	parse_data->out_str = out_buf;
	parse_data->ht0 = -1;

	XML_Parser p = XML_ParserCreate(NULL);
	if (!p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);

	XML_SetUserData(p, parse_data);
	XML_SetReturnNSTriplet(p,XML_TRUE);
	XML_SetElementHandler(p, hl_start, hl_end);
	XML_SetCharacterDataHandler(p, hl_data);

	str_cursor *cur = in_buf->get_cursor();

	char buf[PAGE_SIZE]; //FIXME?
	int len;

	if (parse_data->hl_fragment)
	{
		if (parse_data->inds_count == 0)
			parse_data->ht0 = 0;
		else
			parse_data->ht0 = parse_data->inds[0];
	}

	parse_data->out_buf_cnt = 0;

	parse_data->cur_ind = 0;
	parse_data->word_ind = 0;
	parse_data->done = false;
	parse_data->in_word = false;
	parse_data->hit = false;
	parse_data->in_fragment = false;
	parse_data->last_eff_ch = 0;

	do
	{
		if (parse_data->done)
			break;
		len = cur->copy_blk(buf);
		if (XML_Parse(p, buf, len, len > 0 ? XML_FALSE : XML_TRUE) == XML_STATUS_ERROR)
		{
			char tmp[256];
			//FIXME!!!!: buf, message, cleanup
			sprintf(tmp, "in ft_highlight\nline %d:\n%s\n",
				XML_GetCurrentLineNumber(p),
				XML_ErrorString(XML_GetErrorCode(p)));
			in_buf->free_cursor(cur);
			free(parse_data);
			XML_ParserFree(p);
			throw USER_EXCEPTION2(SE2005, tmp);
		}
	} while (len > 0);
	in_buf->free_cursor(cur);

	XML_ParserFree(p);

	U_ASSERT(!parse_data->hl_fragment || parse_data->ht0 != -1);

	hl_flush(parse_data);
	free(parse_data);
}
