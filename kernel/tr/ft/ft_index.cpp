/*
 * File:  ft_index.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"
#include "tr/ft/ft_index.h"
#include "tr/idx/btree/btree.h"
#include "tr/crmutils/node_utils.h"
#include "tr/crmutils/crmutils.h"
#include "expat.h"
#include "tr/ft/ft_cache.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/tr_globals.h"
#include "tr/strings/utf8.h"


//TODO: remove this 
//neeeded for pcre includes
#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre/pcre.h"


//for expat
#define SEPARATOR '>'

//this length is in bytes, not characters
//words with length more than this are truncated
#define MAX_WORD_LENGTH 150

enum ft_index_op_t
{
	ft_insert
};

struct ft_parse_data
{
	char word_buf[MAX_WORD_LENGTH+1]; //+1 is because ftc_add_word wants a null terminated string
	int word_len;
	int word_ind;
	bool overfl; //next char of current word could not fit into word_buf
	ftc_index_t cur_idx;
	ftc_doc_t cur_doc;
};


//TODO!!!: move these funcs & defines to strings

#define UTF8_EOF -1

//read char from buffer *buf, *buf and *len are changed accordingly
//does not work if buf contains incomplete chars
static inline int utf8_getch(const char **buf, int *len)
{
	unsigned char ch = *(unsigned char *)*buf;
	int r;

	if (*len < 1)
		return UTF8_EOF;

	//FIXME - check len

	++(*buf);
	if (ch < 128)
	{
			r = ch;
			*len -= 1;
	}
	else if (ch < 224) //FIXME: ch mustbe >= 192
	{
		r = ch - 192; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 2;
	}
	else if (ch < 240)
	{
		r = ch - 224; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 3;
	}
	else if (ch < 248)
	{
		r = ch - 240; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 4;
	}
	else if (ch < 252)
	{
		r = ch - 248; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 5;
	}
	else // ch mustbe < 254
	{
		r = ch - 252; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 6;
	}
	return r;
}

static void p_start(void *state, const char *el, const char **attr)
{
	//TODO
}

static void p_end(void *state, const char *el)
{
	//TODO
}

//assumes that s contains full characters (i.e. multibyte chars are not splited between calls)
static void p_data(void *state, const char *s, int len)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;
	int ch;
	while ((ch=utf8_getch(&s, &len)) != UTF8_EOF)
	{
		int c_cl, c_type, c_case;
		c_cl = ucp_findchar(ch, &c_type, &c_case);

		if (c_cl == ucp_L || c_type == ucp_N)
		{
			if (!parse_data->overfl)
				parse_data->overfl = !CharsetHandler_utf8::utf8_putch(ch, parse_data->word_buf, &parse_data->word_len, MAX_WORD_LENGTH);
		}
		else
		{
			if (parse_data->word_len > 0)
			{
				parse_data->word_buf[parse_data->word_len] = 0;
				ftc_add_word(parse_data->cur_idx, parse_data->cur_doc, parse_data->word_buf, parse_data->word_ind);

				parse_data->word_len = 0;
				parse_data->overfl = false;
				parse_data->word_ind++;
			}
		}
	}
}

static void p_finish(void *state)
{
	struct ft_parse_data *parse_data = (struct ft_parse_data *)state;

	//TODO: add last word
}


static void ft_index_update(ft_index_op_t op, xptr acc, op_str_buf *text_buf, ft_idx_data_t *ft_data, ftc_index_t ftc_idx)
{
	//TODO: reuse parser&parse_data for parsing multiple documents (or don't use expat and reuse parse_data)
	XML_Parser p = XML_ParserCreateNS(NULL, SEPARATOR);
	//FIXME: check exception & rollback
	if (!p) throw USER_ENV_EXCEPTION("Couldn't allocate memory for parser\n",true);

	struct ft_parse_data *parse_data = (struct ft_parse_data *)malloc(sizeof(struct ft_parse_data));
	parse_data->word_len = 0;
	parse_data->word_ind = 0;
	parse_data->overfl = false;
	parse_data->cur_idx = ftc_idx;
	U_ASSERT(op == ft_insert);
	parse_data->cur_doc = ftc_add_new_doc(ftc_idx, acc);

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
			free(parse_data);
			XML_ParserFree(p);
			throw USER_EXCEPTION2(SE2005, tmp);
		}
	} while (len > 0);
	text_buf->free_cursor(cur);

	free(parse_data);
	XML_ParserFree(p);
}

#include "tr/idx/indexes.h"

void ft_idx_create(std::vector<xptr> *first_nodes, ft_idx_data_t *ft_data, ft_index_type cm, ft_custom_tree_t* custom_tree, ftc_index_t ftc_idx)
{
	op_str_buf in_buf;

	for (std::vector<xptr>::iterator it = first_nodes->begin(); it != first_nodes->end(); ++it)
	{
		xptr tmp = *it;
		while (tmp != XNULL)
		{
			CHECKP(tmp);
			xptr tmp_indir = ((n_dsc*)XADDR(tmp))->indir;
			//TODO: see whether rewriting this to serialize directly to text parser (expat?), without writing to buffer first is better.
			in_buf.clear();
			print_node_to_buffer(tmp,in_buf,cm,custom_tree);
			ft_index_update(ft_insert, tmp_indir, &in_buf, ft_data, ftc_idx);

			tmp=getNextDescriptorOfSameSortXptr(tmp);
		}
	}
}

void ft_idx_delete(ft_idx_data_t *ft_data)
{
	bt_drop(ft_data->btree_root);
}

