/*
 * File:  ft_index.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"
#include "tr/ft/ft_index.h"
#include "tr/idx/btree/btree.h"
#include "tr/crmutils/node_utils.h"
#include "tr/crmutils/crmutils.h"
#include "expat/expat.h"
#include "tr/ft/ft_cache.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/tr_globals.h"


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

//put utf8 char to buf
//return true if ok or false if not enough space in buffer
//if ch >= (1 << 21), returns true and doesnt write anything to buffer
static inline bool utf8_putch(int ch, char *buf, int *buf_p, const int buf_size)
{
	if (ch < (1 << 7)) {
		if (*buf_p+1 > buf_size) return false;
		buf[(*buf_p)++] = ch;
    } else if (ch < (1 << 11)) {
		if (*buf_p+2 > buf_size) return false;
		buf[(*buf_p)++] = ((ch >> 6) | 0xc0);
		buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
    } else if (ch < (1 << 16)) {
		if (*buf_p+3 > buf_size) return false;
		buf[(*buf_p)++] = ((ch >> 12) | 0xe0);
		buf[(*buf_p)++] = (((ch >> 6) & 0x3f) | 0x80);
		buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
    } else if (ch < (1 << 21)) {
		if (*buf_p+4 > buf_size) return false;
		buf[(*buf_p)++] = ((ch >> 18) | 0xf0);
		buf[(*buf_p)++] = (((ch >> 12) & 0x3f) | 0x80);
		buf[(*buf_p)++] = (((ch >> 6) & 0x3f) | 0x80);
		buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
    }
	return true;
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
				parse_data->overfl = !utf8_putch(ch, parse_data->word_buf, &parse_data->word_len, MAX_WORD_LENGTH);
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


void ft_index_new_node(xptr acc, op_str_buf *text_buf, ft_idx_data_t *ft_data, ftc_index_t ftc_idx)
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
	parse_data->cur_doc = ftc_add_new_doc(ftc_idx, acc);

	XML_SetUserData(p, parse_data);
	XML_SetReturnNSTriplet(p,XML_TRUE);
	XML_SetElementHandler(p, p_start, p_end);
	XML_SetCharacterDataHandler(p, p_data);

	//FIXME: add some function to str_buf/op_str_buf to make this less weird
	if (text_buf->get_type() == text_mem)
	{
		if (XML_Parse(p, (char*)text_buf->get_ptr_to_text(), text_buf->get_size(), XML_TRUE) == XML_STATUS_ERROR)
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
	}
	else
	{
		estr_cursor cur(*(xptr*)text_buf->get_ptr_to_text(), text_buf->get_size());
		char buf[PAGE_SIZE]; //FIXME?
		int len;

		do
		{
			len = cur.copy_blk(buf);
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
	}
	free(parse_data);
	XML_ParserFree(p);
}



//FIXME: all these fuctions are very bad
//FIXME: replace int32_t for index with some other typedef
//FIXME: why function that compares things (and returns int) ends  with _less???
#include "tr/idx/indexes.h"
//XXX: weird function (idx_get_size) from indexes.cpp, also it should heed ALIGNMENT_REQUIRED (besides other things like
//     using some weird assumptions on sorted_sequence blocks structure
static shft get_sz(const xptr& v1, const void * Udata)
{
	shft sz = 0;
	CHECKP(v1);
	
	if (GET_FREE_SPACE(v1)<sizeof(shft))
	{
		((idx_buffer*)Udata)->copy_to_buffer(v1,GET_FREE_SPACE(v1));
		xptr v=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
		CHECKP(v);
		((idx_buffer*)Udata)->copy_to_buffer(v+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(shft)-GET_FREE_SPACE(v1));
		sz=*(shft*)(((idx_buffer*)Udata)->get_buffer_pointer());
	}
	else
	{
		sz=*((shft*)XADDR(v1));
	}
	return sz;
}
//XXX
#ifndef min
#define min(x,y) ((x) < (y) ? (x) : (y))
#endif
int ftc_ss_compare_less(xptr v1, xptr v2, const void * Udata)
{
	idx_buffer* buffer = ((idx_buffer*)Udata);
	shft sz1 = get_sz(v1,Udata);
	shft sz2 = get_sz(v2,Udata);
	int v1part = GET_FREE_SPACE(v1);
	int v2part = GET_FREE_SPACE(v2);
	if (v1part > sz1 + sizeof(shft)+sizeof(xptr)+sizeof(int32_t))
		v1part = sz1 + sizeof(shft)+sizeof(xptr)+sizeof(int32_t);
	if (v2part > sz2 + sizeof(shft)+sizeof(xptr)+sizeof(int32_t))
		v2part = sz2 + sizeof(shft)+sizeof(xptr)+sizeof(int32_t);

	buffer->copy_to_buffer(v1, v1part);
	xptr vs=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
	if (sz1+sizeof(shft)+sizeof(xptr)+sizeof(int32_t)-v1part > 0)
		buffer->copy_to_buffer(vs, v1part,sz1+sizeof(shft)+sizeof(xptr)+sizeof(int32_t)-v1part);
	int end1 = sz1+sizeof(shft)+sizeof(xptr)+sizeof(int32_t);

	buffer->copy_to_buffer(v2, end1, v2part);
	vs=((seq_blk_hdr*)XADDR(BLOCKXPTR(v2)))->nblk+sizeof(seq_blk_hdr);
	if (sz2+sizeof(shft)+sizeof(xptr)+sizeof(int32_t)-v2part > 0)
		buffer->copy_to_buffer(vs, end1+v2part,sz2+sizeof(shft)+sizeof(xptr)+sizeof(int32_t)-v2part);

	char *b1 = buffer->get_buffer_pointer();
	char *b2 = buffer->get_buffer_pointer() + end1;

	int res = sign(memcmp(b1+sizeof(shft)+sizeof(xptr)+sizeof(int32_t), b2+sizeof(shft)+sizeof(xptr)+sizeof(int32_t), min(sz1, sz2))); //XXX: do we really need 1/-1/0?
	if (res)
		return res;
	if (sz1 < sz2)
		return -1;
	if (sz2 < sz1)
		return 1;

	const xptr &x1 = *((xptr*)(b1+sizeof(shft)));
	const xptr &x2 = *((xptr*)(b2+sizeof(shft)));
	if (x1 < x2)
		return -1;
	if (x2 < x1)
		return 1;

	int32_t i1 =*((int32_t*)(b1+sizeof(shft)+sizeof(xptr)));
	int32_t i2 =*((int32_t*)(b2+sizeof(shft)+sizeof(xptr)));
	if (i1 < i2)
		return -1;
	if (i2 < i1)
		return 1;
	return 0;
}

int ftc_ss_get_size (tuple& t, const void * Udata)
{
	return sizeof(shft)+sizeof(xptr)+t.cells[0].get_strlen()+sizeof(int32_t);
}

void ftc_ss_serialize (tuple& t,xptr v1, const void * Udata)
{
	shft sz=t.cells[0].get_strlen();
	CHECKP(v1);
    void * p=XADDR(v1);
	VMM_SIGNAL_MODIFICATION(v1);

#ifdef ALIGNMENT_REQUIRED
	TODO
#else
    *((shft*)p)=sz;
    *((xptr*)((char*)p+sizeof(shft)))=t.cells[1].get_node();
	*((int32_t*)((char*)p+sizeof(shft)+sizeof(xptr)))=(int32_t)t.cells[2].get_xs_integer();
#endif
    shft offset=sizeof(shft)+sizeof(xptr)+sizeof(int32_t);

    tuple_cell& tc=t.cells[0];
	/*tc = tuple_cell::make_sure_light_atomic(tc);
	CHECKP(v1);
	VMM_SIGNAL_MODIFICATION(v1);*/
	char* str = tc.get_str_mem();
	memcpy((char*)p+offset, str, sz); 
}


void ftc_ss_serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    idx_buffer* buffer = (idx_buffer*)Udata;
    shft sz=t.cells[0].get_strlen();
    buffer->copy_to_buffer(&sz,sizeof(shft));
    xptr tmp=t.cells[1].get_node();
    buffer->copy_to_buffer(&tmp, sizeof(shft),sizeof(xptr));
	int32_t tmp1 = (int32_t)t.cells[2].get_xs_integer();
	buffer->copy_to_buffer(&tmp1, sizeof(shft)+sizeof(xptr),sizeof(int32_t));
    shft offset=sizeof(shft)+sizeof(xptr)+sizeof(int32_t);
    tuple_cell& tc=t.cells[0];
	
	buffer->expand_to_size(offset+sz);
	tc = tuple_cell::make_sure_light_atomic(tc);
	char* str = tc.get_str_mem();
	memcpy(buffer->get_buffer_pointer()+offset, str, sz);

    buffer->copy_from_buffer(v1, 0, size1);
    buffer->copy_from_buffer(v2, size1, sz+offset-size1);
}

static tuple_cell get_tc(void* buf, shft size)
{
	char* str = se_new char[size+1];
	memcpy(str, (char*)buf, size);
	str[size]='\0';
	return tuple_cell::tuple_cell(xs_string, str); 
}

void ftc_ss_deserialize (tuple &t, xptr& v1, const void * Udata)
{
    idx_buffer* buffer = (idx_buffer*)Udata;
    CHECKP(v1);
    shft sz=*((shft*)XADDR(v1));

    xptr v2=v1+sizeof(shft);
#ifdef ALIGNMENT_REQUIRED
	TODO
#else
    xptr v22=v2+sizeof(xptr);
    xptr v3=v22+sizeof(int32_t);
	
	tuple_cell key = get_tc( XADDR(v3),sz);

	t.eos = false;
    t.cells[0] = key;
    t.cells[1] = tuple_cell::node(*((xptr*)XADDR(v2)));
	t.cells[2] = tuple_cell::atomic((int64_t)*((int32_t*)XADDR(v22)));//XXX: tuple_cells suck
#endif				
}

void ftc_ss_deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	idx_buffer* buffer = ((idx_buffer*)Udata);	
	shft sz = get_sz(v1,Udata);
	buffer->copy_to_buffer(v1, size1);
	xptr vs=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
	buffer->copy_to_buffer(vs, size1,sz+sizeof(shft)+sizeof(xptr)+sizeof(int32_t)-size1);

	t.eos = false;
    t.cells[0] = get_tc( buffer->get_buffer_pointer()+sizeof(shft)+sizeof(xptr)+sizeof(int32_t),
					sz
				   );
    t.cells[1] = tuple_cell::node(*((xptr*)(buffer->get_buffer_pointer()+sizeof(shft))));
	t.cells[2] = tuple_cell::atomic((int64_t)*((int32_t*)(buffer->get_buffer_pointer()+sizeof(shft)+sizeof(xptr))));//XXX: tuple_cells suck
}

#include "tr/idx/indexes.h"

void ft_idx_create(std::vector<xptr> *first_nodes, ft_idx_data_t *ft_data, ft_index_type cm, ft_custom_tree_t* custom_tree, ftc_index_t ftc_idx)
{
	op_str_buf in_buf;

	idx_buffer buf;
	sorted_sequence * ss = se_new sorted_sequence(ftc_ss_compare_less,ftc_ss_get_size,ftc_ss_serialize,ftc_ss_serialize_2_blks,ftc_ss_deserialize,ftc_ss_deserialize_2_blks,&buf);

	ftc_set_ss(ftc_idx, ss);

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
			ft_index_new_node(tmp_indir, &in_buf, ft_data, ftc_idx);

			tmp=getNextDescriptorOfSameSortXptr(tmp);
		}
	}

	ftc_set_ss(ftc_idx, NULL);

	//TODO: fix this

	d_printf1("sort call");
	ss->lazy_sort();
	d_printf1("sort call ended\n");
	tuple t(3);
	bt_key bkey;
	int nres=0;
	while (true)
	{
		ss->next(t);
		if (t.is_eos())
			break;
		else
		{
			nres++;
			if (nres & 0xffff == 1)
				d_printf2("nres = %d\n", nres);
			const char *str = t.cells[0].get_str_mem();
			xptr acc = t.cells[1].get_node();
			int64_t ind = t.cells[2].get_xs_integer();

			bkey.setnew(str);

			bt_insert_tmpl<ft_idx_btree_element>(ft_data->btree_root, bkey, ft_idx_btree_element(acc, ind), false);
		}
    }
	delete ss;
	ss = NULL;
}

void ft_idx_delete(ft_idx_data_t *ft_data)
{
	bt_drop(ft_data->btree_root);
}

