/*
 * File:  ft_partition.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_partition.h"
#include "tr/btree/btree.h"

xptr FtPartitionSblobWriter::create_new()
{
	const xptr data = data_writer.create_new();

	cur_word[0] = '\x0';
	cur_acc_i = 0;
	state = st_word;

	return data;
}

void FtPartitionSblobWriter::set_st_word()
{
	const state_t st = state;
	switch (st)
	{
	case st_ind:
		end_ind();
		//fall through
	case st_acc:
		end_acc();
		//fall through
	case st_word:
		break;
	default:
		U_ASSERT(false);
	}
}
void FtPartitionSblobWriter::start_acc_i(const uint64_t acc_i)
{
	U_ASSERT(state == st_acc);
	U_ASSERT(acc_i > this->cur_acc_i);
	data_writer.write_uint(acc_i - this->cur_acc_i);
	cur_acc_i = acc_i;
	cur_ind = 0;
	state = st_ind;
}
void FtPartitionSblobWriter::end_acc()
{
	U_ASSERT(state == st_acc);
	data_writer.write_uint(0);
	state = st_word;
	cur_word[0] = '\x0';
}

void FtPartitionSblobWriter::set_acc_i(const uint64_t acc_i)
{
	U_ASSERT(state >= st_acc);
	if (state == st_acc || cur_acc_i != acc_i)
	{
		if (state == st_ind)
			end_ind();
		U_ASSERT(state == st_acc);
		start_acc_i(acc_i);
	}
}

void FtPartitionSblobWriter::write_ind(const ftp_ind_t ind)
{
	U_ASSERT(ind > cur_ind);
	data_writer.write_uint(ind - cur_ind);
	cur_ind = ind;
}
void FtPartitionSblobWriter::end_ind()
{
	U_ASSERT(state == st_ind);
	data_writer.write_uint(0);
	state = st_acc;
}

xptr FtPartitionSblobWriter::start_word(const char *word)
{
	U_ASSERT(state == st_word);
	const int len = strlen(word);
	U_ASSERT(len <= FTP_MAX_WORD_LENGTH);
	strncpy(cur_word, word, FTP_MAX_WORD_LENGTH);
	cur_word[len] = '\x0';

	//add ptr to the (soon to be) added word to vocabulary
	const xptr ptr = data_writer.cur_ptr();

	data_writer.write(cur_word, len+1);
	state = st_acc;
	cur_acc_i = 0; //FIXME: make sure FT_XPTR_TO_UINT(XNULL) == 0

	return ptr;
}

void FtPartitionReader::init(const ft_partition_data *partition)
{
	del_list = partition->del_list;
	data_reader.init(partition->data);
	flag_eos_clear();
	state = st_word;
}

void FtPartitionReader::init(const ft_partition_data *partition, const char *word)
{
	del_list = partition->del_list;

	bt_key bkey;
	bkey.setnew(word);
	bt_cursor cur = bt_find(partition->voc_btree_root, bkey);

	xptr p = cur.bt_next_obj();

	if (p == XNULL)
		flag_eos_set();
	else
	{
		data_reader.init(p);
		flag_eos_clear();
		state = st_word;
	}
}

bool FtPartitionReader::read_word()
{
	U_ASSERT(state == st_word);
	if (data_reader.eos())
		return false;

	//FIXME: check that we've read the whole word
	data_reader.read_str(cur_word, FTP_MAX_WORD_LENGTH+1);
	state = st_acc;
	cur_acc_i = 0;
	return true;
}
bool FtPartitionReader::read_acc()
{
	U_ASSERT(state == st_acc);
	uint64_t acc_delta = data_reader.read_uint();
	if (acc_delta == 0)
	{
		state = st_word;
		return false;
	}
	else
	{
		state = st_ind;
		cur_acc_i = cur_acc_i + acc_delta;
		cur_ind = 0;
		return true;
	}
}
bool FtPartitionReader::read_ind()
{
	U_ASSERT(state == st_ind);
	uint64_t ind_d = data_reader.read_uint();

	if (ind_d == 0)
	{
		state = st_acc;
		return false;
	}
	else
	{
		cur_ind += ind_d;
		return true;
	}
}
void FtPartitionReader::skip_inds()
{
	U_ASSERT(state == st_ind);
	while (state == st_ind)
		read_ind();
}

bool FtPartitionReader::acci_in_del_list(const uint64_t acc_i)
{
	bt_key bkey;
	bkey.setnew((int64_t)acc_i);
	bt_cursor cur = bt_find(del_list, bkey);

	xptr p = cur.bt_next_obj();

	return (p != XNULL);
}


void FtPartitionBuilder::create_new()
{
	//FIXME: create some persistent xptr set implementation
	U_ASSERT(xmlscm_type_size(xs_integer) == sizeof(xptr));

	p_data.voc_btree_root = bt_create(xs_string);
	p_data.del_list = bt_create(xs_integer);
	p_data.data = sblob_writer.create_new();
}

void FtPartitionBuilder::finalize(ft_partition_data *partition)
{
	sblob_writer.finalize();
	p_data.sblob_blocks = sblob_writer.block_count();
	*partition = p_data;
}

//XXX: end_* functions do redundant checks when one is called from another which may be optimized
//add word to sblob and vocabulary
void FtPartitionBuilder::start_word(const char *word)
{
	const xptr ptr = sblob_writer.start_word(word);

	bt_key bkey;
	bkey.setnew(word);
	bt_insert_tmpl<xptr>(p_data.voc_btree_root, bkey, ptr);
}
void FtPartitionBuilder::del_doc(const xptr acc)
{
	bt_key key;
	key.setnew((int64_t)FT_XPTR_TO_UINT(acc));
	bt_insert_tmpl<xptr>(p_data.del_list, key, acc);
}

void FtPartitionBuilder::set_word(const char *word)
{
	if (sblob_writer.state == FtPartitionSblobCursor::st_word || strncmp(word, sblob_writer.cur_word, FTP_MAX_WORD_LENGTH))
	{
		sblob_writer.set_st_word();
		start_word(word);
	}
}

void FtPartitionBuilder::set_acc(const xptr acc)
{
	sblob_writer.set_acc_i(FT_XPTR_TO_UINT(acc));
}

void FtPartitionBuilder::add_ind(const ftp_ind_t ind)
{
	U_ASSERT(sblob_writer.state == FtPartitionSblobCursor::st_ind);
	sblob_writer.write_ind(ind);
}


void FtPartitionBuilder::add_word_occur(const char *word, const xptr acc, const ftp_ind_t word_ind)
{
	set_word(word);
	set_acc(acc);
	U_ASSERT(sblob_writer.state == FtPartitionSblobCursor::st_ind && sblob_writer.cur_acc_i == FT_XPTR_TO_UINT(acc));
	add_ind(word_ind);
}

bool FtPartitionScanner::find_smallest_word()
{
	int minw = -1;
	for (int i = 0; i < n; i++)
		if (!readers[i].flag_eos() && (minw == -1 || strncmp(readers[i].cur_word, readers[minw].cur_word, FTP_MAX_WORD_LENGTH) < 0))
			minw = i;
	if (minw == -1)
		return false;

	//init curw flags and ncurw
	ncurw = 0;
	for (int i = 0; i < n; i++)
	{
		if (!readers[i].flag_eos() && !strncmp(readers[i].cur_word, readers[minw].cur_word, FTP_MAX_WORD_LENGTH))
		{
			readers[i].flag_curw_set();
			ncurw++;
		}
		else
			readers[i].flag_curw_clear();
	}
	U_ASSERT(ncurw > 0);
	return true;
}
void FtPartitionScanner::find_smallest_acc()
{
	int mina = -1;
	for (int i = 0; i < n; i++)
		if (readers[i].flag_curw() && (mina == -1 || readers[i].cur_acc_i < readers[mina].cur_acc_i))
			mina = i;

	ncura = 0;
	for (int i = 0; i < n; i++)
		readers[i].flag_cura_clear();

	if (mina == -1)
		return;

	for (int i = 0; i < n; i++)
		if (readers[i].flag_curw() && readers[i].cur_acc_i == readers[mina].cur_acc_i)
		{
			readers[i].flag_cura_set();
			ncura++;
		}
}

ftp_ind_t FtPartitionScanner::extract_min_ind()
{
	U_ASSERT(ncura > 0);
	int minind = -1;
	for (int i = 0; i < n; i++)
		if (readers[i].flag_cura() && (minind == -1 || readers[i].cur_ind < readers[minind].cur_ind))
			minind = i;
	U_ASSERT(minind >= 0);

	const ftp_ind_t retv = readers[minind].cur_ind;

	if (!readers[minind].read_ind())
	{
		readers[minind].flag_cura_clear();
		ncura--;
		next_acc(minind);
	}
	return retv;
}

bool FtPartitionScanner::acc_is_valid(int i)
{
	for (int j = i+1; j < n; j++)
		if (readers[j].acci_in_del_list(readers[i].cur_acc_i))
			return false;
	return true;
}

void FtPartitionScanner::next_acc(int i)
{
	while (true)
	{
		if (readers[i].read_acc())
		{
			if (acc_is_valid(i))
			{
				const bool r = readers[i].read_ind();
				U_ASSERT(r);
				return;
			}
			else
				readers[i].skip_inds();
		}
		else
		{
			if (readers[i].flag_curw())
			{
				readers[i].flag_curw_clear();
				ncurw--;
			}
			if (one_word || !readers[i].read_word())
			{
				readers[i].flag_eos_set();
				return;
			}
		}
	}
}

void FtPartitionScanner::init(const ft_partition_data *partitions, int npartitions)
{
	n = npartitions;
	one_word = false;
	readers = new FtPartitionReader[npartitions]();
	for (int i = 0; i < npartitions; i++)
		readers[i].init(&partitions[i]);

	//init eos flags, read data, clear flags
	for (int i = 0; i < n; i++)
	{
		U_ASSERT(readers[i].state == FtPartitionSblobCursor::st_word);
		readers[i].flag_cura_clear();
		readers[i].flag_curw_clear();
		if (readers[i].read_word())
		{
			next_acc(i);
		}
		else
			readers[i].flag_eos_set();
	}

	find_smallest_word();
	find_smallest_acc();
}

void FtPartitionScanner::init(const ft_partition_data *partitions, int npartitions, const char *word)
{
	n = npartitions;
	one_word = true;
	readers = new FtPartitionReader[npartitions]();
	for (int i = 0; i < npartitions; i++)
		readers[i].init(&partitions[i], word);

	//init eos flags, read data, clear flags
	for (int i = 0; i < n; i++)
	{
		if (readers[i].flag_eos())
			continue;
		U_ASSERT(readers[i].state == FtPartitionSblobCursor::st_word);
		readers[i].flag_cura_clear();
		readers[i].flag_curw_clear();
		if (readers[i].read_word())
		{
			next_acc(i);
		}
		else
			readers[i].flag_eos_set();
	}

	find_smallest_word();
	find_smallest_acc();
}

void FtPartitionScanner::merge(ft_partition_data *dest_partition, bool merge_del_list)
{
	FtPartitionBuilder b;

	b.create_new();

	if (merge_del_list)
	{
		struct p_struct
		{
			bt_cursor cur;
			xptr val;
		} *p;
		p = new p_struct[n];
		for (int i = 0; i < n; i++)
		{
			p[i].cur = bt_lm(readers[i].del_list);
			p[i].val = p[i].cur.bt_next_obj();
		}

		while (true)
		{
			xptr minacc = XNULL;
			for (int i = 0; i < n; i++)
				if (p[i].val != XNULL && (minacc == XNULL || FT_XPTR_TO_UINT(p[i].val) < FT_XPTR_TO_UINT(minacc)))
					minacc = p[i].val;

			if (minacc == XNULL)
				break;

			b.del_doc(minacc);
			for (int i = 0; i < n; i++)
				if (p[i].val == minacc)
				{
					const xptr tmp  = p[i].cur.bt_next_obj();
					U_ASSERT(tmp == XNULL);
					if (p[i].cur.bt_next_key())
						p[i].val = p[i].cur.bt_next_obj();
					else
						p[i].val = XNULL;
				}
		}

		delete[] p;
	}

	while (true)
	{
		if (ncurw < 1)
		{
			if (!find_smallest_word())
			{
				//all readers are at eos
				break;
			}
		}
		U_ASSERT(ncurw > 0);
		int minw = 0;
		while (minw < n && !readers[minw].flag_curw())
			minw++;
		U_ASSERT(minw < n);
		b.set_word(readers[minw].cur_word);
		while (ncurw > 0)
		{
			if (ncura < 1)
				find_smallest_acc();
			U_ASSERT(ncura > 0);

			int mina = 0;
			while (mina < n && !readers[mina].flag_cura())
				mina++;
			U_ASSERT(mina < n);
			b.set_acc(FT_UINT_TO_XPTR(readers[mina].cur_acc_i));

			while (ncura > 0)
				b.add_ind(extract_min_ind());
		}
	}

	b.finalize(dest_partition);
}

bool FtPartitionScanner::get_next_occur(ft_uint_t *acc_i, ftp_ind_t *ind)
{
	if (ncurw < 1)
		return false;

	if (ncura < 1)
		find_smallest_acc();

	U_ASSERT(ncura > 0);
	int mina = 0;
	while (mina < n && !readers[mina].flag_cura())
		mina++;
	U_ASSERT(mina < n);
	*acc_i = readers[mina].cur_acc_i;
	*ind = extract_min_ind();
	return true;
}

FtPartitionScanner::~FtPartitionScanner()
{
	if (readers)
	{
		delete[] readers;
		readers = NULL;
		n = 0;
	}
}

void ft_delete_partition(const ft_partition_data *partition)
{
	bt_drop(partition->del_list);
	bt_drop(partition->voc_btree_root);
	sblob_delete(partition->data);
}

class FtPartitionWordsScanner : public FtWordsScanner
{
private:
	bt_cursor bt_cur;
	bool eos;
public:
	FtPartitionWordsScanner(const ft_partition_data *partition);
	virtual const char *cur_word();
	virtual void next_word();
	virtual ~FtPartitionWordsScanner() {}
};

FtPartitionWordsScanner::FtPartitionWordsScanner(const ft_partition_data *partition)
{
	bt_cur = bt_lm(partition->voc_btree_root);
	eos = bt_cur.is_null();
}
const char *FtPartitionWordsScanner::cur_word()
{
	if (eos)
		return NULL;
	//FIXME: this needs at least some assert regarding key type
	return (char*)bt_cur.get_key().data();
}
void FtPartitionWordsScanner::next_word()
{
	eos = !bt_cur.bt_next_key();
}

FtWordsScanner *ftp_init_words_scanner(const ft_partition_data *partition)
{
	return new FtPartitionWordsScanner(partition);
}
