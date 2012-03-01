/*
 * File:  ft_partition.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_PARTITION_H
#define _FT_PARTITION_H

#include "common/xptr.h"
#include "tr/ft/ft_types.h"
#include "tr/ft/sblob.h"

struct ft_partition_data
{
	xptr voc_btree_root;
	xptr data;
	xptr del_list;
	int sblob_blocks;
};
typedef int ftp_ind_t;

//FIXME: make sure it's no less than max word length in ft_index
#define FTP_MAX_WORD_LENGTH 250

class FtPartitionSblobCursor
{
public:
	enum state_t {
		st_word, //new word may be started here
		st_acc,  //acc may be started here
		st_ind   //ind list may be started here
	} state;
	char cur_word[FTP_MAX_WORD_LENGTH+1];
	uint64_t cur_acc_i;
	ftp_ind_t cur_ind;
};

class FtPartitionSblobWriter : public FtPartitionSblobCursor
{
private:
	SblobWriter data_writer;
public:
	FtPartitionSblobWriter(bool _pers) : data_writer(_pers) {}
	xptr create_new();
	void finalize() { set_st_word(); data_writer.flush(); }
	int64_t bytes_written() { return data_writer.bytes_written(); }
	int block_count()       { return data_writer.block_count(); }

	//reset state to st_word by closing accesor and word index list if needed
	void set_st_word();
	//append xptr to the list of accesors
	void start_acc_i(const uint64_t acc_i);
	//end a list of accesors
	void end_acc();
	//make sure that we're in in the index list of acc, must be inside some word data (i.e. state >= st_acc)
	void set_acc_i(const uint64_t acc_i);
	//append an index to the list of word indexes
	void write_ind(const ftp_ind_t ind);
	//end list of word indexes
	void end_ind();

	xptr start_word(const char *word);
};
class FtPartitionReader : public FtPartitionSblobCursor
{
private:
	SblobReader data_reader;
	bool fl_curw;
	bool fl_cura;
	bool fl_eos;
public:
	xptr del_list;
	FtPartitionReader() {}

	void init(const ft_partition_data *partition);
	void init(const ft_partition_data *partition, const char *word);

	//state must be st_word, returns false if we're at the end of sblob
	//if returned true, state becomes st_acc
	bool read_word();
	//state must be st_acc, returns false if at the end of acc-list for current word, state becomes st_word
	//if returned true, state becomes st_ind
	bool read_acc();
	//state must be st_ind, returns false if at the end of ind-list, state become st_acc
	bool read_ind();

	//state must be st_ind; skips all indexes for current acc; state becomes st_acc
	void skip_inds();

	bool acci_in_del_list(const uint64_t acc_i);

	//flags used by scanner
	bool flag_curw() {return fl_curw; }
	void flag_curw_set() {fl_curw = true; }
	void flag_curw_clear() {fl_curw = false; }
	bool flag_cura() {return fl_cura; }
	void flag_cura_set() {fl_cura = true; }
	void flag_cura_clear() {fl_cura = false; }
	bool flag_eos() {return fl_eos; }
	void flag_eos_set() {fl_eos = true; }
	void flag_eos_clear() {fl_eos = false; }
};

class FtPartitionBuilder
{
private:
	bool pers;
	struct ft_partition_data p_data;
	FtPartitionSblobWriter sblob_writer;

	void start_word(const char *word);
public:
	FtPartitionBuilder(bool _pers) : pers(_pers), sblob_writer(_pers) {}
	//create empty partition, which can be filled with del_doc and add_*/set_* methods,
	//arguments of these methods must be in ascending order during calls
	void create_new();
	//finish creating partition and save it's data in partition
	void finalize(ft_partition_data *partition);

	void del_doc(const xptr acc); //adds acc to deleted list
	void set_word(const char *word);
	void set_acc(const xptr acc);
	void add_ind(const ftp_ind_t ind);
	//FIXME: remove this function
	void add_word_occur(const char *word, const xptr acc, const ftp_ind_t word_ind);
};

//class used to scan several partitions of index
class FtPartitionScanner
{
private:
	FtPartitionReader *readers;
	int n;

	//after init, all readers ether have eos flag, or valid cur_word, cur_acc and cur_ind (and thus state == st_ind)
	int ncurw;//number of readers with smallest word
	int ncura;

	//find smallest word and set curw flags and ncurw, returns false if all readers are at eos
	bool find_smallest_word();
	//find smallest acc for readers with curw and set cura flags and ncura
	void find_smallest_acc();
	//check if i-th readear contains valid acc (i.e. not deleted)
	//returns smallest ind among readers with cura and advances corr. reader, ncura must be positive
	ftp_ind_t extract_min_ind();

	bool acc_is_valid(int i);
	//read next acc and ind, state must be st_acc
	//if there's no more accs for current word, clears curw flag and reads acc+ind for the next word or sets eos
	void next_acc(int i);

	bool one_word;
public:
	FtPartitionScanner() : readers(NULL), n(0) {}

	//newer partitions have bigger indices

	//scan entire partition(s)
	void init(const ft_partition_data *partitions, int npartitions);
	//scan postings of some word
	void init(const ft_partition_data *partitions, int npartitions, const char *word);

	void merge(ft_partition_data *dest_partition, bool merge_del_list, bool pers);
	bool at_end() { return ncurw == 0; }

	//read next posting for current word
	//if there's no more data for current word, returns false
	bool get_next_occur(ft_acc_uint_t *acc_i, ftp_ind_t *ind);
	//similar to get_next_occur but always moves to next acc (and returns false if there's no new acc-s for current word)
	bool get_next_acc(xptr *acc, ftp_ind_t *ind);


	~FtPartitionScanner();
};

FtWordsScanner *ftp_init_words_scanner(const ft_partition_data *partition, const char* from);


void ft_delete_partition(const ft_partition_data *partition);

#endif
