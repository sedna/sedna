/*
 * File:  sblob.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_SBLOB_H
#define _FT_SBLOB_H

#include "common/sm_vmm_data.h"

// static BLOB structure, very similar to e_strings, but has a different interface and doesn't use static buffers

struct sblob_blk_hdr
{
    vmm_sm_blk_hdr sm_vmm;	// sm/vmm parameters
    xptr nblk;				// next block
	xptr pblk;				// pred block
    int cursor;				// cursor
};

class SblobWriter
{
private:
	xptr last_block;			//last allocated sblob block
	int last_block_avail;       //space (in bytes) available in last_block
	char buf[PAGE_SIZE];
	int buf_pos;                //buf_pos must be <= last_block_avail

	int64_t nbytes_written;
	int	nblocks;

	static const int max_data_in_block = (PAGE_SIZE - sizeof(struct sblob_blk_hdr));
	void alloc_new_blk();
public:
	SblobWriter() : last_block(XNULL), last_block_avail(0), buf_pos(0), nbytes_written(0), nblocks(0) {}
	xptr create_new();
	void flush();
	void write(const char *data, int len);
	const int64_t bytes_written() { return nbytes_written; }
	const int block_count() { return nblocks; }
	xptr cur_ptr();
};

class SblobReader
{
private:
	xptr cur_blk;     //using one xptr is inconvinient because xptr arithmetics is buggy
	int pos_in_blk;
public:
	SblobReader() : cur_blk(XNULL) {}
	void init(const xptr pos);
	bool eos() { return cur_blk == XNULL; }

	///read c-string, but no more than max_len bytes
	///returns number of bytes read, including the trailing '\x0' if it was read
	size_t read_str(char *dest, size_t max_len);
	size_t read_bytes(char *dest, size_t cnt);
};

void sblob_delete(xptr ptr);

#endif