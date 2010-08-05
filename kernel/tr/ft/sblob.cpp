/*
 * File:  sblob.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/u/u.h"
#include "tr/vmm/vmm.h"
#include "tr/ft/sblob.h"

void SblobWriter::alloc_new_blk()
{
	//last_block == XNULL => last_block_avail == 0
	U_ASSERT(last_block_avail == 0);

	xptr new_blk;
	vmm_alloc_data_block(&new_blk);
	if (last_block != XNULL)
	{
		sblob_blk_hdr * const hdr = (sblob_blk_hdr *)XADDR(last_block);
		WRITEP(last_block);
		U_ASSERT(hdr->cursor == PAGE_SIZE);
		hdr->nblk = new_blk;
	}
	WRITEP(new_blk);
	sblob_blk_hdr * const new_blk_hdr = (sblob_blk_hdr *)XADDR(new_blk);
	new_blk_hdr->cursor = sizeof(struct sblob_blk_hdr);
	new_blk_hdr->nblk = XNULL;
	new_blk_hdr->pblk = last_block;

	last_block = new_blk;
	U_ASSERT(PAGE_SIZE - new_blk_hdr->cursor == max_data_in_block);
	last_block_avail = max_data_in_block;
}

xptr SblobWriter::create_new()
{
	alloc_new_blk();
	nbytes_written = 0;
	return cur_ptr();
}

void SblobWriter::flush()
{
	U_ASSERT(buf_pos <= last_block_avail);

	WRITEP(last_block);
	sblob_blk_hdr * const hdr = (sblob_blk_hdr *)XADDR(last_block);
	int cursor = hdr->cursor;
	U_ASSERT(cursor + buf_pos <= PAGE_SIZE);
	memcpy((char*)XADDR(last_block) + cursor, buf, buf_pos);
	cursor += buf_pos;
	last_block_avail -= buf_pos;
	hdr->cursor = cursor;

	buf_pos = 0;
	if (last_block_avail == 0)
		alloc_new_blk();
}

void SblobWriter::write(const char *data, int len)
{
	while (buf_pos + len > last_block_avail)
	{
		const int cnt = last_block_avail - buf_pos; //cnt < len
		if (cnt > 0)
		{
			memcpy(&buf[buf_pos], data, cnt);
			buf_pos += cnt;
			data += cnt;
			len -= cnt;
			nbytes_written += cnt;
		}
		flush();
	}
	// buf_pos + len <= last_block_avail <= sizeof(buf)
	memcpy(&buf[buf_pos], data, len);
	buf_pos += len;
	nbytes_written += len;
}

xptr SblobWriter::cur_ptr()
{
	//if next byte to be written is in the next block, need to flush and allocate new block first
	if (last_block_avail == buf_pos)
		flush();
	U_ASSERT(last_block_avail > buf_pos);
	return last_block + (PAGE_SIZE - last_block_avail + buf_pos);
}

void SblobReader::init(const xptr pos)
{
	this->cur_blk = BLOCKXPTR(pos);
	this->pos_in_blk = pos - this->cur_blk;

	//check that either there are some bytes to read or cur_blk == XNULL, so that eos() and other methods that rely on it work
	//note that currenly empty sblob's are possible (i.e. containing 0 bytes of data)
	if (this->cur_blk != XNULL)
	{
		CHECKP(this->cur_blk);
		const struct sblob_blk_hdr *hdr = (struct sblob_blk_hdr *)XADDR(cur_blk);
		if (pos_in_blk >= hdr->cursor)
		{
			cur_blk = hdr->nblk;
			U_ASSERT(cur_blk == XNULL);
		}
	}
}

size_t SblobReader::read_str(char *dest, size_t max_len)
{
	size_t len = 0;
	if (cur_blk == XNULL)
		return 0;
	while (max_len > 0)
	{
		const struct sblob_blk_hdr *hdr = (struct sblob_blk_hdr *)XADDR(cur_blk);
		CHECKP(cur_blk);
		int avail = s_min(hdr->cursor - pos_in_blk, max_len);

		const char * const curp = (char*)XADDR(cur_blk) + pos_in_blk;
		const void *p = memchr(curp, 0, avail);
		if (p != NULL)
		{
			const ptrdiff_t sz = 1+((char*)p-curp);
			memcpy(dest, curp, sz);
			pos_in_blk += sz;
			if (pos_in_blk >= hdr->cursor)
			{
				if (hdr->nblk == XNULL)
					cur_blk = XNULL;
				else
				{
					cur_blk = hdr->nblk;
					pos_in_blk = sizeof(struct sblob_blk_hdr);
				}
			}
			return len+sz;
		}
		else
		{
			//copy string, and move to next block if needed
			memcpy(dest, curp, avail);
			dest += avail;
			pos_in_blk += avail;
			max_len -= avail;
			len += avail;
			//now either max_len == 0 or no more data in current block
			if (pos_in_blk >= hdr->cursor)
			{
				if (hdr->nblk == XNULL)
				{
					cur_blk = XNULL;
					return len;
				}
				cur_blk = hdr->nblk;
				pos_in_blk = sizeof(struct sblob_blk_hdr);
			}
		}
	}
	return len;
}

size_t SblobReader::read_bytes(char *dest, size_t cnt)
{
	size_t nread = 0;
	if (cur_blk == XNULL)
		return 0;
	while (cnt > nread)
	{
		const struct sblob_blk_hdr *hdr = (struct sblob_blk_hdr *)XADDR(cur_blk);
		CHECKP(cur_blk);
		int avail = s_min(hdr->cursor - pos_in_blk, cnt-nread);
		memcpy(dest, (char*)XADDR(cur_blk) + pos_in_blk, avail);
		dest += avail;
		nread += avail;
		pos_in_blk += avail;
		if (pos_in_blk >= hdr->cursor)
		{
			if (hdr->nblk == XNULL)
			{
				cur_blk = XNULL;
				return nread;
			}
			cur_blk = hdr->nblk;
			pos_in_blk = sizeof(struct sblob_blk_hdr);
		}
	}
	return nread;
}

void sblob_delete(xptr ptr)
{
	xptr blk = BLOCKXPTR(ptr);
	while (blk != XNULL)
	{
		xptr nblk;
		CHECKP(blk);
		nblk = BLOCKXPTR(((struct sblob_blk_hdr *)XADDR(blk))->nblk);
		vmm_delete_block(blk);
		blk = nblk;
	}
}