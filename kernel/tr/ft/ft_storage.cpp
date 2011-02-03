/*
 * File:  ft_storage.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_storage.h"
#include "tr/btree/btree.h"

void fts_create(struct FtsData *data)
{
	data->npartitions = 0;
}

void FtsUpdater::begin_update(struct FtsData *_fts_data)
{
	fts_data = _fts_data;
	empty = true;
}

void FtsUpdater::end_update(struct FtsData *dest)
{
	if (empty)
		return;
	ft_partition_data pdata;
	pb.finalize(&pdata);

	if (dest->npartitions == 0)
	{
		dest->npartitions = 1;
		dest->partitions[0] = pdata;
		return;
	}

	if (dest->npartitions == FTS_MAX_PARTITIONS || dest->partitions[dest->npartitions-1].sblob_blocks < 2*pdata.sblob_blocks)
	{
		//merge some partitions
		int mergeto = dest->npartitions-1; //first partition that will be merged (and also the one that will be replaced with merge result)
		int64_t cur_sz = pdata.sblob_blocks + dest->partitions[mergeto].sblob_blocks;
		while (mergeto > 0)
		{
			//see if next partition is too big to be merged now
			if (dest->partitions[mergeto-1].sblob_blocks > 2*cur_sz)
				break;
			mergeto--;
			cur_sz += dest->partitions[mergeto].sblob_blocks;
		}

		//now merge partitions from mergeto to dest->npartitions and pdata to one partition
		const int nmerge = 1+dest->npartitions-mergeto;
		ft_partition_data *p = new ft_partition_data[nmerge];
		for (int i = 0; i < nmerge-1; i++)
			p[i] = dest->partitions[i+mergeto];
		p[nmerge-1] = pdata;

#ifdef EL_DEBUG
		d_printf2("will merge %d partitions with sizes: ", nmerge);
		for (int i = 0; i < nmerge; i++)
			d_printf2("%d, ", (int)(p[i].sblob_blocks));
		d_printf1("\n");
#endif

		FtPartitionScanner s;
		s.init(p, nmerge);
		//FIXME: some disk space can be saved if partitions are deleted during merge
		s.merge(&dest->partitions[mergeto], mergeto > 0);
		dest->npartitions = mergeto+1;
		for (int i = 0; i < nmerge; i++)
			ft_delete_partition(&p[i]);
		delete[] p;
#ifdef EL_DEBUG
		d_printf2("merge done, resulting size: %d\n", (int)(dest->partitions[mergeto].sblob_blocks));
#endif
	}
	else
	{
		//don't merge anything
		dest->npartitions++;
		dest->partitions[dest->npartitions-1] = pdata;
	}
}
