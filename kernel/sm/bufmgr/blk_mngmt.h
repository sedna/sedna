/*
 * File:  blk_mngmt.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BLK_MNGMT_H
#define _BLK_MNGMT_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"

struct tr_info;

int push_to_persistent_free_blocks_stack(xptr *hd, xptr p, bool canStoreFreeBlocks = true);
int pop_from_persistent_free_blocks_stack(xptr *hd, xptr *p);
int64_t count_elems_of_persistent_free_blocks_stack(xptr hd, bool examineHeadOnly = false);
bool is_in_persistent_free_blocks_stack(xptr hd, xptr what);

int push_to_persistent_used_blocks_stack(xptr *hd, xptr p);
int pop_from_persistent_used_blocks_stack(xptr *hd, xptr *p);

void extend_data_file(int extend_portion);
void extend_tmp_file (int extend_portion);



void new_data_block		(xptr /*out*/ *p);
void delete_data_block	(const xptr &p);

void new_tmp_block		(xptr /*out*/ *p);
void delete_tmp_block	(const xptr &p, tr_info *info);



#endif

