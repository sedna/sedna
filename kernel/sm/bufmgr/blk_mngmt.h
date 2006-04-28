/*
 * File:  blk_mngmt.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BLK_MNGMT_H
#define _BLK_MNGMT_H

#include "sedna.h"
#include "xptr.h"
#include "exceptions.h"

int push_to_persistent_free_blocks_stack(xptr *hd, xptr p);
int pop_from_persistent_free_blocks_stack(xptr *hd, xptr *p);
int count_elems_of_persistent_free_blocks_stack(xptr hd);

int push_to_persistent_used_blocks_stack(xptr *hd, xptr p);
int pop_from_persistent_used_blocks_stack(xptr *hd, xptr *p);



void extend_data_file(int extend_portion) throw (SednaException);
void extend_tmp_file (int extend_portion) throw (SednaException);



void new_data_block		(xptr /*out*/ *p);
void delete_data_block	(const xptr &p);

void new_tmp_block		(xptr /*out*/ *p);
void delete_tmp_block	(const xptr &p, tr_info *info);



#endif

