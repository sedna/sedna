/*
 * File:  ft_index.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_INDEX_H
#define _FT_INDEX_H

#include <vector>
#include "common/xptr.h"
#include "tr/ft/ft_index_data.h"
#include "tr/strings/strings.h"
#include "tr/ft/ft_cache.h"
#include "tr/ft/ft_storage.h"

enum ft_index_op_t
{
	ft_insert, ft_delete, ft_update
};

//op must be insert
void ft_index_update(ft_index_op_t op, xptr acc, op_str_buf *text_buf, ftc_index_t ftc_idx);
void ft_index_delete_doc(ftc_index_t ftc_idx, xptr acc);
void ft_idx_delete(struct FtsData *ft_data);

void ft_highlight(op_str_buf *in_buf, stmt_str_buf *out_buf, ft_word_ind_t *inds, size_t inds_count, bool hl_fragment);


#endif
