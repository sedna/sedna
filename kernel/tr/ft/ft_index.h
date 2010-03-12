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


void ft_idx_create(std::vector<xptr> *first_nodes, ft_idx_data_t *ft_data, ft_index_type cm, ft_custom_tree_t* custom_tree, ftc_index_t ftc_idx);
void ft_idx_delete(ft_idx_data_t *ft_data);


#endif
