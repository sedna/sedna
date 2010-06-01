/*
 * File:  ft_storage.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_storage.h"
#include "tr/idx/btree/btree.h"

void fts_create(struct FtsData *data)
{
	data->btree_root = bt_create(xs_string);
}