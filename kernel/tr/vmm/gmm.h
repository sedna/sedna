/*
 * File:  gmm.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _GMM_H
#define _GMM_H


#include "u/ushm.h"

void create_global_memory_mapping(int os_primitives_id_min_bound);

void release_global_memory_mapping();

void open_global_memory_mapping(int err_code);

void close_global_memory_mapping();

UShMem get_global_memory_mapping();

#endif
