/*
 * File:  gmm.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _GMM_H
#define _GMM_H


#include "common/u/ummap.h"


void create_global_memory_mapping();

void release_global_memory_mapping();

void open_global_memory_mapping(int err_code);

void close_global_memory_mapping();

UMMap get_global_memory_mapping();

void get_vmm_region_values();

void set_vmm_region_values();


#endif
