/*
 * File:  pers_heap.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/* pers_heap.h - persistent heap init/release routines.
   Copyright (c) 2003, 2004 ISP RAS. All rights reserved.

   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#ifndef _PERS_MALLOC_H
#define _PERS_MALLOC_H

#include "common/u/u.h"
#include "common/u/usecurity.h"

int pers_init(const char *file_name, const char *fm_name, global_name sph_name, const void *addr, int mem_release = 0);
int pers_release();
int pers_flush();

int pers_open(const char *file_name, const char *fm_name, global_name sph_name, const void *addr, int should_map = 1, int mem_release = 0);
int pers_close();

int pers_create(const char *file_name, const char *fm_name, const void *addr, int heap_size, USECURITY_ATTRIBUTES *sa);

void * pers_malloc(size_t size);
void pers_free(void *ptr);
void * pers_realloc(void *ptr, size_t size);




#endif

