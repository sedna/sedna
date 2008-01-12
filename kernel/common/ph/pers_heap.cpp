/*
 * File:  pers_heap.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/* pers_heap.cpp - persistent heap init/release routines.
   Copyright (c) 2003, 2004 ISP RAS. All rights reserved.

   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "common/u/ummap.h"
#include "common/u/usem.h"
#include "common/ph/pers_heap.h"
#include "common/ph/_pers_malloc.h"


static UFile 		ph_file;
static UMMap		ph_file_mapping;
static USemaphore	ph_semaphore;
static void			*ph_start_address = NULL;



/* Master block for persistent heap.
   ph_start_address points to the ph_masterblock */
struct ph_masterblock
{
    struct pers_malloc_vars vars;

    void *low_boundary;
    void *high_boundary;
    void *cur_pointer;
};

static struct ph_masterblock *ph_mb;

static global_name
FixPersHeapFileMappingName(global_name name)
{
	/*	On Unices file-mapping emulation is brain-dead. A regular file
		is mapped in memory without any proxy objects creation (no file-mapping
		object) and unlike Windows all mapped views derived from the same file
		are coherent. So concerning persistent heap, the file-mapping name is unused,
		so we reset it. */ 
#if _WIN32
	return name;
#else
	return NULL;
#endif
}

void *_persistent_morecore(long size)
{
     int is_minus, blocks_num;
     void *res;

     is_minus = size < 0 ? 1 : 0;
     blocks_num = BLOCKIFY(size);

     if (is_minus) --blocks_num;

     if (ph_mb->low_boundary  <= (char*)ph_mb->cur_pointer + blocks_num * BLOCKSIZE
      && ph_mb->high_boundary >= (char*)ph_mb->cur_pointer + blocks_num * BLOCKSIZE)
     {
         res = ph_mb->cur_pointer;
         ph_mb->cur_pointer = (char*)ph_mb->cur_pointer + blocks_num * BLOCKSIZE;
     }
     else res = NULL;

     return res;
}


int pers_init(const char *file_name, const char *fm_name, global_name sph_name, const void *addr, int mem_release)
{
    void *_addr = (void*)addr;

#ifdef _WIN32
    if (mem_release)
        if (VirtualFree(_addr,					// address of region
                        0,						// size of region
                        MEM_RELEASE				// operation type
                       ) == 0)
           return 1;
#endif

    ph_file = uOpenFile(file_name, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (ph_file == U_INVALID_FD) return 2;

    ph_file_mapping = uOpenFileMapping(ph_file, 0, FixPersHeapFileMappingName(fm_name), __sys_call_error);
    if (U_INVALID_FILEMAPPING(ph_file_mapping)) return 3;

    ph_start_address = uMapViewOfFile(ph_file_mapping, _addr, 0, 0, __sys_call_error);

    if (ph_start_address == NULL) return 4;

    if (USemaphoreOpen(&ph_semaphore, sph_name, __sys_call_error) != 0) return 5;

    ph_mb = (struct ph_masterblock*)ph_start_address;
    _vars = &(ph_mb->vars);

    return 0;
}

int pers_release()
{
    if (USemaphoreClose(ph_semaphore, __sys_call_error) != 0) return 1;

    if (uUnmapViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 2;

    if (uCloseFileMapping(ph_file_mapping, __sys_call_error) != 0) return 3;

    if (uCloseFile(ph_file, __sys_call_error) == 0) return 4;

    return 0;
}

int pers_flush()
{
    if (ph_start_address)
    {
        if (uFlushViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 1;
    }
    else
    {
        ph_start_address = uMapViewOfFile(ph_file_mapping, 0, 0, 0, __sys_call_error);
        if (ph_start_address == NULL) return 2;

        if (uFlushViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 3;

        if (uUnmapViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 4;

        ph_start_address = NULL;
    }

    return 0;
}

int pers_open(const char *file_name, const char *fm_name, global_name sph_name, const void *addr, int should_map, int mem_release)
{
    // when opening file check that is size greater then 0 and less then max
    // file size for persistent heap
    void *_addr = (void*)addr;

#ifdef _WIN32
    if (mem_release)
        if (VirtualFree(_addr,					// address of region
                        0,						// size of region
                        MEM_RELEASE 			// operation type
                       ) == 0)
           return 1;
#endif

    ph_file = uOpenFile(file_name, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (ph_file == U_INVALID_FD) return 2;

    ph_file_mapping = uCreateFileMapping(ph_file, 0, FixPersHeapFileMappingName(fm_name), NULL, __sys_call_error);
    if (U_INVALID_FILEMAPPING(ph_file_mapping)) return 3;

    if (should_map)
    {
        ph_start_address = uMapViewOfFile(ph_file_mapping, _addr, 0, 0, __sys_call_error);
        if (ph_start_address == NULL) return 4;
    }

    if (USemaphoreCreate(&ph_semaphore, 1, 1, sph_name, NULL, __sys_call_error) != 0) return 5;

    if (should_map)
    {
        ph_mb = (struct ph_masterblock*)ph_start_address;
        _vars = &(ph_mb->vars);
    }
    else 
    {
        ph_mb = NULL;
        _vars = NULL;
    }

    return 0;
}

int pers_close()
{
    if (USemaphoreRelease(ph_semaphore, __sys_call_error) != 0) return 1;

    if (ph_start_address)
    {
       if (uUnmapViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 2;
       ph_start_address = NULL;
    }

    if (uReleaseFileMapping(ph_file_mapping, NULL, __sys_call_error) != 0) return 3;

    if (uCloseFile(ph_file, __sys_call_error) == 0) return 4;

    return 0;
}

int pers_create(const char *file_name, const char *fm_name, const void *addr, int heap_size, USECURITY_ATTRIBUTES *sa)
{

    ph_file = uCreateFile(file_name, 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);
    if (ph_file == U_INVALID_FD) return 1;

    if (heap_size < HEAP) return 2;

    if (uSetEndOfFile(ph_file, (__int64)(heap_size + BLOCKSIZE), U_FILE_BEGIN, __sys_call_error) == 0) return 3;

    // Create file mapping
    ph_file_mapping = uCreateFileMapping(ph_file, 0, FixPersHeapFileMappingName(fm_name), sa, __sys_call_error);
    if (U_INVALID_FILEMAPPING(ph_file_mapping)) return 4;

    void *_addr = (void*)addr;
    ph_start_address = uMapViewOfFile(ph_file_mapping, _addr, 0, 0, __sys_call_error);
    if (ph_start_address == NULL) return 5;

    /* Fill master block */
    ph_mb = (struct ph_masterblock*)ph_start_address;
    ph_mb->vars.initialized = 0;
    ph_mb->low_boundary = (char*)ph_start_address + BLOCKSIZE;
    ph_mb->high_boundary = (char*)ph_start_address + heap_size + BLOCKSIZE;
    ph_mb->cur_pointer = ph_mb->low_boundary;

    // Close file mapping
    if (uUnmapViewOfFile(ph_file_mapping, ph_start_address, 0, __sys_call_error) != 0) return 6;

    if (uReleaseFileMapping(ph_file_mapping, FixPersHeapFileMappingName(fm_name), __sys_call_error) != 0) return 7;

    if (uCloseFile(ph_file, __sys_call_error) == 0) return 8;

    return 0;
}

void * pers_malloc(size_t size)
{
    if (USemaphoreDown(ph_semaphore, __sys_call_error) != 0) return NULL;

    void *ptr = _pers_malloc(size);

    if (USemaphoreUp(ph_semaphore, __sys_call_error) != 0) return NULL;

    return ptr;
}

void pers_free(void *ptr)
{
    if (USemaphoreDown(ph_semaphore, __sys_call_error) != 0) return;

    _pers_free(ptr);

    if (USemaphoreUp(ph_semaphore, __sys_call_error) != 0) return;
}

void * pers_realloc(void *ptr, size_t size)
{
    if (USemaphoreDown(ph_semaphore, __sys_call_error) != 0) return NULL;

    ptr = _pers_realloc(ptr, size);

    if (USemaphoreUp(ph_semaphore, __sys_call_error) != 0) return NULL;

    return ptr;
}
