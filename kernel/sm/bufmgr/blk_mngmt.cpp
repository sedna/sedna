/*
 * File:  blk_mngmt.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include "common/sedna.h"
#include "common/sm_vmm_data.h"
#include "common/lfsGlobals.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/wu/wu.h"
#include "sm/llsm/llMain.h"
#include "sm/llsm/physlog.h"

/*******************************************************************************
********************************************************************************
  LOW LEVEL FUNCTIONS FOR MANAGING A STACK OF FREE BLOCKS
********************************************************************************
*******************************************************************************/
struct free_blk_hdr 
{
    vmm_sm_blk_hdr sm_vmm;	/* sm/vmm parameters */
    xptr nblk;				/* next block */
    int num;				/* number of free block addresses stored */
    TIMESTAMP ts;

	static void init(void *p);
};

/* Number of pointers a single block can store. */ 
#define PERS_STACK_NODE_CAPACITY \
    (((size_t)PAGE_SIZE-sizeof(free_blk_hdr))/sizeof(xptr))

static xptr *get_xptr_array(free_blk_hdr *h)
{
    return (xptr *)(h+1);
}

void free_blk_hdr::init(void *p)
{
    free_blk_hdr *hdr = (free_blk_hdr*)p;
    hdr->nblk = XNULL;
	hdr->num = 0;
	WuGetTimestamp(&(hdr->ts));
}

int push_to_persistent_free_blocks_stack(xptr *hd, xptr p)
{
    //d_printf1("push_to_persistent_free_blocks_stack: begin\n");
    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;
    
    TIMESTAMP pers_ts = llGetPersTimestamp();

    if (*hd == NULL)
    {
        put_block_to_buffer(-1, p, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        if (IS_DATA_BLOCK_LP(blk)) llLogFreeBlocksInfo(p, (void *)blk, PAGE_SIZE);

        free_blk_hdr::init(blk);
        *hd = p;

        blk->sm_vmm.is_changed = true;

        return 0;
    }

    put_block_to_buffer(-1, *hd, &offs);
    blk = (free_blk_hdr*)OFFS2ADDR(offs);

    if (blk->num >= (PAGE_SIZE - (int)sizeof(free_blk_hdr)) / ((int)sizeof(xptr)))
    {
        put_block_to_buffer(-1, p, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        if (IS_DATA_BLOCK_LP(blk)) llLogFreeBlocksInfo(p, (void *)blk, PAGE_SIZE);

        free_blk_hdr::init(blk);
        blk->nblk = *hd;
        *hd = p;
    }
    else
    {
        if (IS_DATA_BLOCK_LP(blk))
        {
        	if (blk->ts < pers_ts)
        		llLogFreeBlocksInfo(*hd, (void *)blk, PAGE_SIZE);
			WuGetTimestamp(&(blk->ts));
		}

        blk->num++;

        *(xptr*)((char*)blk + (PAGE_SIZE - blk->num * sizeof(xptr))) = p;
    }

    blk->sm_vmm.is_changed = true;

    return 0;
}

int pop_from_persistent_free_blocks_stack(xptr *hd, xptr *p)
{
    if (*hd == NULL) return 1;

    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;
    
    TIMESTAMP pers_ts = llGetPersTimestamp();

    put_block_to_buffer(-1, *hd, &offs);
    blk = (free_blk_hdr*)OFFS2ADDR(offs);
    //if (blk->sm_vmm.p == NULL) d_printf1("!!!pop_from_persistent_free_blocks_stack(xptr *hd, xptr *p) 1\n");

    if (blk->num == 0)
    {
        if (IS_DATA_BLOCK_LP(blk))
        {
        	if (blk->ts < pers_ts)
        		llLogFreeBlocksInfo(*hd, (void *)blk, PAGE_SIZE);
			WuGetTimestamp(&(blk->ts));
		}

        xptr tmp = blk->nblk;
        *p = *hd;
        *hd = tmp;
    }
    else 
    {
        *p = *(xptr*)((char*)blk + (PAGE_SIZE - blk->num * sizeof(xptr)));

        if (IS_DATA_BLOCK_LP(blk))
        {
        	if (blk->ts < pers_ts)
        		llLogFreeBlocksInfo(*hd, (void *)blk, PAGE_SIZE);
			WuGetTimestamp(&(blk->ts));
		}

        blk->num--;

        blk->sm_vmm.is_changed = true;
    }
    
    return 0;
}

int count_elems_of_persistent_free_blocks_stack(xptr hd)
{
    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;

    int num = 0;
    while (hd != NULL)
    {
        put_block_to_buffer(-1, hd, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        num += blk->num + 1;
        hd = blk->nblk;
    }

    return num;
}

bool is_in_persistent_free_blocks_stack(xptr hd, xptr what)
{
    bool           found = false;
    int            i;
    ramoffs        offs;
    free_blk_hdr  *node;
    xptr          *nodeXptrArray;
    
    while (hd!=NULL && hd!=what && !found)
    {
        put_block_to_buffer(-1, hd, &offs);
        node = (free_blk_hdr *)OFFS2ADDR(offs);
        nodeXptrArray = get_xptr_array(node);

        for (i=0; i<node->num && !found; ++i)
        {
            found = (nodeXptrArray[PERS_STACK_NODE_CAPACITY-1-i] == what);
        }

        hd = node->nblk;
    }
    return hd == what || found;
}

int push_to_persistent_used_blocks_stack(xptr *hd, xptr p)
{
    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;

    if (*hd == NULL)
    {
        xptr tmp;
        new_tmp_block(&tmp);
        put_block_to_buffer(-1, tmp, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        free_blk_hdr::init(blk);
        *hd = tmp;
    }
    else
    {
        put_block_to_buffer(-1, *hd, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);
    }

    if (blk->num >= (PAGE_SIZE - (int)sizeof(free_blk_hdr)) / ((int)sizeof (xptr)))
    {
        xptr tmp;
        new_tmp_block(&tmp);
        put_block_to_buffer(-1, tmp, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        free_blk_hdr::init(blk);
        blk->nblk = *hd;
        *hd = tmp;
    }

    blk->num++;

    *(xptr*)((char*)blk + (PAGE_SIZE - blk->num * sizeof(xptr))) = p;

    blk->sm_vmm.is_changed = true;

    return 0;
}

int pop_from_persistent_used_blocks_stack(xptr *hd, xptr *p)
{
    if (*hd == NULL) return 1;

    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;

    put_block_to_buffer(-1, *hd, &offs);
    blk = (free_blk_hdr*)OFFS2ADDR(offs);

    if (blk->num == 0)
    {
        xptr tmp = blk->nblk;
        delete_tmp_block(*hd, NULL);
        *hd = tmp;
        return pop_from_persistent_used_blocks_stack(hd, p);
    }
    else 
    {
        *p = *(xptr*)((char*)blk + (PAGE_SIZE - blk->num * sizeof(xptr)));

        blk->num--;

        blk->sm_vmm.is_changed = true;
    }
    
    return 0;
}



/*******************************************************************************
********************************************************************************
  FUNCTIONS FOR EXTENDING DATA SPACE
********************************************************************************
*******************************************************************************/
void extend_data_file(int extend_portion) throw (SednaException)
{
    if (mb->data_file_cur_size + 
        (__int64)extend_portion * 
        (__int64)PAGE_SIZE > mb->data_file_max_size + (__int64)PAGE_SIZE)
        throw USER_EXCEPTION(SE1011);
    
	llLogDecrease(mb->data_file_cur_size);
    
    int res = 0;
    __int64 dsk_offs = 0;

    dsk_offs = (__int64)extend_portion * (__int64)PAGE_SIZE;
    if (uSetEndOfFile(data_file_handler, dsk_offs, U_FILE_END, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE1013);

    __int64 data_file_old_size = mb->data_file_cur_size;
    mb->data_file_cur_size += (__int64)extend_portion * (__int64)PAGE_SIZE;
    xptr xptr_cursor = DATA_FILE_OFFS2XPTR(data_file_old_size - (__int64)PAGE_SIZE);
    
    if (uSetFilePointer(data_file_handler, data_file_old_size, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");
    
    int i = 0;
    system_data_aligned_ptr = (char*)(((__uint32)system_data_buf + VMM_SM_BLK_HDR_MAX_SIZE) / VMM_SM_BLK_HDR_MAX_SIZE * VMM_SM_BLK_HDR_MAX_SIZE);
    vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)system_data_aligned_ptr;
    vmm_sm_blk_hdr::init(hdr);
    
    for (i = 0; i < extend_portion; i++)
    {
        // fill header of the block
        hdr->p = xptr_cursor;
        int number_of_bytes_written = 0;
        
        res = uWriteFile(data_file_handler, hdr, VMM_SM_BLK_HDR_MAX_SIZE, &number_of_bytes_written, __sys_call_error);
        
        if (res == 0 || number_of_bytes_written != VMM_SM_BLK_HDR_MAX_SIZE)
            throw SYSTEM_ENV_EXCEPTION("Cannot write to file");
        
        // put to the list of free blocks
        push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), xptr_cursor);
        
        xptr_cursor += PAGE_SIZE;
        
        dsk_offs = data_file_old_size + (__int64)(i + 1) * (__int64)PAGE_SIZE;
        if (uSetFilePointer(data_file_handler, dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
            throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");
    }

    // !!! MASTER BLOCK HAS BEEN CHANGED
}


void extend_tmp_file(int extend_portion) throw (SednaException)
{
    if (mb->tmp_file_cur_size + 
        (__int64)extend_portion * 
        (__int64)PAGE_SIZE > mb->tmp_file_max_size + (__int64)PAGE_SIZE)
        throw USER_EXCEPTION(SE1012);

    int res = 0;
    __int64 dsk_offs = 0;

    dsk_offs = (__int64)extend_portion * (__int64)PAGE_SIZE;
    if (uSetEndOfFile(tmp_file_handler, dsk_offs, U_FILE_END, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE1014);

    __int64 tmp_file_old_size = mb->tmp_file_cur_size;
    mb->tmp_file_cur_size += (__int64)extend_portion * (__int64)PAGE_SIZE;

    xptr xptr_cursor = TMP_FILE_OFFS2XPTR(tmp_file_old_size);

    if (uSetFilePointer(tmp_file_handler, tmp_file_old_size, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

	int i = 0;
    system_data_aligned_ptr = (char*)(((__uint32)system_data_buf + VMM_SM_BLK_HDR_MAX_SIZE) / VMM_SM_BLK_HDR_MAX_SIZE * VMM_SM_BLK_HDR_MAX_SIZE);
    vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)system_data_aligned_ptr;
	vmm_sm_blk_hdr::init(hdr);

    for (i = 0; i < extend_portion; i++)
    {
        // fill header of the block
        hdr->p = xptr_cursor;
        //d_printf1("xptr_cursor: "); xptr_cursor.print();
        int number_of_bytes_written = 0;

        res = uWriteFile(tmp_file_handler, hdr, VMM_SM_BLK_HDR_MAX_SIZE, &number_of_bytes_written, __sys_call_error);
        if (res == 0 || number_of_bytes_written != VMM_SM_BLK_HDR_MAX_SIZE)
            throw SYSTEM_ENV_EXCEPTION("Cannot write to file");


        // put to the list of free blocks
        push_to_persistent_free_blocks_stack(&(mb->free_tmp_blocks), xptr_cursor);

        xptr_cursor += PAGE_SIZE;

        dsk_offs = tmp_file_old_size + (__int64)(i + 1) * (__int64)PAGE_SIZE;
        if (uSetFilePointer(tmp_file_handler, dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
            throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");
    }

    // !!! MASTER BLOCK HAS BEEN CHANGED
}



/*******************************************************************************
********************************************************************************
  FREE BLOCK MANAGEMENT FUNCTIONS
********************************************************************************
*******************************************************************************/
void new_data_block(xptr /*out*/ *p)
{
    int res = pop_from_persistent_free_blocks_stack(&(mb->free_data_blocks), p);

    if (res != 0) 
    {
        extend_data_file(mb->data_file_extending_portion);
        res = pop_from_persistent_free_blocks_stack(&(mb->free_data_blocks), p);
        if (res != 0) throw SYSTEM_EXCEPTION("Error in logic of extending data file");
    }

    // !!! MASTER BLOCK HAS BEEN CHANGED
}

void delete_data_block(const xptr &p)
{
	int res = 0;
	ramoffs offs;
		
	res = buffer_table.find_remove(p, offs);
	if (res == 0) 
	{
		used_mem.find_remove(offs);
		free_mem.push(offs);
	}

    push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), p);

    // !!! MASTER BLOCK HAS BEEN CHANGED
}

void new_tmp_block(xptr /*out*/ *p)
{
    int res = pop_from_persistent_free_blocks_stack(&(mb->free_tmp_blocks), p);

    if (res != 0) 
    {
        extend_tmp_file(mb->tmp_file_extending_portion);
        res = pop_from_persistent_free_blocks_stack(&(mb->free_tmp_blocks), p);
        if (res != 0) throw SYSTEM_EXCEPTION("Error in logic of extending tmp file");
    }

    // !!! MASTER BLOCK HAS BEEN CHANGED
}

void delete_tmp_block(const xptr &p, tr_info *info)
{
	int res = 0;
	ramoffs offs;
		
	res = buffer_table.find_remove(p, offs);
	if (res == 0) 
	{
        /// callback to trn to unmap block
        if (info)
            unmap_block_in_tr(p, info, true);
        ////////////////////////////////////

		used_mem.find_remove(offs);
		free_mem.push(offs);
	}

    push_to_persistent_free_blocks_stack(&(mb->free_tmp_blocks), p);

    // !!! MASTER BLOCK HAS BEEN CHANGED
}

