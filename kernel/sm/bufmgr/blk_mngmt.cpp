/*
 * File:  blk_mngmt.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include "common/sedna.h"
#include "common/commutil.h"
#include "common/xptr/sm_vmm_data.h"
#include "common/lfsGlobals.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/wu/wu.h"
#include "common/llcommon/llMain.h"
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

static
void FixSpan(
    int64_t *spanBeginOffsPtr, 
    int64_t *spanEndOffsPtr)
{
    *spanBeginOffsPtr = (*spanBeginOffsPtr + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE;
    *spanEndOffsPtr = *spanEndOffsPtr / PAGE_SIZE * PAGE_SIZE;
}

enum ConsumeType {CONSUME_NORMAL, CONSUME_REVERSE};

/* Iterate over blocks in file span, either in normal direction or in reverse. */ 
static
xptr ConsumeNextBlockInSpan(
    int64_t *spanBeginOffsPtr, 
    int64_t *spanEndOffsPtr,
    t_layer layerAdjustment,
    int64_t offsAdjustment,
    enum ConsumeType consumeType)
{
    int64_t offset;
    int64_t spanBeginOffs = *spanBeginOffsPtr;
    int64_t spanEndOffs = *spanEndOffsPtr;
    xptr result = XNULL;

    /* If have room for another block ... */ 
    if (spanEndOffs - spanBeginOffs >= PAGE_SIZE)
    {
        switch (consumeType)
        {
        case CONSUME_NORMAL:
            offset = spanBeginOffs;
            spanBeginOffs += PAGE_SIZE;
            break;

        case CONSUME_REVERSE:
            offset = spanEndOffs - PAGE_SIZE;
            spanEndOffs -= PAGE_SIZE;
        }

        U_ASSERT(layerAdjustment + (offset + offsAdjustment) / LAYER_ADDRESS_SPACE_SIZE <= INT32_MAX);

        result.layer =
            (t_layer)(layerAdjustment + (offset + offsAdjustment) / LAYER_ADDRESS_SPACE_SIZE);

        result.setOffs((lsize_t)((offset + offsAdjustment) % LAYER_ADDRESS_SPACE_SIZE));

        *spanBeginOffsPtr = spanBeginOffs;
        *spanEndOffsPtr = spanEndOffs;
    }

    return result;
}

/* Add blocks from the file span [spanBeginOffs, spanEndOffs) to
   free blocks stack. Parameters layerAdjustment and offsAdjustment
   control offsets conversion to xptrs (in pers free blocks stack 
   blocks are identified by xptr, not offset). 
   
   The basic idea behind add_file_span_to_pfb_stack() is to make
   file extension faster. One may achieve similar results with multiple
   calls to push_to_persistent_free_blocks_stack(). Due to the way
   almost every FS handles file expansion, free blocks order in the
   stack is *important* (see below). Multiple cals to
   push_to_persistent_free_blocks_stack() results in suboptimal order.
   
   Generaly, FS stores 2 values for each file - allocated size and valid
   data size -
         .<-------------allocated size--------------->.
   File: [DATA DATA DATA DATA DATA ???? ???? ???? ????]
         .<---valid data size--->. .<--no mans land-->.
                                        
   Allocated size is the overall file size. When application extends file
   the allocated size increases. FS allocates enough clusters to store entire
   file. The sum of all file clusters equals file's allocate size (rounded).
   Some of these clusters contain file data. Other ones contain random garbage.
   When application writes data to file, one or more clusters recieve file
   data. If application has never touched the particular cluster it stores
   garbage (chunks of deleted files or something). When file is extended
   newly appended tail is built from clusters containing garbage.

   Things are a bit different from read()/write() API point of view. Spec says
   that files are zero initialized - that is when application reads back a part
   of file that was never written it will see zeroes. Initializing file clusters
   with zeroes at file expansion time is not an option - it is slow. FS tracks
   whether particular cluster contains valid data instead and never reads back
   garbage.

   The real FS is a bit simpler than that. It assumes that file valid data is
   contigous and starts from file begin. Consider the figure again. FS stores
   two values - file allocated size and file valid data size. After contigous chunk
   of valid data there is no mans land (garbage). If one writes in the middle of no mans land,
   FS is forsed to reduce no man lands due to restriction on valid file data continuity.

         .<-------------allocated size--------------->.
   File: [DATA DATA DATA DATA DATA 0000 DATA ???? ????]
         .<--------valid data size-------->. .<--nml->.
   
   Writing in the middle of no mans land reduces no mans land. Part of no mans land
   that is now part of valid file data chunk must be zero-initialized. The optimal
   order of blocks in free blocks stack must match block order in the file. This
   guarantees that either no zero-initializing takes place or (due to randomness of
   buffers flushing) zero-initialized portion is small. 
   
   Function creates the following file layout:

   File: [<old data> <free stack> 1 2 3 4 5 ...]

   Here <old data> is file data prior to extension; <free stack> is an extra 
   set of blocks used to store xptr-s of new free blocks; <numbers> denote free 
   blocks (numbers match the order free blocks are fetched by 
   pop_from_persistent_free_blocks_stack()). */ 
static
void add_file_span_to_pfb_stack(
    xptr *hd, 
    int64_t spanBeginOffs, 
    int64_t spanEndOffs,
    t_layer layerAdjustment,
    int64_t offsAdjustment)
{
    xptr p = XNULL, stackP = XNULL;
    int64_t numFreeBlocks = 0;
    int64_t numFreeBlockSlots = 0;
    int64_t numStackBlocks = 0;
    int64_t stackSpanBeginOffs, stackSpanEndOffs;

    /* Make offsets aligned. */ 
    FixSpan(&spanBeginOffs, &spanEndOffs);

    /* Count total number of free blocks. */ 
    numFreeBlocks = (spanEndOffs - spanBeginOffs) / PAGE_SIZE;

    if (numFreeBlocks > 0)
    {
        /* Count number of free blocks current stack can accomodate 
           without growing. */ 
        if (*hd != XNULL)
        {
            numFreeBlockSlots = PERS_STACK_NODE_CAPACITY 
                - count_elems_of_persistent_free_blocks_stack(*hd, true);
        }

        /* Count number of blocks required to make stack large enough. */ 
        if (numFreeBlocks > numFreeBlockSlots)
        {
            numStackBlocks = 
                (numFreeBlocks - numFreeBlockSlots + PERS_STACK_NODE_CAPACITY)
                / (PERS_STACK_NODE_CAPACITY + 1);
        }

        /* Make a span for stack blocks allocation (reducing primary span). */ 
        stackSpanBeginOffs = spanBeginOffs;
        stackSpanEndOffs = stackSpanBeginOffs + numStackBlocks * PAGE_SIZE;
        spanBeginOffs = stackSpanEndOffs;

        while (1)
        {
            p = ConsumeNextBlockInSpan(
                    &spanBeginOffs,
                    &spanEndOffs,
                    layerAdjustment,
                    offsAdjustment,
                    CONSUME_REVERSE);

            /* All blocks processed? */ 
            if (p == XNULL) break;

            /* Should grow stack first? */ 
            if (0 != push_to_persistent_free_blocks_stack(hd, p, false))
            {
                stackP = ConsumeNextBlockInSpan(
                    &stackSpanBeginOffs,
                    &stackSpanEndOffs,
                    layerAdjustment,
                    offsAdjustment,
                    CONSUME_NORMAL);

                if (stackP == XNULL) throw SYSTEM_EXCEPTION("logic error");

                push_to_persistent_free_blocks_stack(hd, stackP, true);
                push_to_persistent_free_blocks_stack(hd, p, false);
            }
        }

        /* Ensure that we haven't lost anything. */ 
        while (1)
        {
            stackP = ConsumeNextBlockInSpan(
                    &stackSpanBeginOffs,
                    &stackSpanEndOffs,
                    layerAdjustment,
                    offsAdjustment,
                    CONSUME_NORMAL);

            if (stackP == XNULL) break;
            push_to_persistent_free_blocks_stack(hd, stackP, true);
        }
    }
}

int push_to_persistent_free_blocks_stack(xptr *hd, xptr p, bool canStoreFreeBlocks)
{
    //d_printf1("push_to_persistent_free_blocks_stack: begin\n");
    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;
    
    TIMESTAMP pers_ts = llGetPersTimestamp();

    if (*hd == XNULL)
    {
        if (!canStoreFreeBlocks) return 1;

        put_block_to_buffer(-1, p, &offs, false);
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
        if (!canStoreFreeBlocks) return 1;

        put_block_to_buffer(-1, p, &offs, false);
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
    if (*hd == XNULL) return 1;

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

int64_t count_elems_of_persistent_free_blocks_stack(xptr hd, bool examineHeadOnly)
{
    ramoffs offs = 0;
    free_blk_hdr *blk = NULL;

    int64_t  num = 0;
    while (hd != XNULL)
    {
        put_block_to_buffer(-1, hd, &offs);
        blk = (free_blk_hdr*)OFFS2ADDR(offs);

        num += blk->num;
        hd = blk->nblk;

        if (examineHeadOnly) break;
        num += 1;
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
    
    while (hd != XNULL && hd!=what && !found)
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

    if (*hd == XNULL)
    {
        xptr tmp;
        new_tmp_block(&tmp);
        put_block_to_buffer(-1, tmp, &offs, false);
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
        put_block_to_buffer(-1, tmp, &offs, false);
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
    if (*hd == XNULL) return 1;

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

static 
xptr
extend_file_helper(UFile fileHandle,
                   int64_t fileSizeCurrent,
                   int64_t fileSizeNew,
                   xptr freeStackHd,
                   t_layer layerAdjustment,
                   int64_t offsAdjustment,
                   bool initializeData)
{
    /* Does it make sense to initialize every block header? 
       I doubt since when a block is allocated it is never read back from HDD,
       instead header is initialized when block is put to buffer. */ 
    if (initializeData)
    {
        int64_t spanBeginOffs = fileSizeCurrent;
        int64_t spanEndOffs = fileSizeNew;
        int64_t offset;
        void *buf = NULL;
        vmm_sm_blk_hdr *hdr = NULL;
        unsigned int bytesOutCnt = 0;

        /* Allocate aligned buffer and initialize header struct. */ 
        buf = malloc(VMM_SM_BLK_HDR_MAX_SIZE * 2);
        if (!buf) throw std::bad_alloc();
        memset(buf, 0, VMM_SM_BLK_HDR_MAX_SIZE);

        hdr = (vmm_sm_blk_hdr *)ALIGN_PTR(buf, VMM_SM_BLK_HDR_MAX_SIZE);
        vmm_sm_blk_hdr::init(hdr);

        /* Make offsets aligned on block boundary. */ 
        FixSpan(&spanBeginOffs, &spanEndOffs);

        /* Visit every block. */ 
        while (1)
        {
            offset = spanBeginOffs;
            hdr->p = ConsumeNextBlockInSpan(
                    &spanBeginOffs, 
                    &spanEndOffs,
                    layerAdjustment,
                    offsAdjustment,
                    CONSUME_NORMAL);

            /* All done? */ 
            if (hdr->p == XNULL) break;

            /* Seek to offset. */ 
            if (!uSetFilePointer(
                    fileHandle,
                    offset,
                    NULL,
                    U_FILE_BEGIN,
                    __sys_call_error))
            {
                free(buf);
                throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");
            }

            /* Write header. */ 
            if (!uWriteFile(
                    fileHandle, 
                    hdr, 
                    VMM_SM_BLK_HDR_MAX_SIZE, 
                    &bytesOutCnt,
                    __sys_call_error) || bytesOutCnt!=VMM_SM_BLK_HDR_MAX_SIZE)
            {
                free(buf);
                throw SYSTEM_ENV_EXCEPTION("Cannot write to file");
            }
        }

        /* Dismiss buffer. */ 
        hdr = NULL;
        free(buf); buf = NULL; 
    }

    add_file_span_to_pfb_stack(
        &freeStackHd, 
        fileSizeCurrent, 
        fileSizeNew, 
        layerAdjustment, 
        offsAdjustment);

    return freeStackHd;
}

void extend_data_file(int extend_portion)
{
    int64_t fileSizeCurrent;
    int64_t fileSizeNew;

    fileSizeCurrent = mb->data_file_cur_size;
    fileSizeNew = fileSizeCurrent + (int64_t)extend_portion * PAGE_SIZE;

    /* if mb->data_file_max_size == 0 than size is unlimited */
    if (mb->data_file_max_size > 0 && 
        fileSizeNew > mb->data_file_max_size * PAGE_SIZE)
        throw USER_EXCEPTION(SE1011);
    
	llLogDecrease(fileSizeCurrent);
    
     /* Extend the file. */ 
    if (!uSetEndOfFile(
            data_file_handle, 
            fileSizeNew, 
            U_FILE_BEGIN, 
            __sys_call_error)) throw USER_EXCEPTION(SE1013);
    
    mb->data_file_cur_size = fileSizeNew;
   
    mb->free_data_blocks = extend_file_helper(
                                data_file_handle,
                                fileSizeCurrent,
                                fileSizeNew,
                                mb->free_data_blocks,
                                1,          /* 0-layer is reserved */
                                -PAGE_SIZE, /* since first block is master-block */
                                false);

    elog(EL_LOG, ("Data file has been extended, size: %" PRIx64 "", fileSizeNew));
    // !!! MASTER BLOCK HAS BEEN CHANGED
}


void extend_tmp_file(int extend_portion)
{
    int64_t fileSizeCurrent;
    int64_t fileSizeNew;

    fileSizeCurrent = mb->tmp_file_cur_size;
    fileSizeNew = fileSizeCurrent + (int64_t)extend_portion * PAGE_SIZE;

    /* if mb->data_file_max_size == 0 than size is unlimited */
    if (mb->tmp_file_max_size > 0 && 
        fileSizeNew > mb->tmp_file_max_size * PAGE_SIZE)
        throw USER_EXCEPTION(SE1012);

    /* Extend the file. */ 
    if (!uSetEndOfFile(
            tmp_file_handle, 
            fileSizeNew, 
            U_FILE_BEGIN, 
            __sys_call_error)) throw USER_EXCEPTION(SE1014);
    
    mb->tmp_file_cur_size = fileSizeNew;

    mb->free_tmp_blocks = extend_file_helper(
                                tmp_file_handle,
                                fileSizeCurrent,
                                fileSizeNew,
                                mb->free_tmp_blocks,
                                TMP_LAYER_STARTS_WITH,
                                0,
                                false);
    elog(EL_LOG, ("Temp file has been extended, size: %" PRIx64 "", fileSizeNew));

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
		
	res = buffer_table.find_remove(p, &offs);
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
		
	res = buffer_table.find_remove(p, &offs);
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

