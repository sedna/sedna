/*
 * File:  bm_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BM_FUNCTIONS_H
#define _BM_FUNCTIONS_H

#include "common/sedna.h"

#include "common/xptr/xptr.h"
#include "common/xptr/sm_vmm_data.h"

void bm_startup();

void bm_shutdown();

void bm_register_session(session_id sid, int is_rcv_mode);

void bm_unregister_session(session_id sid);

void bm_register_transaction(session_id sid, transaction_id trid);

void bm_unregister_transaction(session_id sid, transaction_id trid);

void bm_delete_tmp_blocks(session_id sid);

void bm_allocate_data_block(session_id sid,
                            xptr /*out*/ *p,
                            ramoffs /*out*/ *offs,
                            xptr /*out*/ *swapped);

void bm_allocate_tmp_block(session_id sid,
                           xptr /*out*/ *p,
                           ramoffs /*out*/ *offs,
                           xptr /*out*/ *swapped);

void bm_delete_block(session_id sid,
                     xptr p);

void bm_get_block(session_id sid,
                  xptr p,
                  ramoffs /*out*/ *offs,
                  xptr /*out*/ *swapped);

void bm_enter_exclusive_mode(session_id sid,
                             int *number_of_potentially_allocated_blocks);

void bm_exit_exclusive_mode(session_id sid);

void bm_memlock_block(session_id sid, xptr p);

void bm_memunlock_block(session_id sid, xptr p);

//void bm_pseudo_allocate_data_block(session_id sid,
//                                   xptr /*out*/ *p);

//void bm_pseudo_delete_data_block(session_id sid,
//                                 xptr p);

/// Debug functions
void bm_block_statistics(sm_blk_stat *stat);


#endif

