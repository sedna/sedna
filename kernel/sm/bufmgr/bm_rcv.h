/*
 * File:  bm_rcv.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BM_RCV_H
#define _BM_RCV_H

#include "xptr.h"

void bm_rcv_init();

void bm_rcv_release();

void bm_rcv_change(const xptr& xaddr, const void *p, shft size, __int64 file_size = 0);

void bm_rcv_decrease(__int64 old_size);

void bm_rcv_master_block(const void* p);

void bm_rcv_tmp_file();

void bm_rcv_ph(bool ph_bu_to_ph = true);

//void bm_rcv_create_node_blk(const xptr& blk);

/******************************************************************************
  Database recovery plan:
      1. call bm_rcv_init()
      2. call bm_rcv_ph()
      3.  +->-+
          |   | call bm_rcv_change()
          |   | call bm_rcv_decrease()
          |   | call bm_rcv_master_block()
          +-<-+
      4. call bm_rcv_release()
      5. bm_rcv_tmp_file()

******************************************************************************/



#endif
