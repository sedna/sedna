/*
 * File:  TransCB.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TRANS_CB_H_
#define _TRANS_CB_H_

#include "common/sedna.h"
#include "common/base.h"
#include "common/lockmantypes.h"

#include "sm/lm/lock_table.h"

class lock_request;

enum
{
    NORMAL_PROCESSING, ROLLING_BACK_AFTER_DEADLOCK
};

class TransCB
{
public:
    lock_request* locks; //anchor of transaction lock list
    lock_request* wait; //lock waited for by this transaction (or NULL)
    TransCB* cycle; //used by deadlock detector
    transaction_id tr_id; // the identifier of transaction
    int status; //transaction status
  
    TransCB(transaction_id _tr_id_);
    void print();
};

#endif /* _TRANS_CB_H_ */
