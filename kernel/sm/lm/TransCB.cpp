/*
 * File:  TransCB.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "common/base.h"
#include "sm/lm/TransCB.h"
#include "common/errdbg/d_printf.h"

using namespace std;
/*****************************************************************************
 TransCB class implementation
 *****************************************************************************/

TransCB::TransCB(transaction_id _tr_id_)
{
    locks = NULL;
    wait = NULL;
    cycle = NULL;
    tr_id = _tr_id_;
    status = NORMAL_PROCESSING;
}

void TransCB::print()
{
    d_printf2("transaction_id=%d\n", tr_id);
  
    lock_request *request;

    for (request = locks; request != NULL; request = request->tran_next)
    {
        request->print();
        d_printf1("\n");
    }
}
