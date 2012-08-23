/*
 * File:  trans_table.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TRANS_TABLE_H_
#define _TRANS_TABLE_H_

#include "common/sedna.h"
#include <map>
#include "common/base.h"
#include "u/usem.h"
#include "sm/lm/TransCB.h"
#include "common/lockmantypes.h"

class tr_lock_head
{
public:
    TransCB *tran;

    tr_lock_head();
    tr_lock_head(TransCB*);
    ~tr_lock_head();
    void print();
};

typedef std::pair<transaction_id, tr_lock_head*> tr_pair;
typedef std::map<transaction_id, tr_lock_head*>::iterator Trans_It;

class trans_table
{
    friend bool lock_table::deadlock(transaction_id trid, bool sync);
private:
    std::map<transaction_id, tr_lock_head*> _tr_table_;
public:
    //returns NULL if there is no such transaction in trans_table
    tr_lock_head* find_tr_lock_head(transaction_id);
    int insert_tr_lock_head(tr_pair);
    //returns the number of removed elements
    size_t remove_tr(transaction_id);
    void print();
    size_t get_trns_num() const
    {
        return _tr_table_.size();
    }
};

#endif
