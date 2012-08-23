/*
 * File:  lock_table.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LOCK_TABLE_H_
#define _LOCK_TABLE_H_

#include "common/sedna.h"
#include "common/base.h"
#include "common/lockmantypes.h"

#include "u/umutex.h"
#include "u/usem.h"

#include "sm/lm/TransCB.h"

#include <string>
#include <map>

bool lock_compat(lock_mode, lock_mode);
lock_mode lock_max(lock_mode, lock_mode);

class lock_head;
class lock_request;
class TransCB;

class lock_head
{
public:
    resource_id r_id; //the id of the locking resource
    lock_request* queue; // the queue of requests for this lock
    lock_mode granted_mode; // the mode of the granted group
    bool waiting;// flag indicates nonempty wait group

    lock_head(resource_id, lock_mode);
    ~lock_head();
    void print();
};

class lock_request
{
public:
    lock_request* queue; //pointer to next request in lock queue
    lock_head* head;//pointer back to head of the queue
    lock_status status; //granted, waiting, converting, denied
    lock_mode mode; //mode requested
    lock_mode convert_mode; // if in convert wait, mode desired
    int count; //count of the number of times lock was locked
    lock_class class_; //class in which lock is held (lock duration)
    USemaphore process_xsem;//sem to up when lock is granted
    TransCB* tran; //pointer to transaction record
    lock_request* tran_prev;// previous lock request in transaction list
    lock_request* tran_next;// next lock request in transaction list

    lock_request(transaction_id, session_id, lock_head*, lock_mode, lock_class);
    ~lock_request();
    void print();
};

typedef std::pair<std::string, lock_head*> lock_pair;
typedef std::map<std::string, lock_head*>::iterator lock_iter;

class lock_table
{
private:
    uMutexType xsem; //exclusive mutex protecting hash table
    std::map<std::string, lock_head*> _l_table_;
    // the auxilary functions
    lock_head* find_lock(resource_id&, bool sync = true);

    // visited flags; we use it to look for nodes already visited during graph traversal
    transaction_id nodesVisited[CHARISMA_MAX_TRNS_NUMBER];
    unsigned int nVisited;

public:
    void init_lock_table();
    void release_lock_table();
    // the main functions
    lock_reply lock(transaction_id, session_id, resource_id, lock_mode,
            lock_class, long, bool sync = true);

    lock_reply unlock(transaction_id, resource_id, bool sync = true);
    lock_reply release_tr_locks(transaction_id, bool sync = true);
    void print(bool sync = true);

    bool deadlock(transaction_id trid, bool sync);
    void visit(TransCB* me);

    inline void down_sem(bool sync)
    {
        if (sync)
            if (uMutexLock(&xsem, __sys_call_error) != 0)
                throw SYSTEM_EXCEPTION("cannot obtain lock table sync mutex");
    }

    inline void up_sem(bool sync)
    {
        if (sync)
            if (uMutexUnlock(&xsem, __sys_call_error) != 0)
                throw SYSTEM_EXCEPTION("cannot unlock lock table sync mutex");
    }
};

#endif
