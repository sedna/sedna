/*
 * File:  locks.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LOCKS_H
#define _LOCKS_H

#include "common/sedna.h"
#include <list>

#include "common/lm_base.h"
#include "u/usem.h"
#include "common/SSMMsg.h"


void init_local_lock_mgr(SSMMsg* _sm_server_);

void set_tr_mode_lock_mgr(bool flag);

void release_local_lock_mgr();

void release_locks();

void release_resource(const char* name, resource_kind kind);

//typedef std::list<resource_id>::iterator ridl_it;

class LocalLockMgr
{
private:
    //std::list<resource_id> resource_l;
    bool tr_ro_mode; // is transaction in RO-mode?
    lock_mode mode;
    SSMMsg *sm_server;
    void obtain_lock(const char* name, resource_kind kind, bool intention_mode = false);
public:
    void Init_LocalLockMgr(SSMMsg* _sm_server_);
    void Release_LocalLockMgr();
    void put_lock_on_document(const char *name);
    void put_lock_on_collection(const char *name);
    void put_lock_on_index(const char *name);
    void put_lock_on_trigger(const char *name);
    void put_lock_on_db();
    void lock(lock_mode _mode_) {mode = _mode_;};
    void ro_mode(bool flag) {tr_ro_mode = flag;};
    lock_mode get_cur_lock_mode() {return mode;};

    //void lock(const char* name, resource_kind kind, lock_mode mode) throw (LockMgrException);
    //void release(const char* name, resource_kind kind) throw (LockMgrException);
    void release();
    void release_resource(const char* name, resource_kind kind);
};

extern LocalLockMgr *local_lock_mrg;

#endif

