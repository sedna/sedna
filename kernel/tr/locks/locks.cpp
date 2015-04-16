/*
 * File:  locks.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <string.h>
#include <common/xptr/sm_vmm_data.h>

#include "tr/vmm/vmm.h"
#include "tr/locks/locks.h"
#include "common/lockmantypes.h"
//#include "common/utils.h" // TODO: why do we need it
#include "common/ssmmsg/SSMMsg.h"
#include "tr/tr_globals.h"
#include "common/errdbg/d_printf.h"
#include "tr/tr_common_funcs.h"


using namespace std;

bool is_stop_session();//declared in tr_functions.cpp

LocalLockMgr *local_lock_mrg;

bool is_init_lock_mgr = false;

void init_local_lock_mgr(SSMMsg* _sm_server_)
{
    local_lock_mrg = new LocalLockMgr();
    local_lock_mrg->Init_LocalLockMgr(_sm_server_);
    is_init_lock_mgr = true;
}

void set_tr_mode_lock_mgr(bool flag)
{
    if (is_init_lock_mgr)
    {
        local_lock_mrg->ro_mode(flag);
    }
}

void release_locks()
{
    if (is_init_lock_mgr)
    {
        local_lock_mrg->release();//release all locks acquired by transaction
    }
}

void release_resource(const char* name, resource_kind kind)
{
    if (is_init_lock_mgr)
    {
        local_lock_mrg->release_resource(name, kind);//release one resource
    }
}

void release_local_lock_mgr()
{
    if (is_init_lock_mgr == true)
    {
        local_lock_mrg->Release_LocalLockMgr();
        delete local_lock_mrg;
    }

    is_init_lock_mgr = false;
}

/****************************************************************************
 Local Manager Class implementation
 *****************************************************************************/


static char getCharFromResource(resource_kind kind)
{
    char c;

    switch (kind)
    {
        case LM_DOCUMENT:
            c = 'd';
            break;
        case LM_COLLECTION:
            c = 'c';
            break;
        case LM_INDEX:
            c = 'i';
            break;
        case LM_TRIGGER:
            c = 't';
            break;
        case LM_DATABASE:
            c = 'b';
            break;
        default:
            throw SYSTEM_EXCEPTION("Unexpected lock type");
    }

    return c;
}

// This needed for debug purposes only (elog).
static const char *getNameFromLockMode(lock_mode mode) {
    switch (mode)
    {
        case NULL_LOCK:
            return "Null lock";
        case lm_s:
            return "Shared (lm_s)";
        case lm_x:
            return "Exclusive (lm_x)";
        case lm_is:
            return "Intention shared (lm_is)";
        case lm_ix:
            return "Intention exclusive (lm_ix)";
        case lm_six:
            return "Shared and intention exclusive (lm_six)";
        default:
            throw SYSTEM_EXCEPTION("Unexpected lock mode");
    }
};

void LocalLockMgr::Init_LocalLockMgr(SSMMsg* _sm_server_)
{
    sm_server = _sm_server_;
}

void LocalLockMgr::Release_LocalLockMgr()
{
}

void LocalLockMgr::put_lock_on_document(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::databaseOptions.databaseName.c_str(), LM_DATABASE, true);
    obtain_lock(name, LM_DOCUMENT);
}

void LocalLockMgr::put_lock_on_collection(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::databaseOptions.databaseName.c_str(), LM_DATABASE, true);
    obtain_lock(name, LM_COLLECTION);
}

void LocalLockMgr::put_lock_on_index(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::databaseOptions.databaseName.c_str(), LM_DATABASE, true);
    obtain_lock(name, LM_INDEX);
}

void LocalLockMgr::put_lock_on_trigger(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::databaseOptions.databaseName.c_str(), LM_DATABASE, true);
    obtain_lock(name, LM_TRIGGER);
}

void LocalLockMgr::put_lock_on_db()
{
    if (tr_globals::databaseOptions.databaseName.length() > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::databaseOptions.databaseName.c_str(), LM_DATABASE);
}

void LocalLockMgr::obtain_lock(const char* name, resource_kind kind,
        bool intention_mode)
{
    if (tr_ro_mode && mode == lm_x) // cannot acquire eXclusive locks in RO-mode
        throw USER_EXCEPTION(SE4706);

    if (tr_ro_mode)
        return; // we don't need any locks for ro-transaction

    sm_msg_struct msg;

    msg.cmd = msg_lock_object;
    msg.trid = tr_globals::trid;
    msg.sid = tr_globals::sid;

// NOTE: TODO: need to check that code using locks doesn't rely on previous version assumptions
//             (that "intention" has more priority than the real lock mode)
    msg.data.lock.mode = mode;
    msg.data.lock.rkind = kind;

    strcpy(msg.data.lock.name, name);

    if (sm_server->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE3034);

    int result = msg.data.lock.result;
    elog(EL_DBG, ("[LTRK] Going to lock '%s' (%c) with mode=%d, result is %d",
            name,
            getCharFromResource(kind),
            getNameFromLockMode(mode),
            result));

    switch (result)
    {
        case '0':
        {
            d_printf1("Transaction is blocked\n");
            int res;
            for (;;)
            {
                res = USemaphoreDownTimeout(tr_globals::wait_sem, 1000, __sys_call_error);
                if (res == 0) //unblocked
                {
                    break;
                } else {// error
                    throw USER_EXCEPTION2(SE4015, "SEDNA_TRANSACTION_LOCK");
                }
            }            
            d_printf1("Transaction is unblocked\n");
            break;
        }
        case '1':
        {
            d_printf1("Transaction has got locks\n");
            break;
        }
        case '2':
            throw USER_EXCEPTION(SE4703);
        default:
        {
            d_printf2("Unknown reply from Lock Manager: %c\n", result);
            throw USER_EXCEPTION(SE4704);
            break;
        }
    }
    elog(EL_DBG, ("[LTRK] Resource '%s' (%c) has been locked with mode=%c.",
            name,
            getCharFromResource(msg.data.lock.rkind),
            getNameFromLockMode(mode)));
}

void LocalLockMgr::release()
{
    if (tr_ro_mode)
        return; // we don't need to release any locks for ro-transaction

    sm_msg_struct msg;
    msg.cmd = msg_release_locks;
    msg.trid = tr_globals::trid;

    if (sm_server->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE3034);

    elog(EL_DBG, ("[LTRK] All resources have been released."));
}

void LocalLockMgr::release_resource(const char* name, resource_kind kind)
{
    if (tr_ro_mode)
        return; // we don't need to release any locks for ro-transaction

    sm_msg_struct msg;
    msg.cmd = msg_unlock_object;
    msg.trid = tr_globals::trid;

    msg.data.lock.rkind = kind;

    strcpy(msg.data.lock.name, name);

    if (sm_server->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE3034);

    elog(EL_DBG, ("[LTRK] Resource '%s' (%c) has been released.", name, getCharFromResource(kind)));
}
