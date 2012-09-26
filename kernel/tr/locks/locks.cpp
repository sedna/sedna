/*
 * File:  locks.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <string.h>

#include "tr/vmm/vmm.h"
#include "tr/locks/locks.h"
#include "common/lm_base.h"
#include "common/utils.h"
#include "common/SSMMsg.h"
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

    obtain_lock(tr_globals::db_name, LM_DATABASE, true);
    obtain_lock(name, LM_DOCUMENT);
}

void LocalLockMgr::put_lock_on_collection(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::db_name, LM_DATABASE, true);
    obtain_lock(name, LM_COLLECTION);
}

void LocalLockMgr::put_lock_on_index(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::db_name, LM_DATABASE, true);
    obtain_lock(name, LM_INDEX);
}

void LocalLockMgr::put_lock_on_trigger(const char *name)
{
    if (strlen(name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::db_name, LM_DATABASE, true);
    obtain_lock(name, LM_TRIGGER);
}

void LocalLockMgr::put_lock_on_db()
{
    if (strlen(tr_globals::db_name) > (MAX_RESOURCE_NAME_LENGTH - 1))
        throw USER_EXCEPTION(SE4702);

    obtain_lock(tr_globals::db_name, LM_DATABASE);
}

void LocalLockMgr::obtain_lock(const char* name, resource_kind kind,
        bool intention_mode)
{
    if (tr_ro_mode && mode == lm_x) // cannot acquire eXclusive locks in RO-mode
        throw USER_EXCEPTION(SE4706);

    if (tr_ro_mode)
        return; // we don't need any locks for ro-transaction

    sm_msg_struct msg;

    msg.cmd = 3;
    msg.trid = tr_globals::trid;
    msg.sid = tr_globals::sid;

    if (intention_mode == false)
        msg.data.data[0] = ((mode == lm_s) ? 's' : 'x');
    else
        msg.data.data[0] = ((mode == lm_s) ? 'r' : 'w'); //'r' intention read; 'w' intention write

    msg.data.data[1] = getCharFromResource(kind);

    strcpy((msg.data.data) + 2, name);

    if (sm_server->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE3034);

    int result = msg.data.data[0];
    if (intention_mode == false)
        elog(EL_DBG, ("[LTRK] Going to lock '%s' (%c) with mode=%c, result is %d", name, msg.data.data[1], ((mode == 1) ? 's': 'x'), result));
    else
        elog(EL_DBG, ("[LTRK] Going to lock '%s' (%c) with intention mode=%s, result is %d", name, msg.data.data[1], ((mode == 1) ? "is" : "ix"), result));

    switch (msg.data.data[0])
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
            d_printf2("Unknown reply from Lock Manager: %c\n", msg.data.data[0]);
            throw USER_EXCEPTION(SE4704);
            break;
        }
    }
    if (intention_mode == false)
        elog(EL_DBG, ("[LTRK] Resource '%s' (%c) has been locked with mode=%c.", name, msg.data.data[1], ((mode == 1) ? 's': 'x')));
    else
        elog(EL_DBG, ("[LTRK] Resource '%s' (%c) has been locked with intention mode=%s.", name, msg.data.data[1], ((mode == 1) ? "is" : "ix")));

}

void LocalLockMgr::release()
{
    if (tr_ro_mode)
        return; // we don't need to release any locks for ro-transaction

    sm_msg_struct msg;
    msg.cmd = 4;
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
    msg.cmd = 5;
    msg.trid = tr_globals::trid;

    msg.data.data[1] = getCharFromResource(kind);

    strcpy((msg.data.data) + 2, name);

    if (sm_server->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE3034);

    elog(EL_DBG, ("[LTRK] Resource '%s' (%c) has been released.", name, msg.data.data[1]));
}
