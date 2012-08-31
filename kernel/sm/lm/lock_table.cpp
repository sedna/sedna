/*
 * File:  lock_table.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/lockmantypes.h"

#include "u/usem.h"
#include "u/ugnames.h"

#include "sm/lm/lm_globals.h"
#include "sm/lm/lock_table.h"
#include "sm/lm/trans_table.h"

#include "sm/smtypes.h"

#include <iostream>
#include <string>

using namespace std;

/******************************************************************************
 resource_id class friends
 ******************************************************************************/

void print_resource_id(std::string r_id)
{
    d_printf2("%s", r_id.c_str());
}

/******************************************************************************
 lock_request class implementation
 ******************************************************************************/

lock_request::lock_request(transaction_id tr_id,
                           session_id s_id,
                           lock_head* l_h,
                           lock_mode m,
                           lock_class c)
{
    queue = NULL;
    status = LOCK_GRANTED;
    head = l_h;
    mode = m;
    convert_mode = NULL_LOCK;
    count = 1;
    class_ = c;

    {
        /* open process_sem */
        /* TODO: prepare all sems before creation */
        SessionIdString sidstr(s_id);
        CHECK_ENV(USemaphoreOpen(&process_xsem, transactionLockName, __sys_call_error), SE4012, transactionLockName.name);
    }

    //maintain the transaction lock list
    tr_lock_head* tr_l_h = tr_table.find_tr_lock_head(tr_id);

    if (tr_l_h == NULL)
    {
        TransCB* trCB = new TransCB(tr_id);
        trCB->locks = this;

        tr_pair
                tr_p(tr_id, new tr_lock_head(trCB)/*pointer to new tr_lock_head*/);

        tr_table.insert_tr_lock_head(tr_p);

        tran = trCB;
        tran_prev = NULL;
        tran_next = NULL;
    }
    else
    {
        tran = tr_l_h->tran;

        lock_request *r, *last=NULL;

        for (r = tr_l_h->tran->locks; r != NULL; r = r->tran_next)
        {
            last = r;
        }

        tran_prev = last;
        tran_next = NULL;
        last->tran_next = this;
    }
}

lock_request::~lock_request()
{
    if (USemaphoreClose(process_xsem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4013, "SEDNA_TRANSACTION_LOCK");

    tr_lock_head* tr_l_h = tr_table.find_tr_lock_head(tran->tr_id);

    if (tr_l_h == NULL)
        throw USER_EXCEPTION2(SE4701, "There is lock_request which does not belong to any transaction");

    if (tran_prev == NULL)//the case when the first request in transaction list
    {
        if (tran_next == NULL)
        {
            tran->locks = NULL;
            tr_table.remove_tr(tran->tr_id);
        }
        else
        {
            tran->locks = tran_next;
            tran->locks->tran_prev = NULL;
        }

    }
    else
    {
        tran_prev->tran_next = tran_next;
    }
}

void lock_request::print()
{
    d_printf3("lock_status=%d, lock_mode=%d", status, mode);
    d_printf3(", convert_mode=%d, count=%d", convert_mode, count);
    d_printf2(", lock_class=%d", class_);
    d_printf2(", resource_id=%s", head->r_id.get_res_name().c_str());
    d_printf2(", transaction_id=%d\n", tran->tr_id);
}

/*****************************************************************************
 lock_head class implementation
 ******************************************************************************/

lock_head::lock_head(resource_id _r_id_, lock_mode _m_)
{
    r_id = _r_id_;
    granted_mode = _m_;
    queue = NULL;
    waiting = false;
}

lock_head::~lock_head()
{
}

void lock_head::print()
{
    d_printf3("group_mode=%d, waiting=%d\n", granted_mode, waiting);
    lock_request *it;
    for (it = queue; it != NULL; it = it->queue)
    {
        it->print();
        d_printf1("\n");
    }
}

/******************************************************************************
 lock_table class implementation
 ******************************************************************************/

void lock_table::init_lock_table()
{
    if (uMutexInit(&xsem, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot init lock table sync mutex");
}

void lock_table::release_lock_table()
{
    if (uMutexDestroy(&xsem, __sys_call_error) != 0)
        throw SYSTEM_EXCEPTION("cannot release lock table sync mutex");
}

//returns NULL if r_id is not found
lock_head* lock_table::find_lock(resource_id &r_id, bool sync)
{
    down_sem(sync);

    lock_head* ret;

    lock_iter it = _l_table_.find(r_id.get_str_res_id());

    if (it == _l_table_.end())
        ret = NULL;
    else
        ret = it->second;

    return ret;

    up_sem(sync);
}

/////////////////////////////////////////////////////////////////////////////
////////////////////// lock method///////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


lock_reply lock_table::lock(transaction_id tr_id, session_id s_id,
        resource_id r_id, lock_mode mode, lock_class class_, long timeout,
        bool sync)
{
    lock_reply ret_code;
    lock_head * lock;
    lock_request *request, *last=NULL, *convert_request;
    transaction_id me = tr_id;

    down_sem(sync);

    //the whole database lock case//
    //////////////////////////////

    lock = find_lock(r_id, false);

    // lock is free case
    if (lock == NULL)
    {
        lock = new lock_head(r_id, mode);
        lock->queue = new lock_request(tr_id, s_id, lock, mode, class_);

        lock_pair p(r_id.get_str_res_id(), lock);
        _l_table_.insert(p);

        ret_code = LOCK_OK;
        goto end;
    }

    //lock not free case
    for (request = lock->queue; request != NULL; request = request->queue)
    {
        if (request->tran->tr_id == me)
            break;

        last = request;
    }

    // FIFO sheduling for new request
    if (request == NULL)
    { // a new request for this resource by this transaction
        request = new lock_request(me, s_id, lock, mode, class_);

        last->queue = request;

        if (!(lock->waiting) && lock_compat(mode, lock->granted_mode))
        {//new request compatible and no other waiters

            lock->granted_mode = lock_max(mode, lock->granted_mode);

            ret_code = LOCK_OK;
            goto end;
        }
        else
        {//new request must wait
            lock->waiting = true;
            request->status = LOCK_WAITING;
            request->tran->wait = request;

            ret_code = LOCK_NOT_LOCKED;
            goto end;
        }
    }
    else
    {//re-request for this transaction (conversion case)
        lock_mode convert_mode = lock_max(request->mode, mode);

        convert_request = request;

        for (request = lock->queue; request != NULL; request = request->queue)
        {

            if ((request->status == LOCK_GRANTED || request->status
                    == LOCK_CONVERTING) && request->tran->tr_id
                    != convert_request->tran->tr_id && !lock_compat(
                    request->mode, convert_mode))
            {
                convert_request->convert_mode = convert_mode;
                convert_request->status = LOCK_CONVERTING;
                convert_request->tran->wait = convert_request;

                lock->waiting = true;//thus, no locks could be added to the granted group

                ret_code = LOCK_NOT_LOCKED;
                goto end;
            }
        }

        //convert_lock is compatible with each granted lock of other transactions
        convert_request->mode = convert_mode;
        convert_request->status = LOCK_GRANTED;
        convert_request->convert_mode = NULL_LOCK;

        lock->granted_mode = lock_max(lock->granted_mode, convert_mode);

        ret_code = LOCK_OK;
        goto end;
    }

    end:

    up_sem(sync);
    return ret_code;
}

/////////////////////////////////////////////////////////////////////////////
//////////////////// unlock method //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


lock_reply lock_table::unlock(transaction_id tr_id, resource_id r_id, bool sync)
{
    lock_head *lock = NULL;
    lock_request *request;
    lock_request *prev_req = NULL;
    transaction_id me = tr_id;

    down_sem(sync);

    //find the requestor's request
    lock = find_lock(r_id, false);

    if (lock == NULL)
    {
        up_sem(sync);
        return LOCK_OK;
    }

    for (request = lock->queue; request != NULL; request = request->queue)
    {
        if (request->tran->tr_id == me)
            break;
        prev_req = request;
    }

    if (request == NULL)
    {
        up_sem(sync);
        return LOCK_OK;
    }

    // only one request in the queue
    if (lock->queue == request && request->queue == NULL)
    {
        lock->queue = NULL;
        delete request;

        _l_table_.erase((lock->r_id).get_str_res_id());
        delete lock;

        up_sem(sync);
        return LOCK_OK;
    }

    //interesting case: granted group not null when this request leaves
    if (prev_req != NULL)
        prev_req->queue = request->queue;//remove tr_id's request
    else
        lock->queue = request->queue;

    delete request;

    //reset lock_header
    lock->waiting = false;
    lock->granted_mode = NULL_LOCK;

    bool convert_request_waiting = false;

    //traverse lock queue: compute granted mode, wake compatible waiters

    for (request = lock->queue; request != NULL; request = request->queue)
    {
        //the case is useful when the converting request
        //is founded and it is not compatible with locks of
        //other transactions.
        if (convert_request_waiting == true)
        {//this code needs to compute granted mode for group
            if (request->status == LOCK_GRANTED)
            {
                lock->granted_mode
                        = lock_max(request->mode, lock->granted_mode);
                continue;
            }
            else
            {
                if (request->status == LOCK_CONVERTING)
                {
                    lock->granted_mode = lock_max(request->mode,
                            lock->granted_mode);
                    //here request->mode is the granted lock for the converting lock
                    continue;
                }
                else
                {
                    if (request->status == LOCK_WAITING)
                        break;
                    else
                        throw USER_EXCEPTION2(SE4701, "Unknown status of request");
                }
            }
        }

        if (request->status == LOCK_GRANTED)//if req granted, add to granted mode
        {
            lock->granted_mode = lock_max(request->mode, lock->granted_mode);
        }
        else
        {
            if (request->status == LOCK_WAITING) // if request waiting
            {
                if (lock_compat(request->mode, lock->granted_mode))//if compat with granted
                {
                    request->status = LOCK_GRANTED;
                    request->tran->wait = NULL;
                    lock->granted_mode = lock_max(request->mode,
                            lock->granted_mode);

                    d_printf2("wake up transaction with id=%d\n", request->tran->tr_id);
                    if (request->tran->status != ROLLING_BACK_AFTER_DEADLOCK)
                        if (0 != USemaphoreUp(request->process_xsem,
                                __sys_call_error))
                            throw USER_EXCEPTION2(SE4014, "SEDNA_TRANSACTION_LOCK");
                }
                else //if request is incompatible then FIFO
                {
                    lock->waiting = true;
                    break;
                }
            }
            else //convert waiting case
            {
                if (request->status == LOCK_CONVERTING)
                {
                    lock_request *_req;
                    bool convert_compatible = true;
                    for (_req = lock->queue; _req != NULL; _req = _req->queue)
                    {
                        if (_req->status == LOCK_GRANTED && _req->tran->tr_id
                                != request->tran->tr_id && !lock_compat(
                                request->convert_mode, _req->mode))
                        {
                            convert_compatible = false;
                            break;
                        }
                    }//end nested for

                    if (convert_compatible == true)
                    {
                        request->status = LOCK_GRANTED;
                        request->mode = request->convert_mode;
                        request->tran->wait = NULL;
                        lock->granted_mode = lock_max(request->convert_mode,
                                lock->granted_mode);
                        request->convert_mode = NULL_LOCK;

                        d_printf2("wake up transaction with id=%d\n", request->tran->tr_id);
                        if (request->tran->status
                                != ROLLING_BACK_AFTER_DEADLOCK)
                            if (0 != USemaphoreUp(request->process_xsem,
                                    __sys_call_error))
                                throw USER_EXCEPTION2(SE4014, "SEDNA_TRANSACTION_LOCK");
                    }
                    else
                    {
                        convert_request_waiting = true;
                        lock->waiting = true;
                    }

                }
                else
                    throw USER_EXCEPTION2(SE4701, "Unknown status of request");
            }
        }
    }// end for


    up_sem(sync);
    return LOCK_OK;
}

/////////////////////////////////////////////////////////////////////////////
//////////////////// release all transaction locks method ///////////////////
/////////////////////////////////////////////////////////////////////////////

lock_reply lock_table::release_tr_locks(transaction_id tr_id, bool sync)
{
    lock_request *request;

    down_sem(sync);

    tr_lock_head* tr_head = tr_table.find_tr_lock_head(tr_id);

    if (tr_head == NULL)
    {
        up_sem(sync);
        return LOCK_OK;
    }

    resource_id r_id;
    request = tr_head->tran->locks;
    while (request != NULL)
    {
        r_id = resource_id(request->head->r_id);
        //obtain the pointer to the next request since after unlock call the request will be deleted
        request = request->tran_next;
        unlock(tr_id, r_id, false);
    }

    up_sem(sync);

    delete tr_head;

    return LOCK_OK;
}

//////////////////////////////////////////////////////////////////////////////
//////////////////// print method/////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

void lock_table::print(bool sync)
{
    lock_iter it;

    d_printf1("================= Lock Table ==================\n");

    down_sem(sync);

    for (it = _l_table_.begin(); it != _l_table_.end(); it++)
    {
        //it->first.print();
        print_resource_id(it->first);
        d_printf1(": ");
        it->second->print();
        d_printf1("\n");
    }

    up_sem(sync);

    d_printf1("===============================================\n\n\n");
}

bool lock_table::deadlock(transaction_id trid, bool sync)
{
    down_sem(sync);

    //int tr_num = tr_table.get_trns_num();
    Trans_It it;

    //clean cycle fields in  trn's descriptors
    for (it = tr_table._tr_table_.begin(); it != tr_table._tr_table_.end(); it++)
        if (it->second->tran->tr_id == trid)
            break;

    if (it == tr_table._tr_table_.end())
        throw SYSTEM_EXCEPTION("Lock table is in incosistent state!");

    // nobody visited yet
    nVisited = 0;

    // we look for cycles "starting" only from the current node
    // other cycles we'll be found via corresponding deadlock checks
    try
    {
        visit(it->second->tran);
    }
    catch (SednaUserException)
    {
        up_sem(sync);
        return true;//deadlock is detected
    }

    up_sem(sync);
    return false;//deadlock is not detected
}

void lock_table::visit(TransCB* me)
{
    lock_request* them;

    if (me->wait == NULL)
        return;

    // have we met this node?
    for (unsigned int i = 0; i < nVisited; i++)
    {
        if (nodesVisited[i] == me->tr_id)
        {
            if (i == 0)
                throw USER_EXCEPTION(SE4705); // deadlock
            else
                return; // another cycle, but not through us; or a "false" cycle
        }
    }

    nodesVisited[nVisited++] = me->tr_id;

    them = ((me->wait)->head)->queue;

    if (me->wait->status == LOCK_WAITING)
    {
        //look at everyone  in the queue ahead of me
        while (them->tran->tr_id != me->tr_id)
        {
            if (!lock_compat(them->mode, me->wait->mode) || (them->status
                    == LOCK_CONVERTING && !lock_compat(them->convert_mode,
                    me->wait->mode)))
            {
                visit(them->tran);
            }

            them = them->queue;
            if (them == NULL)
                break;
        }
    }
    else//lock is converting
    {
        bool ahead_me = true;
        //look at everyone in the queue which hold any lock (i.e. granted and converting requests)
        while (them->status != LOCK_WAITING)
        {
            if (them->tran->tr_id == me->tr_id)
            {
                ahead_me = false;
                them = them->queue;
                if (them == NULL)
                    break;
                else
                    continue;
            }
            else
            {
                if ((ahead_me && (!lock_compat(them->mode,
                        me->wait->convert_mode) || (them->status
                        == LOCK_CONVERTING && !lock_compat(them->convert_mode,
                        me->wait->convert_mode)))) || (!ahead_me
                        && (!lock_compat(them->mode, me->wait->convert_mode))))
                {
                    visit(them->tran);
                }

                them = them->queue;
                if (them == NULL)
                    break;
            }
        }//end while
    }

    return;
}

/*****************************************************************************
 helpers
 ******************************************************************************/

bool lock_compat(lock_mode m1, lock_mode m2)
{
    if (m1 == NULL_LOCK || m2 == NULL_LOCK)
        return true;

    //first matrix line
    if (m1 == lm_is && m2 == lm_is)
        return true;

    if (m1 == lm_is && m2 == lm_ix)
        return true;

    if (m1 == lm_is && m2 == lm_s)
        return true;

    if (m1 == lm_is && m2 == lm_x)
        return false;

    if (m1 == lm_is && m2 == lm_six)
        return true;

    //second matrix line
    if (m1 == lm_ix && m2 == lm_is)
        return true;

    if (m1 == lm_ix && m2 == lm_ix)
        return true;

    if (m1 == lm_ix && m2 == lm_s)
        return false;

    if (m1 == lm_ix && m2 == lm_x)
        return false;

    if (m1 == lm_ix && m2 == lm_six)
        return false;

    //third matrix line
    if (m1 == lm_s && m2 == lm_is)
        return true;

    if (m1 == lm_s && m2 == lm_ix)
        return false;

    if (m1 == lm_s && m2 == lm_s)
        return true;

    if (m1 == lm_s && m2 == lm_x)
        return false;

    if (m1 == lm_s && m2 == lm_six)
        return false;

    //fourth matrix line
    if (m1 == lm_x && m2 == lm_is)
        return false;

    if (m1 == lm_x && m2 == lm_ix)
        return false;

    if (m1 == lm_x && m2 == lm_s)
        return false;

    if (m1 == lm_x && m2 == lm_x)
        return false;

    if (m1 == lm_x && m2 == lm_six)
        return false;

    //fifth line
    if (m1 == lm_six && m2 == lm_is)
        return true;

    if (m1 == lm_six && m2 == lm_ix)
        return false;

    if (m1 == lm_six && m2 == lm_s)
        return false;

    if (m1 == lm_six && m2 == lm_x)
        return false;

    if (m1 == lm_six && m2 == lm_six)
        return false;
    else
        throw SYSTEM_EXCEPTION("Unknown lock modes given");
}

lock_mode lock_max(lock_mode m1, lock_mode m2)
{
    if (m1 == NULL_LOCK)
        return m2;

    if (m2 == NULL_LOCK)
        return m1;

    //at least one mode is exclusive
    if (m1 == lm_x || m2 == lm_x)
        return lm_x;

    //at least one mode is six (and not exist exclusive)
    if (m1 == lm_six || m2 == lm_six)
        return lm_six;

    if (m1 == lm_is && m2 == lm_is)
        return lm_is;

    if (m1 == lm_is && m2 == lm_ix)
        return lm_ix;

    if (m1 == lm_ix && m2 == lm_is)
        return lm_ix;

    if (m1 == lm_ix && m2 == lm_ix)
        return lm_ix;

    if (m1 == lm_s && m2 == lm_is)
        return lm_s;

    if (m1 == lm_is && m2 == lm_s)
        return lm_s;

    if (m1 == lm_s && m2 == lm_ix)
        return lm_six;

    if (m1 == lm_ix && m2 == lm_s)
        return lm_six;

    if (m1 == lm_s && m2 == lm_s)
        return lm_s;

    else
        throw SYSTEM_EXCEPTION("Unknown lock modes given");
}
