/*
 * File:  gov_table.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <map>
#include <vector>
#include <iostream>

#include "common/sedna.h"

#include "gov/gov_table.h"
#include "common/errdbg/d_printf.h"
#include "common/SSMMsg.h"
#include "common/config.h"
#include "config_utils.h"
#include "common/ipc_ops.h"

#include "common/sm_vmm_data.h"


void info_table::init(gov_config_struct* cfg)
{
    int i = 0;

    for (i=0; i< MAX_SESSIONS_NUMBER; i++)
        _ids_table_.push_back(i);

    if (0 != uCreateShMem(&gov_shm_service_dsc,
        GOVERNOR_SHARED_MEMORY_NAME,
        sizeof(gov_config_struct),
        NULL,
        __sys_call_error))
        throw USER_EXCEPTION2(SE4016, "GOVERNOR_SHARED_MEMORY_NAME");


    gov_shared_mem = uAttachShMem(&gov_shm_service_dsc,
        NULL,
        0,
        __sys_call_error);

    if (gov_shared_mem == NULL)
        throw USER_EXCEPTION2(SE4023, "GOVERNOR_SHARED_MEMORY_NAME");

    memcpy(gov_shared_mem, cfg, sizeof(gov_config_struct));
}

void info_table::release()
{
    if ( 0 != uDettachShMem(&gov_shm_service_dsc, gov_shared_mem, __sys_call_error))
        throw USER_EXCEPTION2(SE4024, "GOVERNOR_SHARED_MEMORY_NAME");

    if ( 0 != uReleaseShMem(&gov_shm_service_dsc, GOVERNOR_SHARED_MEMORY_NAME, __sys_call_error))
        throw USER_EXCEPTION2(SE4020, "GOVERNOR_SHARED_MEMORY_NAME");
}


session_id info_table::get_id()
{
    session_id id;
    if (_ids_table_.empty())
        id = -1;
    else
    {
        id = _ids_table_.back();
        _ids_table_.pop_back();
    }

    d_printf2("returned id=%d\n", id);
    if (id >= MAX_SESSIONS_NUMBER) throw SYSTEM_EXCEPTION("Got incorrect session id");
    return id;
}


void info_table::give_id(const session_id& id)
{
    if (id != -1)
        _ids_table_.push_back(id);
}


int info_table::get_session_info(const session_id& s_id, UPID& pid, UPHANDLE& proc_handle, bool& is_child)
{
    s_table_const_iter c_it;

    c_it = _session_table_.find(s_id);

    if ( c_it != _session_table_.end() )//exist record with key s_id
    {
        pid = c_it->second.pid;
        proc_handle = c_it->second.p;
        is_child = c_it->second.is_child;
        return 0;
    }
    else
        return -1;
}


int info_table::get_database_info(const database_id& db_id, UPID& pid/*out*/, UPHANDLE& proc_handle/*out*/)
{
    db_table_const_iter c_it;

    c_it = _database_table_.find(db_id);

    if ( c_it != _database_table_.end() )//exist record with key db_id
    {
        pid = c_it->second.pid;
        proc_handle = c_it->second.p;
        return 0;
    }
    else
        return -1;
}


void info_table::wait_erase_session(const session_id& s_id)
{
    UPHANDLE proc_handle;
    UPID pid = 0;
    bool is_child = false;
    int res = 0;

    if(this->get_session_info(s_id, pid, proc_handle, is_child) != -1) {
        if (is_child)
            res = uWaitForChildProcess(pid, proc_handle, NULL, __sys_call_error);
        else
            res = uWaitForProcess(pid, proc_handle, __sys_call_error);

        if (res != 0)
            throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");

        _session_table_.erase(s_id);
        this->give_id(s_id);
        uCloseProcessHandle(proc_handle, __sys_call_error);

        ((gov_config_struct*)gov_shared_mem)->sess_vars[s_id].stop = 0;
        ((gov_config_struct*)gov_shared_mem)->sess_vars[s_id].idfree = 0;
    }
}


void info_table::stop_session(const session_id& s_id)
{
    ((gov_config_struct*)gov_shared_mem)->sess_vars[s_id].stop = 1;

    this->wait_erase_session(s_id);

    return;
}


/* Returns session_id and locks pid until end of session */
int
info_table::insert_session(/* in */  UPID &pid,
                           UPHANDLE* h_p,
                           /* in */  std::string &db_name,
                           bool is_child,
                           /* out */ session_id& s_id,
                           bool special_mode)
{
    UPHANDLE proc_handle;

    if (h_p == NULL)
    {
        if (uOpenProcess(pid, &proc_handle, __sys_call_error) != 0)
            return -4;
    }
    else proc_handle = *h_p;

    /* Check if database is running */
    int i;
    for (i = 0; i < MAX_DBS_NUMBER; i++)
    {
        if (strcmp(((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name, db_name.c_str()) == 0)
        {
            sm_operation_mode m = ((gov_config_struct*)gov_shared_mem)->db_vars[i].mode;
            if (m != OM_SM_WORKING && !(special_mode && m == OM_SM_SPECIAL_MODE)) return -1;
            break;
        }
    }
    if( i == MAX_DBS_NUMBER ) return -1;

    /* Check if database exists in database table */
    if (_database_table_.count(db_name) == 0)
    {
        if (!is_child) uCloseProcessHandle(proc_handle, __sys_call_error);
        return -1;
    }

    s_id = this->get_id();
    if (s_id == -1)
    {
        if (!is_child) uCloseProcessHandle(proc_handle, __sys_call_error);
        return -2;
    }

    std::pair<s_table_iter, bool> it;

    Session s = Session(db_name, pid, proc_handle, is_child);
    it = _session_table_.insert(s_record(s_id, s));

    if (!(it.second))
    {
        this->give_id(s_id);
        if (!is_child) uCloseProcessHandle(proc_handle, __sys_call_error);
        return -3;
    }

    ((gov_config_struct*)gov_shared_mem)->sess_vars[s_id].idfree = 1;
    ((gov_config_struct*)gov_shared_mem)->sess_vars[s_id].stop = 0;

    return 0;
}


int info_table::insert_database(UPID &pid, std::string &db_name, bool special_mode)
{
    int i = 0;
    UPHANDLE proc_handle;
    if (uOpenProcess(pid, &proc_handle, __sys_call_error) != 0)
        return -4;

    for (i=0; i< MAX_DBS_NUMBER; i++)
    {
        if (strcmp(((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name, db_name.c_str()) == 0)
        {
            ((gov_config_struct*)gov_shared_mem)->db_vars[i].mode = special_mode ? OM_SM_SPECIAL_MODE : OM_SM_WORKING;
            ((gov_config_struct*)gov_shared_mem)->db_vars[i].sm_pid = pid;
            break;
        }
    }

    if (MAX_DBS_NUMBER == i)
    {
        uCloseProcessHandle(proc_handle, __sys_call_error);
        return -2;
    }

    Database db = Database(pid, proc_handle);
    std::pair<db_table_iter, bool> it;
    it = _database_table_.insert(db_record(db_name, db));
    if (!(it.second))
    {
        uCloseProcessHandle(proc_handle, __sys_call_error);
        ((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name[0] = '\0';
        ((gov_config_struct*)gov_shared_mem)->db_vars[i].mode = OM_SM_DOWN;
        ((gov_config_struct*)gov_shared_mem)->db_vars[i].sm_pid = -1;
        return -3;
    }

    return 0;
}


void info_table::erase_database(const database_id& db_id)
{
    int i=0;

    for (i=0; i< MAX_DBS_NUMBER; i++)
    {
        if (strcmp(((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name, db_id.c_str()) == 0)
        {
            ((gov_config_struct*)gov_shared_mem)->db_vars[i].mode = OM_SM_DOWN;
            ((gov_config_struct*)gov_shared_mem)->db_vars[i].sm_pid = -1;
            break;
        }
    }

    _database_table_.erase(db_id);
}


void info_table::put_all_free_sids_in_ids_table()
{
    int i=0;

    for (i = 0; i < MAX_SESSIONS_NUMBER; i++)
        if (((gov_config_struct*)gov_shared_mem)->sess_vars[i].idfree == 2)
        {
            d_printf2("erase session: %d\n", i);
            this->wait_erase_session(i);
        }
}


void info_table::erase_all_closed_pids()
{
    pids_table_iter it;
    std::vector<UPID> tmp;
    int res;

    for(it = _pids_table_.begin(); it != _pids_table_.end(); it++)
    {
        uNonBlockingWaitForChildProcess(it->first);
        res = uIsProcessExist(it->first, it->second.p, __sys_call_error);
        if (-1 == res)
            tmp.push_back(it->first);
        else if (-2 == res)
            throw SYSTEM_EXCEPTION("Governor failed while erasing closed pids");
    }

    for (size_t i = 0; i< tmp.size(); i++)
        _pids_table_.erase(tmp[i]);
}


void info_table::stop_sessions(const std::string &db_name)
{
    s_table_iter it;
    std::vector<session_id> tmp;

    for(it = _session_table_.begin(); it!=_session_table_.end(); it++)
    {
        if ( it->second.db_name == db_name )
            tmp.push_back(it->first);
    }

    for (unsigned int i = 0; i < tmp.size(); i++)
        stop_session(tmp[i]);

    elog(EL_DBG, ("%"PRIu32" session(s) on '%s' database stopped", tmp.size(), db_name.c_str()));
}


void info_table::stop_sessions()
{
    s_table_iter it;
    std::vector<session_id> tmp;

    for (it = _session_table_.begin(); it!=_session_table_.end(); it++)
        tmp.push_back(it->first);

    for (unsigned int i = 0; i < tmp.size(); i++)
        stop_session(tmp[i]);

    elog(EL_DBG, ("%"PRIu32" session(s) have been stopped", tmp.size()));
}


void info_table::stop_database(const database_id& db_id)
{
    UPID pid;
    UPHANDLE proc_handle;
    SSMMsg *sm_server = NULL;
    int res;
    char buf[1024];

    d_printf2("database id =%s\n", db_id.c_str());

    if (this->get_database_info(db_id, pid, proc_handle) != 0)
        throw SYSTEM_EXCEPTION("Error in Logic of Governor service (database is not found)");

    sm_server = new SSMMsg(SSMMsg::Client,
        sizeof (sm_msg_struct),
        CHARISMA_SSMMSG_SM_ID(get_db_id_by_name((gov_config_struct*)gov_shared_mem, db_id.c_str()), buf, 1024),
        SM_NUMBER_OF_SERVER_THREADS);

    if (sm_server->init() != 0)
        throw SYSTEM_EXCEPTION("Failed to initialize SSMMsg service (message service)");

    this->erase_database(db_id);//this call must be before send message to sm

    sm_msg_struct msg;
    msg.cmd = 10;

    if (sm_server->send_msg(&msg) != 0)
        throw SYSTEM_EXCEPTION("Can't send message via SSMMsg");

    if (sm_server->shutdown() != 0)
        throw SYSTEM_EXCEPTION("Failed to shutdown SSMMsg service (message service)");

    delete sm_server;
    sm_server = NULL;

    res = uWaitForProcess(pid, proc_handle, __sys_call_error);
    if (res != 0)
        throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");

    uCloseProcessHandle(proc_handle, __sys_call_error);
}


bool info_table::is_database_run(const database_id& db_id)
{
    db_table_const_iter c_it;

    c_it = _database_table_.find(db_id);

    if ( c_it != _database_table_.end() )
        /* Exist record with key db_id */
        return true;
    else
        return false;
}


void info_table::stop_databases()
{
    db_table_iter it;
    std::vector<std::string> tmp;

    for(it = _database_table_.begin(); it!=_database_table_.end(); it++)
        tmp.push_back(it->first);

    for(size_t i=0; i< tmp.size(); i++)
        stop_database(tmp[i]);

    d_printf1("All databases over Sedna has been closed succesfully\n");
}


int info_table::check_stop_gov()
{
    if (((gov_config_struct*)gov_shared_mem)->gov_vars.is_server_stop != SE_STOP_NO)
    {
        stop_sessions();
        stop_databases();
        return 1;
    }
    return 0;
}


int info_table::check_stop_databases()
{
    int ret_code =0;
    for (unsigned int i=0; i< MAX_DBS_NUMBER; i++)
    {
        if ((((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name)[0] != '\0' &&
            ((gov_config_struct*)gov_shared_mem)->db_vars[i].mode == OM_SM_SHUTDOWN)
        {
            d_printf4("%s: %d %s\n", __FUNCTION__, i, ((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name);
            stop_sessions(std::string(((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name));

            stop_database(std::string(((gov_config_struct*)gov_shared_mem)->db_vars[i].db_name));
            ret_code = 1;
        }
    }

    return ret_code;
}


/* 
 * Fills the given runtime configuration vector. If the 
 * function succeeds, the return value is 0. Else it returns
 * 1 which means that gov table state is not consistent at 
 * the moment. 
 */
int info_table::get_rc(/*in*/ rc_vector& rc)
{
    db_table_const_iter dit     = _database_table_.begin();
    db_table_const_iter dit_end = _database_table_.end();

    for(; dit != dit_end; dit++)
    {
        rc.insert ( rc_pair(dit->first, 0) );
    }

    s_table_const_iter cit      = _session_table_.begin();
    s_table_const_iter cit_end  = _session_table_.end();

    for(; cit != cit_end; cit++)
    {
        rc_iterator rit = rc.find( (cit->second).db_name );
        if(rit != rc.end())
        {
            (rit->second)++;
        }
        else
            return 1;
    }
    return 0;
}


void info_table::add_pid(UPID pid, UPHANDLE &h)
{
    _pids_table_.insert(pid_record(pid, Process(h)));
}


void info_table::remove_pid(UPID pid)
{
    _pids_table_.erase(pid);
}


bool info_table::find_pid(UPID pid, UPHANDLE& p)
{
    pids_table_iter it;
    if ( (it = _pids_table_.find(pid)) == _pids_table_.end())
        return false;
    else
    {
        p = it->second.p;
        return true;
    }
}


void info_table::wait_remove_pid(UPID pid, bool is_child_process)
{
    UPHANDLE proc_handle;
    int res;

    if (is_child_process)
    {
        if (! (this->find_pid(pid, proc_handle)))
            throw SYSTEM_EXCEPTION("Error, pid not found in gov table");

        res = uWaitForChildProcess(pid, proc_handle, NULL, __sys_call_error);
        if (res != 0)
        {
            throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");
        }

        uCloseProcessHandle(proc_handle, __sys_call_error);

        this->remove_pid(pid);
    }
    else
    {
        res = uOpenProcess(pid, &proc_handle, __sys_call_error);
        if(res == 0)
        {
            if (uWaitForProcess(pid, proc_handle, __sys_call_error) != 0)
                throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");
            uCloseProcessHandle(proc_handle, __sys_call_error);
        }
        else
            elog(EL_WARN, ("Can't open session process to wait"));

    }
}


void info_table::wait_all_notregistered_sess()
{
    pids_table_iter it;

    elog(EL_DBG, ("%u not registered session(s) will be terminated", (unsigned int)_pids_table_.size()));
    for(it = _pids_table_.begin(); it!=_pids_table_.end(); it++)
    {
        /* Win32 API Note. In this case we operate with child processes 
         * handles. So that we have PROCESS_ALL_ACCESS and can run Terminate
         * process and other manipulation functions.
         */
        uTerminateProcess(it->first, it->second.p, 1, __sys_call_error);
        uWaitForChildProcess(it->first, it->second.p, NULL, __sys_call_error);
        uCloseProcessHandle(it->second.p, __sys_call_error);
    }
    _pids_table_.clear();
}


size_t info_table::get_total_session_procs_num()
{
    return _pids_table_.size() + _session_table_.size();
}


gov_config_struct* info_table::get_config_struct()
{
    return (gov_config_struct*)gov_shared_mem;
}


void info_table::print_info_table()
{
    s_table_iter c_it;

    d_printf1("\n\n=============== SESSION TABLE ====================\n");
    for(c_it = _session_table_.begin(); c_it!=_session_table_.end(); c_it++)
    {
        d_printf2("%d", c_it->first);
        c_it->second.print_session();
        d_printf1("\n\n");
    }
    d_printf1("==================================================\n\n");
}


void Session::print_session()
{
    d_printf2("db_name=%s\n", db_name.c_str());
    d_printf2("pid=%d\n", pid);
}
