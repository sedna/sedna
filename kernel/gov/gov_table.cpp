/*
 * File:  gov_sess.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <map>
#include <vector>
#include <iostream>

#include "sedna.h"

#include "gov_table.h"
#include "d_printf.h"
#include "SSMMsg.h"




using namespace std;
/*****************************************************************************/
/********************* Session class implementation **************************/
/*****************************************************************************/

void Session::print_session()
{
  d_printf2("db_name=%s\n", db_name.c_str());
  d_printf2("pid=%d\n", pid);
}


/*****************************************************************************/
/********************* info_table class implementation **************************/
/*****************************************************************************/

void info_table::init(int lstnr_port_number)
{
   int i = 0;
  
   for (i=0; i< MAX_SESSIONS_NUMBER; i++)
       _ids_table_.push_back(i);



   if (0 != uCreateShMem(&gov_shm_service_dsc,
                         GOVERNOR_SHARED_MEMORY_NAME,
                         GOV_SHM_SIZE,
                         NULL,
                         __sys_call_error))
      throw USER_EXCEPTION2(SE4016, "GOVERNOR_SHARED_MEMORY_NAME");


   gov_shared_mem = uAttachShMem(gov_shm_service_dsc,
                                 NULL,
                                 GOV_SHM_SIZE,
                                 __sys_call_error);

   if (gov_shared_mem == NULL)
      throw USER_EXCEPTION2(SE4023, "GOVERNOR_SHARED_MEMORY_NAME");

 //  if (uNotInheritDescriptor(UHANDLE(gov_shm_service_dsc)) != 0) throw USER_EXCEPTION(SE4080);

   //init section
   ((gov_header_struct*)GOV_SHM)->is_server_stop = 0;
   ((gov_header_struct*)GOV_SHM)->lstnr_port_number = lstnr_port_number;
   ((gov_header_struct*)GOV_SHM)->gov_pid = uGetCurrentProcessId(__sys_call_error);
   

   for (i = 0; i<MAX_DBS_NUMBER; i++)
   {
      (((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] = '\0';
      ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->is_stop = 0;
      ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->sm_pid = -1;

   }

   for (i = 0; i<MAX_SESSIONS_NUMBER; i++)
   {
      ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + i*sizeof(gov_sess_struct)))->idfree = 0;
      ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + i*sizeof(gov_sess_struct)))->stop = 0;
   }
}

void info_table::release()
{
  if ( 0 != uDettachShMem(gov_shm_service_dsc, gov_shared_mem, __sys_call_error))
     throw USER_EXCEPTION2(SE4024, "GOVERNOR_SHARED_MEMORY_NAME");

  if ( 0 != uReleaseShMem(gov_shm_service_dsc, __sys_call_error))
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
  UPID pid;
  bool is_child;
  int res;

  this->get_session_info(s_id, pid, proc_handle, is_child);

  if (is_child)
     res = uWaitForChildProcess(pid, proc_handle, NULL, __sys_call_error);
  else
     res = uWaitForProcess(pid, proc_handle, __sys_call_error);

  if (res != 0)
     throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");


  _session_table_.erase(s_id);
  this->give_id(s_id);
  uCloseProcess(proc_handle, __sys_call_error);

  ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + s_id*sizeof(gov_sess_struct)))->stop = 0;
  ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + s_id*sizeof(gov_sess_struct)))->idfree = 0;
}

void info_table::stop_session(const session_id& s_id)
{

  UPID pid;  
  UPHANDLE proc_handle;
  int res;
  bool is_child_process = true;
  
  ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + s_id*sizeof(gov_sess_struct)))->stop = 1;

  this->wait_erase_session(s_id);

  return;
}

int info_table::insert_session(UPID &pid/*in*/, UPHANDLE* h_p, std::string &db_name/*in*/, bool is_child, session_id& s_id/*out*/)//returns session_id and locks pid until end of session
{


  UPHANDLE proc_handle;

  
  if (h_p == NULL)
  {
     if (uOpenProcess(pid, &proc_handle, __sys_call_error) != 0)
        return -4;
  }
  else proc_handle = *h_p;

  //check is exist database
  if (_database_table_.count(db_name) == 0)
  {
     if (!is_child)  uCloseProcess(proc_handle, __sys_call_error);
     return -1;
  }

  s_id = this->get_id();
  if (s_id == -1)
  {
     if (!is_child)  uCloseProcess(proc_handle, __sys_call_error);
     return -2;
  }


  bool ret_val = true;
  pair<s_table_iter, bool> it;  

  
  Session s = Session(db_name, pid, proc_handle, is_child);
  it = _session_table_.insert(s_record(s_id, s));

  if (!(it.second))
  { 
     this->give_id(s_id);
     if (!is_child)  uCloseProcess(proc_handle, __sys_call_error);
     return -3;
  }

  ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + s_id*sizeof(gov_sess_struct)))->idfree = 1;
  ((gov_sess_struct*)(GOV_SHM_DBS_OFFS + s_id*sizeof(gov_sess_struct)))->stop = 0;

  return 0;
}

int info_table::insert_database(UPID &pid/*in*/, std::string &db_name)//return -1 if error
{
  int i = 0;
  UPHANDLE proc_handle;
  if (uOpenProcess(pid, &proc_handle, __sys_call_error) != 0)
     return -4;



  for (i=0; i< MAX_DBS_NUMBER; i++)
  {
     if ((((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] == '\0')
     {
        //copy the whole string excepting first symbol
        strcpy((((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name) + sizeof(char), (db_name.c_str()) + sizeof(char));
        ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->is_stop = 0;
        ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->sm_pid = pid;
        //this indicates the end of coping
        (((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] = (db_name.c_str())[0];

        d_printf2("shm database name=%s\n", ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name);
        break;
     }   
  }

  if (i== MAX_DBS_NUMBER)
  { 
     uCloseProcess(proc_handle, __sys_call_error);
     return -2;
  }

  Database db = Database(pid, proc_handle);
  pair<db_table_iter, bool> it;
  it = _database_table_.insert(db_record(db_name, db));
  if (!(it.second))
  {
     uCloseProcess(proc_handle, __sys_call_error);
     (((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] = '\0';
     ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->is_stop = 0;
     ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->sm_pid = -1;

     return -3;
  }



  return 0;
}


void info_table::erase_database(const database_id& db_id)
{
  int i=0;

  for (i=0; i< MAX_DBS_NUMBER; i++)
  {
     if (strcmp(((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name, db_id.c_str()) == 0)
     {
        (((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] = '\0';
        ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->is_stop = 0;
        ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->sm_pid = -1;
        break;
     }   
  }

   _database_table_.erase(db_id);  
}



void info_table::put_all_free_sids_in_ids_table()
{
  int i=0;
  d_printf1("put free ids\n");

  for (i = 0; i< MAX_SESSIONS_NUMBER; i++)
    if (((gov_sess_struct*)(GOV_SHM_DBS_OFFS + i*sizeof(gov_sess_struct)))->idfree == 2)
    {
       d_printf1("erase session\n");
       this->wait_erase_session(i);
    }
}

void info_table::erase_all_closed_pids()
{
  pids_table_iter it;
  vector<UPID> tmp;
  int i;

  for(it = _pids_table_.begin(); it != _pids_table_.end(); it++)
  {
    if (uIsProcessExist(it->first, it->second.p, __sys_call_error) != 1)
       tmp.push_back(it->first);
  }

  for (i = 0; i< tmp.size(); i++)
    _pids_table_.erase(tmp[i]);
}


void info_table::stop_sessions(const string &db_name)
{
  s_table_iter it;
  vector<session_id> tmp;
  int i=0;

  for(it = _session_table_.begin(); it!=_session_table_.end(); it++)
  {
     if ( it->second.db_name == db_name )
        tmp.push_back(it->first);
  }

  for (i = 0; i< tmp.size(); i++)  
      stop_session(tmp[i]);

}


void info_table::stop_sessions()
{
  s_table_iter it;
  vector<session_id> tmp;
  int i=0;


  for (it = _session_table_.begin(); it!=_session_table_.end(); it++)
      tmp.push_back(it->first);

  for (i=0; i< tmp.size(); i++)
      stop_session(tmp[i]);

  d_printf1("All sessions over Sedna has been closed succesfully\n");
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
                         CHARISMA_SSMMSG_SM_ID(db_id.c_str(), buf, 1024), 
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

  uCloseProcess(proc_handle, __sys_call_error);

  return;
}

bool info_table::is_database_run(const database_id& db_id)
{
  db_table_const_iter c_it;
  
  c_it = _database_table_.find(db_id);

  if ( c_it != _database_table_.end() )//exist record with key db_id
     return true;
  else
     return false;
}

void info_table::stop_databases()
{
  db_table_iter it;
  vector<string> tmp;
  int i=0;


  for(it = _database_table_.begin(); it!=_database_table_.end(); it++)
     tmp.push_back(it->first);

  for(i=0; i< tmp.size(); i++) 
     stop_database(tmp[i]);

  d_printf1("All databases over Sedna has been closed succesfully\n");
}

int info_table::check_stop_gov()
{
  if (((gov_header_struct*)GOV_SHM)->is_server_stop == 1)
  {
     stop_sessions();
     stop_databases();
     return 1;
  }
  return 0;
}

int info_table::check_stop_databases()
{
  int i=0;
  int ret_code =0;
  for (i=0; i< MAX_DBS_NUMBER; i++) 
  {
     if ((((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name)[0] != '\0' &&
         ((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->is_stop == 1)
     {
        stop_sessions(string(((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name));

        stop_database(string(((gov_dbs_struct*)(GOV_SHM_HEADER_OFFS + i*sizeof(gov_dbs_struct)))->db_name));
        ret_code = 1;
     }
  }

  return ret_code;
}

string info_table::get_rc()
{
   string rc;
   db_table_iter it;

   rc =  "Runtime configuration information for the SEDNA server:\n\n";
   rc += "==================================\n";
   rc += "| Components\t | Status\t |\n";
   rc += "==================================\n"; 
   rc += "| GOVERNOR\t | running\t |\n";


   rc += "==================================\n";

   rc += "\n\n";

   if (!_database_table_.empty())
   {
     rc += "The following databases (SMs) are started:\n";

     for(it = _database_table_.begin(); it!=_database_table_.end(); it++)
        rc += string("\t") + it->first + "\n";
   }
   else
     rc += "There is no any database (SM) started";
     
   return rc;
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

//    uTerminateProcess(pid, proc_handle, 1);    
////////tmp
//uOpenProcess(pid, proc_handle);
/////////tmp
    res = uWaitForChildProcess(pid, proc_handle, NULL, __sys_call_error);
    if (res != 0)
    {
       throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");
    }

    uCloseProcess(proc_handle, __sys_call_error);

    this->remove_pid(pid);    
  }
  else
  {

    if (uOpenProcess(pid, &proc_handle, __sys_call_error) == 0)
    {
//       uTerminateProcess(pid, proc_handle, 1);

       res = uWaitForProcess(pid, proc_handle, __sys_call_error);
       if (res != 0)
          throw SYSTEM_EXCEPTION("Error, WaitForProcess failed");

       uCloseProcess(proc_handle, __sys_call_error);
    }
    else 
       d_printf1("Can't open process\n");

  }
}



void info_table::wait_all_notregistered_sess()
{
  pids_table_iter it;
  int status;

  for(it = _pids_table_.begin(); it!=_pids_table_.end(); it++)
  {
     uTerminateProcess(it->first, it->second.p, 1, __sys_call_error);
     uWaitForChildProcess(it->first, it->second.p, NULL, __sys_call_error);
     uCloseProcess(it->second.p, __sys_call_error);
  }

  _pids_table_.clear();
}

int info_table::get_total_session_procs_num()
{
  return _pids_table_.size() + _session_table_.size();
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
