/*
 * File:  gov_sess.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_SESS_H
#define _GOV_SESS_H

#include <string>
#include <map>

#include "sedna.h"

#include "base.h"
#include "ushm.h"

#define GOV_SHM (gov_shared_mem)
#define GOV_SHM_HEADER_OFFS ((char*)gov_shared_mem + sizeof(gov_header_struct))
#define GOV_SHM_DBS_OFFS  ((char*)gov_shared_mem + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct))
#define GOV_SHM_SESS_OFFS ((char*)gov_shared_mem + sizeof(gov_header_struct) + MAX_DBS_NUMBER*sizeof(gov_dbs_struct) + MAX_SESSIONS_NUMBER*sizeof(gov_sess_struct))

class Session
{
public:
  std::string db_name;
  UPID pid;
  UPHANDLE p;
  bool is_child;
  Session(std::string _db_name_, UPID _pid_, UPHANDLE _p_, bool _is_child_): db_name(_db_name_), pid(_pid_), p(_p_), is_child(_is_child_){};
  ~Session(){};
  void print_session();
    
};

class Database
{
public:
  UPID pid;
  UPHANDLE p;
  Database(UPID _pid_, UPHANDLE _p_): pid(_pid_), p(_p_){};
  ~Database(){};
};

class Process
{
public:
  UPHANDLE p;
  Process(UPHANDLE _p_): p(_p_) {};
};

typedef std::map<session_id, Session>::const_iterator s_table_const_iter;
typedef std::map<session_id, Session>::iterator s_table_iter;
typedef std::pair<session_id, Session> s_record;//session record, which consists of Session identifier and class session

typedef std::string database_id;

typedef std::map<database_id, Database>::const_iterator db_table_const_iter;
typedef std::map<database_id, Database>::iterator db_table_iter;
typedef std::pair<database_id, Database> db_record;

typedef std::map<UPID, Process>::iterator pids_table_iter;
typedef std::pair<UPID, Process> pid_record;


class info_table
{
private:
  //variables
  std::map<session_id, Session> _session_table_;
  std::vector<session_id> _ids_table_;
  std::map<database_id, Database> _database_table_;
  std::map<UPID, Process> _pids_table_;//contains pid's of all child processes
  UShMem gov_shm_service_dsc;
  void* gov_shared_mem;

  //functions
  session_id get_id();
  void give_id(const session_id&);
  int get_session_info(const session_id& s_id, UPID& pid/*out*/, UPHANDLE& proc_handle/*out*/, bool& is_child/* out */);
  int get_database_info(const database_id& db_id, UPID& pid/*out*/, UPHANDLE& proc_handle/*out*/);
  void stop_session(const session_id& s_id);//stop sessions and erase session from the table (plus unlock pid of session)  and return session id
  void wait_erase_session(const session_id& s_id);//give session id, unlock pid of session and erase from session table
  void erase_database(const database_id& db_id);

public:  
  void init(int lstnr_port_number);
  void release();

  int insert_session(UPID &pid, UPHANDLE *p, std::string &db_name, bool is_child, session_id& s_id/*out*/);//init s_id locks pid until end of session
  int insert_database(UPID &pid/*in*/, std::string &db_name);//return -1 if error
  void put_all_free_sids_in_ids_table();
  void erase_all_closed_pids();
  void stop_sessions(const std::string &db_name);//stop all sessions of db_name and erase sessions from the table
  void stop_sessions();//atopes and erases all sessions in sedna

  void stop_database(const database_id& db_id);
  bool is_database_run(const database_id& db_id);
  void stop_databases();//stops all active databases
  int  check_stop_gov();
  int  check_stop_databases();
  std::string get_rc();
  void add_pid(UPID, UPHANDLE &h);
  void remove_pid(UPID);
  void wait_remove_pid(UPID, bool);
  bool find_pid(UPID pid, UPHANDLE& p);//returns true if pid is found (and inits p)
  void wait_all_notregistered_sess();

  int  get_total_session_procs_num();
  void print_info_table();
};

#endif

