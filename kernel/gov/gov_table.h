/*
 * File:  gov_table.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _GOV_TABLE_H
#define _GOV_TABLE_H

#include <string>
#include <map>

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/ushm.h"
#include "common/config.h"
#include "gov/rc.h"


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

/* Session record, which consists of Session identifier and class session */
typedef std::pair<session_id, Session> s_record;

typedef std::string database_id;

typedef std::map<database_id, Database>::const_iterator db_table_const_iter;
typedef std::map<database_id, Database>::iterator db_table_iter;
typedef std::pair<database_id, Database> db_record;

typedef std::map<UPID, Process>::iterator pids_table_iter;
typedef std::pair<UPID, Process> pid_record;

class info_table
{
private:
    std::map<session_id, Session> _session_table_;
    std::vector<session_id> _ids_table_;
    std::map<database_id, Database> _database_table_;
    /* Contains pid's of all child processes */
    std::map<UPID, Process> _pids_table_;
    UShMem gov_shm_service_dsc;
    void* gov_shared_mem;

    session_id get_id();
    void give_id(const session_id&);
    int get_session_info(const session_id& s_id, UPID& pid/*out*/, UPHANDLE& proc_handle/*out*/, bool& is_child/* out */);
    int get_database_info(const database_id& db_id, UPID& pid/*out*/, UPHANDLE& proc_handle/*out*/);
    
    /* Stop session and erase session from the table (plus 
     * unlock pid of session) and return session id */
    void stop_session(const session_id& s_id);
    
    /* Give session id, unlock pid of session and
     * erase from session table */
    void wait_erase_session(const session_id& s_id);
    void erase_database(const database_id& db_id);

public:  
    void init(gov_config_struct* cfg);
    void release();

    int insert_session(UPID &pid, UPHANDLE *p, std::string &db_name, bool is_child, session_id& s_id/*out*/, bool special_mode);
    int insert_database(UPID &pid, std::string &db_name, bool special_mode);
    void put_all_free_sids_in_ids_table();
    void erase_all_closed_pids();
    void stop_sessions(const std::string &db_name);//stop all sessions of db_name and erase sessions from the table
    void stop_sessions();//atopes and erases all sessions in sedna

    void stop_database(const database_id& db_id);
    bool is_database_run(const database_id& db_id);
    void stop_databases();//stops all active databases
    int  check_stop_gov();
    int  check_stop_databases();
    int get_rc(rc_vector& rc);
    void add_pid(UPID, UPHANDLE &h);
    void remove_pid(UPID);
    void wait_remove_pid(UPID, bool);
    bool find_pid(UPID pid, UPHANDLE& p);//returns true if pid is found (and inits p)
    void wait_all_notregistered_sess();

    int  get_total_session_procs_num();
    gov_config_struct* get_config_struct();
    void print_info_table();
};

#endif /* _GOV_TABLE_H */

