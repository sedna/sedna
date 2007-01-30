/*
 * File:  client_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CLIENT_CORE_H
#define _CLIENT_CORE_H

#include "sedna.h"
#include <string>
#include "ipc_ops.h"
#include "crmutils.h"
#include "exec_output.h"
#include "uhdd.h"
#include "base.h"

struct client_file{
	FILE* f;
	char name[1024];
    __int64 file_size;//size of file in bytes
};

class client_core
{
public:
	//virtual ~client_core() {}
	
    virtual void init() = 0;
    virtual void release() = 0;
    virtual void read_msg(msg_struct *msg) = 0;
    virtual char* get_query_string(msg_struct *msg) = 0;
    virtual t_print get_result_type(msg_struct *msg) = 0;
    virtual QueryType get_query_type() = 0;
    virtual se_ostream* get_se_ostream() = 0;
    virtual void get_session_parameters() = 0;
    virtual client_file get_file_from_client(const char* filename) = 0;
    virtual void close_file_from_client(client_file &inout_cf) = 0;
    virtual void respond_to_client(int instruction) = 0;
    virtual void begin_item() = 0;
    virtual void end_of_item(bool exist_next) = 0;
    virtual bool is_print_progress() = 0;
/*    
    virtual void update_result(bool res) = 0;
    virtual void bulk_load_result(bool res, const std::string& body) = 0;
    virtual void begin_tr_result(bool res, const std::string& body) = 0;
    virtual void commit_tr_result(bool res, const std::string& body) = 0;
    virtual void rollback_tr_result(bool res, const std::string& body) = 0;
    virtual void rollback_tr_before_close(bool res, const std::string& body) = 0;
    virtual void close_session_result(bool res, const std::string& body) = 0;
*/    
    virtual void authentication_result(bool res, const std::string& body) = 0;
    virtual void process_unknown_instruction(int instruction, bool in_transaction) = 0;
    virtual void error(int code, const std::string& body) = 0;
    virtual void show_time(std::string qep_time) = 0;
    virtual void write_user_query_to_log() = 0;
};


#endif

