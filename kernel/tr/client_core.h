/*
 * File:  client_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CLIENT_CORE_H
#define _CLIENT_CORE_H

#include "common/sedna.h"
#include <string>
#include "common/ipc_ops.h"
#include "tr/crmutils/crmutils.h"
#include "tr/crmutils/exec_output.h"
#include "common/u/uhdd.h"
#include "common/base.h"

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
    virtual void set_session_options(msg_struct *msg) = 0;
    virtual void reset_session_options() = 0;
    virtual void get_file_from_client(std::vector<std::string>* filenames, std::vector<client_file>* cf_vec) = 0;
    virtual void close_file_from_client(client_file &inout_cf) = 0;
    virtual void respond_to_client(int instruction) = 0;
    virtual void begin_item() = 0;
    virtual void end_of_item(qepNextAnswer exist_next) = 0;
    virtual bool is_print_progress() = 0;
    virtual int get_os_primitives_id_min_bound() = 0;
    virtual void authentication_result(bool res, const std::string& body) = 0;
    virtual void process_unknown_instruction(int instruction, bool in_transaction) = 0;
    virtual void error(int code, const std::string& body) = 0;
    virtual void show_time(std::string qep_time) = 0;
    virtual void write_user_query_to_log() = 0;
    virtual void set_keep_alive_timeout(int sec) = 0;
};


#endif

