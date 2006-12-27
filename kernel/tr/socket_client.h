/*
 * File:  socket_client.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SOCKET_CLIENT_H
#define _SOCKET_CLIENT_H

#include "sedna.h"	
#include <list>
#include <string>
#include "exec_output.h"
#include "client_core.h"
#include "sp.h"
#include "base.h"
#include "usocket.h"
#include "tr_globals.h"
#include "tr_utils.h"

enum client_states {NO_TRANSACTION, IN_TRANSACTION};
enum read_msg_states {se_BeginAuthenticatingTransaction = 3,
                      se_Authentication = 2,
                      se_CommitAuthenticatingTransaction = 1,
                      se_GetNextMessageFromClient = 0 };

EXTERN_DECLARE_TIME_VARS
class socket_client : public client_core
{
private:
    USOCKET Sock;
     
	protocol_version p_ver;
	
	read_msg_states read_msg_count;
	bool has_next_item;
    bool is_on_stop;
	char query_string[SE_SOCKET_MSG_BUF_SIZE+1];
	
	se_socketostream *stream;
    char* long_query_stream;
	
    struct timeval timeout;
public:
    socket_client();
 //   ~socket_client() {}
    virtual void process_unknown_instruction(int instruction, bool in_transaction);
	
    virtual void init();
    virtual void release();
    virtual void read_msg(msg_struct *msg);
    virtual char* get_query_string(msg_struct *msg);
    virtual QueryType get_query_type();
    virtual t_print get_result_type(msg_struct *msg);
    virtual se_ostream* get_se_ostream();
    virtual client_file get_file_from_client(const char* filename);
    virtual void close_file_from_client(client_file cf);
    virtual void get_session_parameters();
    virtual void respond_to_client(int instruction);
    virtual void begin_item();
    virtual void end_of_item(bool res);
    virtual bool is_print_progress() { return false; }
/*    
    virtual void update_result(bool res);
    virtual void bulk_load_result(bool res, const std::string& body);
    virtual void begin_tr_result(bool res, const std::string& body);
    virtual void commit_tr_result(bool res, const std::string& body);
    virtual void rollback_tr_result(bool res, const std::string& body);
    virtual void rollback_tr_before_close(bool res, const std::string& body);
    virtual void close_session_result(bool res, const std::string& body);
*/
    virtual void authentication_result(bool res, const std::string& body);
    virtual void error(int code, const std::string& body);
    virtual void error();
    virtual void show_time(std::string qep_time);
    virtual void write_user_query_to_log();
};


#endif

