/*
* File:  socket_client.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _SOCKET_CLIENT_H
#define _SOCKET_CLIENT_H

#include <string>

#include "common/sedna.h"

#include "tr/client_core.h"
#include "tr/crmutils/exec_output.h"
#include "tr/tr_utils.h"

enum client_states 
{
    NO_TRANSACTION, 
    IN_TRANSACTION
};

enum read_msg_states 
{
    se_BeginAuthenticatingTransaction = 3,
    se_Authentication = 2,
    se_CommitAuthenticatingTransaction = 1,
    se_GetNextMessageFromClient = 0 
};

EXTERN_DECLARE_TIME_VARS

class socket_client : public client_core
{
private:
    USOCKET Sock;

    protocol_version p_ver;

    read_msg_states read_msg_count;
    bool has_next_item;
    bool is_on_stop;
    bool recreate_debug_stream;
    char query_string[SE_SOCKET_MSG_BUF_SIZE+1];

    se_ostream *stream;           //current output stream
    se_ostream *dbg_s;            //debug output stream
    se_ostream *nul_s;            //null stream
    se_socketostream *out_s;      //socket output stream

    int max_result_size_to_pass;
    char* long_query_stream;

    struct timeval timeout;
    int os_primitives_id_min_bound;
    int ka_timeout;               //session keep alive timeout

public:
    socket_client();
    //   ~socket_client() {}

    virtual void process_unknown_instruction(int instruction, bool in_transaction);
    virtual void init();
    virtual void release();
    virtual void read_msg(msg_struct *msg);
    virtual char* get_query_string(msg_struct *msg);
    virtual QueryType get_query_type() { return TL_XQuery; }
    virtual void get_file_from_client(std::vector<std::string>* filenames, std::vector<client_file>* cf_vec);
    virtual void close_file_from_client(client_file &inout_cf);
    virtual void get_session_parameters();
    virtual void set_session_options(msg_struct *msg);
    virtual void reset_session_options();
    virtual void respond_to_client(int instruction);
    virtual bool is_print_progress() { return false; }
    virtual int get_os_primitives_id_min_bound() { return os_primitives_id_min_bound; }
    virtual void authentication_result(bool res, const std::string& body);
    virtual void error(int code, const std::string& body);
    virtual void error();
    virtual void show_time(u_timeb qep_time);
    virtual void show_time_ex(uint64_t qep_time);
    virtual void write_user_query_to_log() {}
    virtual void set_keep_alive_timeout(int sec);
    virtual void user_statement_begin();
    
     /* Output managment */
    virtual se_ostream* get_se_ostream()     { return stream;  }
    virtual se_ostream* get_debug_ostream();
    virtual bool disable_output();
    virtual void enable_output()             { stream = out_s; }
    virtual bool is_output_enabled()         { return (stream != NULL && stream == out_s); }
    
    /* Handlers for start/finish of item printing. */
    virtual void begin_item (bool is_atomic, xmlscm_type st, t_item nt, const char* uri);
    virtual void end_item   (qepNextAnswer exist_next);
    
    /* If client supports serialization. 
     * Since protocol version >4 it's client's task
     * to make indentation, space delimiting, etc ...
     */
    virtual bool supports_serialization() { return p_ver.major_version >= 4; }
};


#endif /* _SOCKET_CLIENT_H */
