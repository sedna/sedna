/*
* File:  cl_client.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _CL_CLIENT_H
#define _CL_CLIENT_H

#include <vector>
#include <string>

#include "common/sedna.h"
#include "tr/client_core.h"
#include "tr/xqp/XQuerytoLR.h"

class cl_command
{
public:
    int type;
    int length;
};

class command_line_client : public client_core
{
private:
    std::list<cl_command> cl_cmds;   //stack of client commands to be executed
    StringVector stmnts_array;       //vector of statements to be executed
    int statement_index;
    se_ostream *cur_s;               //current output stream
    se_ostream *dbg_s;               //debug output stream
    se_ostream *nul_s;               //null stream
    se_ostream *out_s;               //stdlib output stream
    int os_primitives_id_min_bound;
    bool recreate_debug_stream;

    bool statements_ready;

    void clear_stack_for_stop_signal();

public:
    command_line_client(int argc, char** argv);
    virtual ~command_line_client() {} 

    virtual void init();
    virtual void register_session_on_gov();
    virtual void unregister_session_on_gov();
    
    virtual void release();
    virtual void read_msg(msg_struct *msg);
    virtual char* get_query_string(msg_struct *msg);
    virtual QueryType get_query_type();
    virtual void get_file_from_client(std::vector<std::string>* filenames, std::vector<client_file>* cf_vec);
    virtual void close_file_from_client(client_file &inout_fs);
    virtual void respond_to_client(int instruction);
    virtual bool is_print_progress() { return true; }
    virtual int  get_os_primitives_id_min_bound() { return os_primitives_id_min_bound; }
    virtual void authentication_result(bool res, const std::string& body);
    virtual void process_unknown_instruction(int instruction, bool in_transaction);
    virtual void error(int code, const std::string& body);
    virtual void show_time(u_timeb qep_time);
    virtual void show_time_ex(uint64_t qep_time);
    virtual void write_user_query_to_log();
    virtual void user_statement_begin();

    /* Do nothing in command line mode */
    virtual void get_session_parameters()              {}
    virtual void set_session_options(msg_struct *msg)  {}
    virtual void reset_session_options()               {}
    virtual void set_keep_alive_timeout(int sec)       {}

    /* Output managment */
    virtual se_ostream* get_se_ostream()     { return cur_s;  }
    virtual se_ostream* get_debug_ostream();
    virtual bool disable_output();
    virtual void enable_output()             { cur_s = out_s; }
    virtual bool is_output_enabled()         { return (cur_s != NULL && cur_s == out_s); }
    
     /* Handlers for start/finish of item printing. */
    virtual void begin_item (bool is_atomic, xmlscm_type st, t_item nt, const char* uri) {}
    virtual void end_item   (qepNextAnswer exist_next);

    /* If client supports serialization. */
    virtual bool supports_serialization()    { return false; }
};

#endif /* _CL_CLIENT_H */
