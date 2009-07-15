/*
* File:  client_core.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#ifndef _CLIENT_CORE_H
#define _CLIENT_CORE_H

#include <string>
#include <vector>

#include "common/sedna.h"
#include "common/u/uhdd.h"
#include "common/base.h"

#include "tr/crmutils/crmbase.h"

struct client_file{
    FILE* f;
    char name[1024];
    __int64 file_size;
};

class client_core
{
public:
    //virtual ~client_core() {}

    virtual void init() = 0;
    virtual void release() = 0;
    virtual void read_msg(msg_struct *msg) = 0;
    virtual char* get_query_string(msg_struct *msg) = 0;
    virtual QueryType get_query_type() = 0;
    virtual void get_session_parameters() = 0;
    virtual void set_session_options(msg_struct *msg) = 0;
    virtual void reset_session_options() = 0;
    virtual void get_file_from_client(std::vector<std::string>* filenames, std::vector<client_file>* cf_vec) = 0;
    virtual void close_file_from_client(client_file &inout_cf) = 0;
    virtual void respond_to_client(int instruction) = 0;
    virtual void end_of_item(qepNextAnswer exist_next) = 0;
    virtual bool is_print_progress() = 0;
    virtual int get_os_primitives_id_min_bound() = 0;
    virtual void authentication_result(bool res, const std::string& body) = 0;
    virtual void process_unknown_instruction(int instruction, bool in_transaction) = 0;
    virtual void error(int code, const std::string& body) = 0;
    virtual void show_time(std::string qep_time) = 0;
    virtual void write_user_query_to_log() = 0;
    virtual void set_keep_alive_timeout(int sec) = 0;

    /* Returns current output stream. If output is disabled 
     * returns null stream.
     */
    virtual se_ostream* get_se_ostream() = 0;
    
    /* Returns current debug output stream. If output is disabled 
     * returns null stream.
     */
    virtual se_ostream* get_debug_ostream() = 0;

    /* Disables/Enables output. Each call may invalidate 
     * stream returned by get_se_ostream call.
     * disable_output() returns previous state.
     */
    virtual bool disable_output() = 0;
    virtual void enable_output()  = 0;
    
    /* Returns if output is enabled */
    virtual bool is_output_enabled() = 0;
    
    /* Get or set serialization type,
     * e.g. XML, SXML, etc ... 
     */
    virtual t_print get_result_type() = 0;
    virtual void set_result_type(msg_struct *msg) = 0;
    
    /* Should be called just before run execution of each 
     * statement. May clear some internal state (e.g. enable 
     * output or force recreation of the debug stream).
     */
    virtual void user_statement_begin() = 0;
};

#endif /* _CLIENT_CORE_H */
