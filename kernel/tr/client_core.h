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

#include "tr/crmutils/crmbase.h"
#include "tr/crmutils/global_options.h"
#include "tr/tr_base.h"

struct client_file
{
    std::istream * stream;
    std::string name;
    int64_t size;

    client_file() : stream(NULL) {};
    ~client_file() { delete stream; };
};

class Serializer;

class client_core
{
protected:
    enum se_output_method output_method;

public:
    virtual ~client_core() {}

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
    virtual bool is_print_progress() = 0;
    virtual int  get_os_primitives_id_min_bound() = 0;
    virtual void authentication_result(bool res, const std::string& body) = 0;
    virtual void process_unknown_instruction(int instruction, bool in_transaction) = 0;
    virtual void error(int code, const std::string& body) = 0;
    virtual void show_time(u_timeb qep_time) = 0;
    virtual void show_time_ex(uint64_t qep_time) = 0;
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
    virtual void set_result_type(enum se_output_method method) { output_method = method; }
    virtual enum se_output_method get_result_type() { return output_method; }
//    virtual enum se_output_method get_result_type() { return se_output_method_sxml; }
    
    /* Should be called just before run execution of each 
     * statement. May clear some internal state (e.g. enable 
     * output or force recreation of the debug stream).
     */
    virtual void user_statement_begin() = 0;
    
    /* Handlers for start/finish of item printing.
     * st  - XMLSchema type of the node or atomic value
     * nt  - node type (i.e. element, attribute, etc ...)
     * uri - either namespace URI for attribute nodes, or
     *       document URI for document nodes
     */
    virtual void begin_item (bool is_atomic, xmlscm_type st, t_item nt, const char* uri) = 0;
    virtual void end_item   (qepNextAnswer exist_next) = 0;

    /* If client supports serialization by itself or not. 
     * For command line client this value is always false 
     * which means that we do indentation at trn's side. 
     * For socket client this value depends on the protocol 
     * version.
     */
     virtual bool supports_serialization() = 0;
};

#endif /* _CLIENT_CORE_H */
