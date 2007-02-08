/*
 * File:  cl_client.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CL_CLIENT_H
#define _CL_CLIENT_H

#include "common/sedna.h"
#include <vector>
#include <string>
#include "tr/crmutils/exec_output.h"
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
  std::list<cl_command> cl_cmds;//stack of client commands to be executed
  StringVector stmnts_array;    //vector of statements to be executed
  int statement_index;
  se_ostream *s;

  void clear_stack_for_stop_signal();

public:
  command_line_client(int argc, char** argv);
  virtual ~command_line_client() {} 
  virtual void init();
  virtual void release();
  virtual void read_msg(msg_struct *msg);
  virtual char* get_query_string(msg_struct *msg);
  virtual t_print get_result_type(msg_struct *msg);
  virtual QueryType get_query_type();
  virtual se_ostream* get_se_ostream();
  virtual void get_file_from_client(std::vector<std::string>* filenames, std::vector<client_file>* cf_vec);
  virtual void close_file_from_client(client_file &inout_fs);
  virtual void respond_to_client(int instruction);
  virtual void begin_item();
  virtual void end_of_item(bool exist_next);
  virtual bool is_print_progress() { return true; }
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
  virtual void get_session_parameters();
  virtual void set_session_options(msg_struct *msg);
  virtual void reset_session_options();
  virtual void process_unknown_instruction(int instruction, bool in_transaction);
  virtual void error(int code, const std::string& body);
  
  virtual void show_time(std::string qep_time);

  virtual void write_user_query_to_log();
};


#endif
