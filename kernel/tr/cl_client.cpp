/*
 * File:  cl_client.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <list>
#include <fstream>
#include <stdio.h>

#include "tr/cl_client.h"
#include "common/base.h"
#include "common/ipc_ops.h"
#include "tr/tr_globals.h"
#include "tr/tr_functions.h"
#include "common/u/uhdd.h"
#include "common/version.h"

#define BATCH_DELIMITER "\\"

using namespace std;


command_line_client::command_line_client(int argc, char** argv)
{
  char buf[1024];

  if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) == 0)
  {//!!! metadata transaction case !!!
     if (argc != 2)
        throw SYSTEM_EXCEPTION("Bad number of input parameters to load metadata");

     strcpy(db_name, argv[1]);
     strcpy(filename, "dummy");
     strcpy(q_type, "XQuery");
     s = NULL;

  }
  else
  {
     //parse command line arguments
     if (is_command_line_args_length_overflow(argc, argv))
        throw USER_EXCEPTION(SE4600);

     if (argc == 1)
        print_tr_usage();
     else
     {
        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, tr_argtable, narg, NULL, errmsg, NULL);
        if (tr_s_help == 1 || tr_l_help == 1) print_tr_usage();
        if (tr_version == 1) { print_version_and_copyright("Sedna Transaction"); throw USER_SOFT_EXCEPTION(""); }
        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);
     }
     s = NULL;
     // call session options object method: 
     int option = debug_mode ? SEDNA_DEBUG_ON : SEDNA_DEBUG_OFF;
     dynamic_context::set_session_option(se_debug_mode, (void*)&option, sizeof(int));
  }
}


void command_line_client::init()
{
//  u_timeb ttt1, ttt2;
//  u_ftime(&ttt1);

   //check the correctness of input parameters from command line
   if (string(filename) == "???" || string(db_name) == "???")
     throw USER_EXCEPTION(SE4601);


   if (string(q_type) == "XQuery") query_type = TL_XQuery;
   else if (string(q_type) == "POR") query_type = TL_POR;
   else if (string(q_type) == "LR") query_type = TL_ForSemAnal;
   else throw USER_EXCEPTION(SE4002);

   gov_header_struct cfg;
   get_default_sednaconf_values(&cfg);
   get_gov_config_parameters_from_sednaconf(&cfg);//get config parameters from sednaconf

   os_primitives_id_min_bound = cfg.os_primitives_id_min_bound;

   string plain_batch_text;

   char env_buf[8];
   if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, env_buf, 1024, __sys_call_error) != 0)
   {
      //init output res
      if (string(output_file) == "STDOUT") res_os = stdout;
      else if ((res_os = fopen(output_file, "w")) == NULL) throw USER_EXCEPTION2(SE4040, output_file);


      //read batch text in string
      FILE *f;
      if ((f = fopen(filename, "r")) == NULL)
        throw USER_EXCEPTION2(SE4042, filename);

      while(!feof(f)){
		  static const size_t rdChunkSz = 0x10000; /* 64 KB */ 
		  size_t rdSz = 0, curSz = 0;

		  curSz = plain_batch_text.size();
		  plain_batch_text.resize(curSz + rdChunkSz);
		  rdSz = fread(&plain_batch_text[curSz], 1, rdChunkSz, f); /* fread NEVER return -1 on error */ 
		  plain_batch_text.resize(curSz + rdSz);
      }
   }
   else 
   {
       plain_batch_text = string("CREATE COLLECTION ") + string("\"") + string(MODULES_COLLECTION_NAME) + string("\"");
       if(strcmp(env_buf, "2") == 0) // database is created with db-security option != off => we need to load db_security_data
       {
           string path_to_security_file; 
           char path_buf[U_MAX_PATH + 32];
           path_to_security_file = uGetImageProcPath(path_buf, __sys_call_error) + string("/../share/") + string(INITIAL_SECURITY_METADATA_FILE_NAME);

#ifdef _WIN32
           for (int i=0; i<path_to_security_file.size(); i++)
              if (path_to_security_file[i] == '\\') path_to_security_file[i] = '/';
/*
MG: now metadata is stored locally in sedna/share/sedna_auth_md.xml
#else
       if(!uIsFileExist(path_to_security_file.c_str(), __sys_call_error))
          path_to_security_file = string("/usr/share/sedna-") + SEDNA_VERSION + "." + SEDNA_BUILD +string("/sedna_auth_md.xml");*/
#endif



            plain_batch_text += string("\n\\\n") +
                                string("LOAD ") +
                                string("\"") + path_to_security_file + string("\" ") +
                                string("\"") + string(SECURITY_METADATA_DOCUMENT) + string("\"");
       }
   }
   stmnts_array = parse_batch(query_type, plain_batch_text.c_str());


   //add 'coomit' command if there is not end of transaction (coommit or rollback) command
   if (stmnts_array.back().substr(0, 8).find("rollback") == string::npos &&
       stmnts_array.back().substr(0, 6).find("commit") == string::npos)
       stmnts_array.push_back("commit");

   cl_command cmd;

   //!!!init stack!!!
   //put close session on buttom
   cmd.type = se_CloseConnection;
   cmd.length = 0;
   cl_cmds.push_front(cmd);

   //put terminate transaction cmd
   if (stmnts_array.back().substr(0, 8).find("rollback") == string::npos &&
       stmnts_array.back().substr(0, 6).find("commit") == string::npos)
   {
      cmd.type = 220;//commit
   }
   else //commit or rollback exists in script
   {
      if (stmnts_array.back().substr(0, 6).find("commit") != string::npos)
         cmd.type = se_CommitTransaction;//commit
      else
         cmd.type = se_RollbackTransaction;//rollback

      stmnts_array.pop_back();//delete terminate command
   }

   cmd.length = 0;
    
   cl_cmds.push_front(cmd);

   //put queries to stack   
   for (int i=stmnts_array.size()-1; i>=0; i--)
   {
      cmd.type = se_Execute;
      cmd.length = stmnts_array[i].size();
      cl_cmds.push_front(cmd);

   }

   statement_index = 0;
   //put tr begin
   cmd.type = se_BeginTransaction;
   cmd.length = 0;
   cl_cmds.push_front(cmd);

   //put authenticate transaction on top of stack

   cmd.type = se_CommitTransaction;//commit tr
   cmd.length = 0;
   cl_cmds.push_front(cmd);

   cmd.type = se_Authenticate;//authenticate
   cmd.length = 0;
   cl_cmds.push_front(cmd);

   cmd.type = se_BeginTransaction;//begin tr
   cmd.length = 0;
   cl_cmds.push_front(cmd);
   
   s = se_new se_stdlib_ostream(std::cout);
//  u_ftime(&ttt2);
//  cerr << "init!!!!!!!!!!!!: " << to_string(ttt2 - ttt1).c_str() << endl;
}


void command_line_client::release()
{
   if (s != NULL) 
   {
       delete s;
       s = NULL;
   }
}

void command_line_client::read_msg(msg_struct *msg)
{
  if (is_stop_session())//session closed forcedly by se_stop utility
  {
     clear_stack_for_stop_signal();
  }

  msg->instruction = cl_cmds.front().type;
  msg->length = cl_cmds.front().length;
  cl_cmds.pop_front();
}

char* command_line_client::get_query_string(msg_struct *msg)
{
   return (char*)stmnts_array[statement_index++].c_str();
}

t_print command_line_client::get_result_type(msg_struct *msg)
{
	return xml;
}


QueryType command_line_client::get_query_type()
{
  if (query_type == TL_POR) return TL_POR;
  else return TL_ForSemAnal;
}

se_ostream* command_line_client::get_se_ostream()
{
  return s;
}

void command_line_client::get_file_from_client(std::vector<string>* filenames, std::vector<client_file>* cf_vec)
{
    int i;

    try {
        for(i=0; i<filenames->size(); i++)
        {
            char buf[1024];
            const char* client_filename = filenames->at(i).c_str();
            client_file &cf = cf_vec->at(i);
            
            if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) == 0)
            {//load metadata case (client_filename must be absolute path)
                if ((cf.f = fopen (client_filename, "r")) == NULL)
                    throw USER_EXCEPTION2(SE4042, client_filename);
                if (uGetFileSizeByName(client_filename, &(cf.file_size), __sys_call_error) == 0)
                    throw USER_EXCEPTION2(SE4050, client_filename);
                strcpy(cf.name, client_filename);
                return;
            }
            
            char cur_dir_abspath[U_MAX_PATH];
            char qfile_abspath[U_MAX_PATH];
            char cfile_abspath[U_MAX_PATH];
            char dir[U_MAX_PATH];
            char *res = NULL;
            
            res = uGetCurrentWorkingDirectory(cur_dir_abspath, U_MAX_PATH, __sys_call_error);
            if (res == NULL)
                throw USER_EXCEPTION(SE4602);
            
            res = uGetAbsoluteFilePath(filename, qfile_abspath, U_MAX_PATH, __sys_call_error);
            if (res == NULL)
                throw USER_EXCEPTION2(SE4603, filename);
            
            char* new_dir;
            new_dir = uGetDirectoryFromFilePath(qfile_abspath, dir, U_MAX_PATH, __sys_call_error);
            
            if (uChangeWorkingDirectory(new_dir, __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4604, new_dir);
            
            res = uGetAbsoluteFilePath(client_filename, cfile_abspath, U_MAX_PATH, __sys_call_error);
/*if (res == NULL)
     throw USER_EXCEPTION2(SE4603, client_filename);
*/
            if (uChangeWorkingDirectory(cur_dir_abspath, __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4604, cur_dir_abspath);
            if ((cf.f = fopen(cfile_abspath, "r")) == NULL)
            {
                res = uGetAbsoluteFilePath(client_filename, cfile_abspath, U_MAX_PATH, __sys_call_error);
/*     if (res == NULL)
        throw USER_EXCEPTION2(SE4603, client_filename);
*/
                if ((cf.f = fopen(cfile_abspath, "r")) == NULL)
                    throw USER_EXCEPTION2(SE4042, client_filename);
                if (uGetFileSizeByName(cfile_abspath, &(cf.file_size), __sys_call_error) == 0)
                    throw USER_EXCEPTION2(SE4050, client_filename);
            }
            else
            {
                if (uGetFileSizeByName(cfile_abspath, &(cf.file_size), __sys_call_error) == 0)
                    throw USER_EXCEPTION2(SE4050, client_filename);
            }
            strcpy(cf.name, client_filename);
        } //for
           
  } catch (...) {
    // close all files from cf_vec
    for (int j=0; j<i; j++)
    {
        if (cf_vec->at(i).f && (fclose(cf_vec->at(i).f) != 0))
        {
        cf_vec->at(i).f = NULL;
        throw USER_EXCEPTION(SE3020);
        }  
        cf_vec->at(i).f = NULL;  
    }
    throw;
  } //try       

}

void command_line_client::close_file_from_client(client_file &cf)
{
    if (cf.f && (fclose(cf.f) != 0))
    {
        cf.f = NULL;
        throw USER_EXCEPTION(SE3020);
    }
    cf.f = NULL;
}

void command_line_client::respond_to_client(int instruction)
{
    switch (instruction)
    {
        case se_UpdateSucceeded:
            fprintf(res_os, "\nUPDATE is executed successfully\n");
            break;
        case se_BulkLoadSucceeded:
            d_printf1("\nBulk load succeeded\n");
            break;
        case se_BeginTransactionOk:
            d_printf1("\nTr is started successfully\n");
            break;
        case se_CommitTransactionOk:
            d_printf1("\nTr is committed successfully\n");
            break;
        case se_RollbackTransactionOk:
            d_printf1("\nTr is rolled back successfully\n");
            break;
        case se_TransactionRollbackBeforeClose:
            d_printf1("\nTr is rolled back successfully\n");
            d_printf1("\nSession is closed successfully\n");
            break;
        case se_CloseConnectionOk:
            d_printf1("\nSession is closed successfully\n");
            break;
        default:
            d_printf2("\nUnknown instruction = %d\n", instruction);
            break; 
    }
}

void command_line_client::begin_item()
{
}

void command_line_client::end_of_item(bool exist_next)
{
  
  if (exist_next)
  {//put next portion on top of stack
     cl_command cmd;
     cmd.type = se_GetNextItem;
     cmd.length = 0;
     cl_cmds.push_front(cmd);
  }
}

void command_line_client::authentication_result(bool res, const string& body)
{
   if (res)
      d_printf1("\nAuthentication is passed successfully\n");
   else
      d_printf2("\nAuthentication failed: %s\n", body.c_str()); 
}




void command_line_client::get_session_parameters()
{
}

void command_line_client::set_session_options(msg_struct *msg)
{
    // call static some object method to set session options
}

void command_line_client::reset_session_options()
{
    //obj.reset_options();
}

void command_line_client::process_unknown_instruction(int instruction, bool in_transaction)
{
    throw USER_ENV_EXCEPTION("Command line client got unknown instruction.", true);
}


void command_line_client::error(int code, const string& body)
{
  //erase all commands from stack and push close session to stack
  //  (last transaction to this point already finished)
  cl_cmds.clear();

  cl_command cmd;
  cmd.type = se_CloseConnection;
  cmd.length = 0;

  cl_cmds.push_front(cmd);
}

void command_line_client::clear_stack_for_stop_signal()
{
   cl_command cmd;

   if (cl_cmds.front().type == se_Execute || //exexute query
       cl_cmds.front().type == se_GetNextItem || //next portion
       cl_cmds.front().type == se_Authenticate  || //authenticate
       cl_cmds.front().type == se_CommitTransaction || //commit
       cl_cmds.front().type == se_RollbackTransaction    //rollback
      )
   {//exist not finished transaction
    //put rollback (for not finished transaction) and close session
      cl_cmds.clear();
      cmd.type = se_CloseConnection;
      cmd.length = 0;
      cl_cmds.push_front(cmd);

      cmd.type = se_RollbackTransaction;
      cmd.length = 0;
      cl_cmds.push_front(cmd);
   }
   else
   {//there is not active transaction
    //put close session on stack
      cl_cmds.clear();
      cmd.type = se_CloseConnection;
      cmd.length = 0;
      cl_cmds.push_front(cmd);
   }
}

void command_line_client::show_time(string qep_time)
{
	d_printf2("Execution time of the latest query %s\n",qep_time.c_str());
}


void command_line_client::write_user_query_to_log()
{
   char buf[1000000];

   if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, buf, 1024, __sys_call_error) != 0)
   {

      //read batch text in string

      FILE *f;
      if ((f = fopen(filename, "r")) == NULL)
         throw USER_EXCEPTION2(SE4042, filename);

      string plain_batch_text;
      plain_batch_text.reserve(100000);

      while(!feof(f))
      {
         size_t len= fread(buf, sizeof(char), sizeof(buf), f);
         plain_batch_text.append(buf, len);
      }

      elog_long(EL_LOG, "User's query:\n", plain_batch_text.c_str());
   }

}
