/*
 * File: cl_client.cpp
 * The Institute for System Programming of the Russian Academy of Sciences
 * Copyright (C) 2013 ISP RAS
 */

#include <string>
#include <list>
#include <fstream>
#include <stdio.h>

#include "common/sedna.h"

#include "common/u/uhdd.h"
#include "common/base.h"
#include "common/ipc_ops.h"
#include "common/errdbg/d_printf.h"

#include "tr/cl_client.h"
#include "tr/tr_globals.h"
#include "tr/tr_common_funcs.h"
#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/exec_output.h"
#include "tr/auth/auc.h"

#define BATCH_DELIMITER "\\"

using namespace std;

command_line_client::command_line_client(int argc, char** argv)
{
    cur_s = NULL;
    dbg_s = NULL;
    nul_s = NULL;
    out_s = NULL;
    recreate_debug_stream = true;
    statements_ready = false;
    output_method = se_output_method_xml;

    /* Load metadata transaction */
    if (tr_globals::first_transaction)
    {
        if (argc != 2)
            throw SYSTEM_EXCEPTION("Bad number of input parameters to load metadata");

        strcpy(tr_globals::db_name,  argv[1]);
        strcpy(tr_globals::filename, "dummy");
    }
    /* General transaction from the command line */
    else
    {
        /* Parse command line */
        parse_trn_command_line(argc, argv);
    }
}


void command_line_client::init()
{
    /* Check the correctness of input parameters from command line */
    if (string(tr_globals::filename) == "???" || string(tr_globals::db_name) == "???")
        throw USER_EXCEPTION(SE4601);

    gov_header_struct cfg;
    get_sednaconf_values(&cfg);

    os_primitives_id_min_bound = cfg.os_primitives_id_min_bound;

    out_s = se_new se_stdlib_ostream(std::cout);
    nul_s = se_new se_nullostream();
    cur_s = out_s;

    /* Initialize commands stack */
    cl_command cmd;

    output_method = se_output_method_xml;

    cmd.type = se_BeginTransaction;
    cmd.length = 0;
    cl_cmds.push_front(cmd);

    cmd.type = se_CommitTransaction;
    cmd.length = 0;
    cl_cmds.push_front(cmd);

    cmd.type = se_Authenticate;
    cmd.length = 0;
    cl_cmds.push_front(cmd);

    cmd.type = se_BeginTransaction;
    cmd.length = 0;
    cl_cmds.push_front(cmd);
}


void command_line_client::release()
{
    if (out_s != NULL) {
        delete out_s;
        out_s = NULL;
    }
    if (dbg_s != NULL) {
        delete dbg_s;
        dbg_s = NULL;
    }
    if (nul_s != NULL) {
        delete nul_s;
        nul_s = NULL;
    }
    cur_s = NULL;
}

void command_line_client::read_msg(msg_struct *msg)
{
	msg->body[0] = se_output_method_xml;
    if (is_stop_session())//session closed forcedly by se_stop utility
    {
        clear_stack_for_stop_signal();
    }

    // we've got security transaction and has already started a new one
    if (!cl_cmds.size() && !statements_ready)
    {
        string plain_batch_text;

        char env_buf[8];
        if (uGetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, env_buf, sizeof(env_buf), __sys_call_error) != 0)
        {
            /* Init output res */
            if (string(tr_globals::output_file) == "STDOUT")
                res_os = stdout;
            else if ((res_os = fopen(tr_globals::output_file, "w")) == NULL)
                throw USER_EXCEPTION2(SE4040, tr_globals::output_file);

            /* Read batch text in string */
            FILE *f;
            if ((f = fopen(tr_globals::filename, "r")) == NULL)
                throw USER_EXCEPTION2(SE4042, tr_globals::filename);

            while(!feof(f)){
                static const size_t rdChunkSz = 0x10000; /* 64 KB */
                size_t rdSz = 0, curSz = 0;

                curSz = plain_batch_text.size();
                plain_batch_text.resize(curSz + rdChunkSz);
                rdSz = fread(&plain_batch_text[curSz], 1, rdChunkSz, f); /* fread NEVER return -1 on error */
                plain_batch_text.resize(curSz + rdSz);
            }
        }
        else /* load metadata transaction is running */
        {
            tr_globals::internal_auth_switch = BLOCK_AUTH_CHECK;

            /* database is created with db-security option != off => we need to load db_security_data */
            if(strcmp(env_buf, "2") == 0)
            {
                string path_to_security_file;
                char path_buf[U_MAX_PATH + 32];
                path_to_security_file = uGetImageProcPath(path_buf, __sys_call_error) + 
                                        string("/../share/") + 
                                        string(INITIAL_SECURITY_METADATA_FILE_NAME);

                for (string::size_type i=0; i<path_to_security_file.size(); i++)
                    if (path_to_security_file[i] == '\\') path_to_security_file[i] = '/';

                plain_batch_text = string("LOAD ") + 
                                   string("'") + path_to_security_file + string("' ") +
                                   string("'") + string(SECURITY_METADATA_DOCUMENT) + string("'") +
                                   string("\n\\\n");
            }

            plain_batch_text += string("CREATE COLLECTION ") + 
                                string("'") + string(MODULES_COLLECTION_NAME) + string("'");
        }

        // here we parse our queries via driver and then get ast-strings
        stmnts_array = parse_xq_to_ast(plain_batch_text.c_str());

        //add 'commit' command if there is not end of transaction (coommit or rollback) command
        if (stmnts_array.back().substr(0, 8).find("rollback") == string::npos &&
            stmnts_array.back().substr(0, 6).find("commit") == string::npos)
            stmnts_array.push_back("commit");

        cl_command cmd;

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
        statements_ready = true;
    }

    msg->instruction = cl_cmds.front().type;
    msg->length = cl_cmds.front().length;
    cl_cmds.pop_front();
}


char* command_line_client::get_query_string(msg_struct *msg)
{
    return (char*)stmnts_array[statement_index++].c_str();
}


QueryType command_line_client::get_query_type()
{
    return TL_ASTInitial;
}

void command_line_client::get_file_from_client(std::vector<string>* filenames, std::vector<client_file>* cf_vec)
{
    std::vector<string>::size_type i;

    try {
        for(i = 0; i < filenames->size(); i++)
        {
            const char* client_filename = filenames->at(i).c_str();
            client_file &cf = cf_vec->at(i);

            if (tr_globals::first_transaction)
            {   /* Load metadata case (client_filename must be absolute path) */
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

            res = uGetAbsoluteFilePath(tr_globals::filename, qfile_abspath, U_MAX_PATH, __sys_call_error);
            if (res == NULL)
                throw USER_EXCEPTION2(SE4603, tr_globals::filename);

            char* new_dir;
            new_dir = uGetDirectoryFromFilePath(qfile_abspath, dir, U_MAX_PATH, __sys_call_error);

            if (uChangeWorkingDirectory(new_dir, __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4604, new_dir);

            res = uGetAbsoluteFilePath(client_filename, cfile_abspath, U_MAX_PATH, __sys_call_error);
            
            if (uChangeWorkingDirectory(cur_dir_abspath, __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4604, cur_dir_abspath);
            if ((cf.f = fopen(cfile_abspath, "r")) == NULL)
            {
                res = uGetAbsoluteFilePath(client_filename, cfile_abspath, U_MAX_PATH, __sys_call_error);
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
        }
    } catch (ANY_SE_EXCEPTION) {
        /* Close all files from cf_vec */
        for (std::vector<string>::size_type j = 0; j < i; j++)
        {
            if (cf_vec->at(j).f && (fclose(cf_vec->at(j).f) != 0))
            {
                cf_vec->at(j).f = NULL;
                throw USER_EXCEPTION(SE3020);
            }
            cf_vec->at(j).f = NULL;
        }
        throw;
    }
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

void command_line_client::end_item(qepNextAnswer exist_next)
{
    if (exist_next == se_next_item_exists)
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

    if (cl_cmds.front().type == se_Execute ||            //exexute query
        cl_cmds.front().type == se_GetNextItem ||        //next portion
        cl_cmds.front().type == se_Authenticate  ||      //authenticate
        cl_cmds.front().type == se_CommitTransaction ||  //commit
        cl_cmds.front().type == se_RollbackTransaction   //rollback
        )
    {   /* Exist not finished transaction
         * put rollback (for not finished transaction) and close session.
         */
        cl_cmds.clear();
        cmd.type = se_CloseConnection;
        cmd.length = 0;
        cl_cmds.push_front(cmd);

        cmd.type = se_RollbackTransaction;
        cmd.length = 0;
        cl_cmds.push_front(cmd);
    }
    else
    {   /* There is not active transaction
         * put close session on stack
         */
        cl_cmds.clear();
        cmd.type = se_CloseConnection;
        cmd.length = 0;
        cl_cmds.push_front(cmd);
    }
}

void command_line_client::show_time(u_timeb qep_time)
{
    d_printf2("Execution time of the latest query %s\n secs", to_string(qep_time).c_str());
}

void command_line_client::write_user_query_to_log()
{
    char buf[1000000];

    if (!tr_globals::first_transaction)
    {
        /* Read batch text in string */
        FILE *f;
        if ((f = fopen(tr_globals::filename, "r")) == NULL)
            throw USER_EXCEPTION2(SE4042, tr_globals::filename);

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

se_ostream*
command_line_client::get_debug_ostream() {

    if (NULL == dbg_s || recreate_debug_stream)
    {
        if(dbg_s != NULL)
        {
            delete dbg_s;
            dbg_s = NULL;
            recreate_debug_stream = false;
        }
        dbg_s = cur_s->get_debug_ostream();
    }
    return dbg_s;
}

bool
command_line_client::disable_output() {
    bool res = is_output_enabled();
    cur_s = nul_s;
    return res;
}

void
command_line_client::user_statement_begin()
{
    enable_output();
    recreate_debug_stream = true;
}
