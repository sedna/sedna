/*
* File:  socket_client.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>
#include <list>
#include <fstream>
#include <sstream>
#include <stdio.h>

#include "common/sedna.h"
#include "common/base.h"
#include "common/u/uhdd.h"
#include "common/u/uutils.h"
#include "common/u/uprocess.h"
#include "common/utils.h"
#include "common/errdbg/d_printf.h"

#include "tr/executor/base/PPBase.h"
#include "tr/socket_client.h"
#include "tr/tr_globals.h"
#include "tr/tr_common_funcs.h"
#include "tr/auth/auc.h"
#include "tr/tr_utils.h"

static msg_struct sp_msg;
using namespace std;


#define THROW_SOCKET_EXCEPTION(code)  {client_sock = U_INVALID_SOCKET; throw USER_EXCEPTION2((code), usocket_error_translator());}

socket_client::socket_client()
{
    p_ver.major_version = 5;
    p_ver.minor_version = 0;	

    read_msg_count = se_BeginAuthenticatingTransaction;
    has_next_item = true;
    is_on_stop = false;

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    client_sock = U_INVALID_SOCKET;
    stream = NULL;
    out_s = NULL;
    dbg_s = NULL;
    nul_s = NULL;
    max_result_size_to_pass = 0; //can be sent as a session option; 0 - pass whole result
    long_query_stream = NULL;
    recreate_debug_stream = true;
    output_method = se_output_method_xml;
    
    gov_sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
    if(U_SOCKET_ERROR == gov_sock) throw USER_EXCEPTION (SE3001);

    while(0 != uconnect_tcp(gov_sock, tr_globals::socket_port, tr_globals::gov_address, __sys_call_error))
    {
        if(!utry_connect_again())
        {
            ushutdown_close_socket(gov_sock, __sys_call_error);
            throw USER_EXCEPTION (SE3003);
        }
#ifdef _WIN32
#else
        if(ushutdown_close_socket(gov_sock, __sys_call_error)!=0) throw USER_EXCEPTION (SE3011);
        gov_sock = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(gov_sock == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);
#endif
    }
    govCommunicator = new MessageExchanger(gov_sock);
    
}

void socket_client::init()
{
//   !TODO: delete this when trn will be stable
    char buffer[ENV_BUF_SIZE + 1];

    if (uGetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, buffer, ENV_BUF_SIZE, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4073, SEDNA_OS_PRIMITIVES_ID_MIN_BOUND);

    os_primitives_id_min_bound = atoi(buffer);
}

void socket_client::register_session_on_gov()
{
    UPID s_pid  = uGetCurrentProcessId(__sys_call_error);
   
    govCommunicator->beginSend(se_RegisterNewSession);
    govCommunicator->writeString(tr_globals::db_name);
    govCommunicator->writeInt32(s_pid);
    govCommunicator->writeChar(tr_globals::first_transaction ? 1 : 0);
    govCommunicator->endSend();
    
    while(!govCommunicator->receive());

    if(govCommunicator->getInstruction() == se_TrnRegisterOK)
    {
        /* Trn registered on gov successfully, get  
         * sid (session identificator) is a global parameter */

        tr_globals::sid = govCommunicator->readInt32();
#ifndef _WIN32        
        USOCKET unix_socket = usocket(AF_UNIX, SOCK_STREAM, 0, __sys_call_error);
        client_sock = U_INVALID_SOCKET;
        
        char socket_unix_address[14];
        memset (socket_unix_address, 0, 14);
        memcpy (socket_unix_address, "sdn", strlen("sdn"));
        
        u_itoa(tr_globals::sid, socket_unix_address + strlen("sdn"), 10);
       
        if(uconnect_unix(unix_socket, socket_unix_address, __sys_call_error) != 0)
        {
           ushutdown_close_socket(unix_socket, __sys_call_error);
           throw USER_EXCEPTION2 (SE3003, usocket_error_translator());
        }

        MessageExchanger * socket_communicator = new MessageExchanger(unix_socket);
        
        socket_communicator->beginSend(se_ReceiveSocket);
        socket_communicator->writeInt32(tr_globals::sid);
        socket_communicator->endSend();
        
        if (0 != socket_communicator->receiveSocket(&client_sock)) throw USER_EXCEPTION2(SE3007, usocket_error_translator());
        d_printf2("Client socket received. It is %d \n", client_sock);
#else
        if (0 != govCommunicator->receiveSocket(&client_sock)) throw USER_EXCEPTION2(SE3007, usocket_error_translator());
        d_printf2("Client socket received. It is %d \n", client_sock);
#endif
        clientCommunicator = new MessageExchanger(client_sock);
    }
    
    if(govCommunicator->getInstruction() == se_TrnRegisterFailedNotRunningOrSpecialMode)
    {
        /* Database is not started or running in special mode*/
        ushutdown_close_socket(gov_sock, __sys_call_error);
        throw USER_EXCEPTION2(SE4409,tr_globals::db_name);
    }
    
    if(govCommunicator->getInstruction() == se_TrnRegisterOKFirstTransaction)
    {
        /* we cannot reach this point in socket_client; I've left it to be able to catch errors */
        throw USER_EXCEPTION2(SE4409,tr_globals::db_name);
    }
    
    if(govCommunicator->getInstruction() == se_TrnRegisterFailedMaxSessLimit)
    {
        /* Currently there are maximum number of session in the system */
        ushutdown_close_socket(gov_sock, __sys_call_error);
        throw USER_EXCEPTION(SE3046);
    }
}


void socket_client::unregister_session_on_gov()
{
    UPID s_pid  = uGetCurrentProcessId(__sys_call_error);

    govCommunicator->beginSend(se_UnRegisterSession);
    govCommunicator->writeString(tr_globals::db_name);
    govCommunicator->writeInt32(tr_globals::sid);
    
    govCommunicator->endSend();
    
    while(!govCommunicator->receive());
}


void socket_client::release()
{
    if(client_sock != U_INVALID_SOCKET)
        if(ushutdown_close_socket(client_sock, __sys_call_error)!=0) client_sock = U_INVALID_SOCKET;
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
    stream = NULL;
    if(long_query_stream != NULL) {
        se_free( long_query_stream );
        long_query_stream = NULL;
    }
}

void socket_client::read_msg(msg_struct *msg)
{
    int res;
    int timeout_counter = 0;

    if(is_stop_session())            //session closed forcibly by stop_serv utility
    {
        (*msg).instruction = se_CloseConnection; //close session
        (*msg).length = 0;
        is_on_stop = true;
    }
    else if(read_msg_count == se_BeginAuthenticatingTransaction)  // emulate BeginTransaction
    {
        (*msg).instruction = se_BeginTransaction;
        (*msg).length = 0;
    }
    else if(read_msg_count == se_Authentication)      // process authentication
    {
        (*msg).instruction = se_Authenticate;   // Internal code for authentication
        (*msg).length = 0; 
    }
    else if(read_msg_count == se_CommitAuthenticatingTransaction)      // emulate CommitTransaction
    {
        (*msg).instruction = se_CommitTransaction;  //CommitTransaction
        (*msg).length = 0;  
    }
    else if(read_msg_count == se_GetNextMessageFromClient)      // read next message from client
    {	
        while(1)
        {
            if(is_stop_session())            // emulate close connection without sending message to client
            {
                (*msg).instruction = se_CloseConnection; //close session
                (*msg).length = 0;
                is_on_stop = true;
                return;
            }

            timeout.tv_sec  = 1;
            timeout.tv_usec = 0;
            res = uselect_read(client_sock, &timeout, __sys_call_error);

            if(0 != this->ka_timeout)
           	{
                timeout_counter++;
                if(timeout_counter >= this->ka_timeout) {client_sock = U_INVALID_SOCKET; throw USER_EXCEPTION(SE4624);}
           	}

            if(res == 1) //ready to recv data
            {
                timeout_counter = 0;
                break;
            }
            else if(res == U_SOCKET_ERROR) THROW_SOCKET_EXCEPTION(SE3007);
        }

        res = sp_recv_msg(client_sock, msg);
        
        if(res == U_SOCKET_ERROR) 
            THROW_SOCKET_EXCEPTION(SE3007);

        if(res == 1) 
            throw USER_EXCEPTION(SE3012);
    }
}

char* socket_client::get_query_string(msg_struct *msg)
{
    uint32_t query_portion_length;
    int res;
    size_t query_size = 0;
    bool query_too_large = false;
    size_t malloced_size = SE_SOCKET_MSG_BUF_SIZE*5;

    if(long_query_stream != NULL)
   	{
        se_free( long_query_stream );
        long_query_stream = NULL;
   	}

    try{
        if((*msg).instruction == se_ExecuteLong) // get long query
        {
            if ((long_query_stream = (char*)se_alloc(malloced_size+1)) == NULL) throw USER_EXCEPTION(SE4080);

            while((*msg).instruction != se_LongQueryEnd) // while not the end of long query
            {
                if (!query_too_large)
                {
                    net_int2int(&query_portion_length, (*msg).body+2);
                    if ( (query_size + query_portion_length) > malloced_size )
                    {
                        if ( ( long_query_stream = (char*)se_realloc( long_query_stream,  malloced_size + SE_SOCKET_MSG_BUF_SIZE*2 + 1 )) == NULL)
                            throw USER_EXCEPTION(SE4080);
                        malloced_size += SE_SOCKET_MSG_BUF_SIZE*2;
                    }
                    memcpy(long_query_stream+query_size, (*msg).body+6, query_portion_length);
                    query_size += query_portion_length;

                    if(query_size > SE_MAX_QUERY_SIZE) query_too_large = true;
                }
                res = sp_recv_msg(client_sock, msg);
                if (res == U_SOCKET_ERROR) THROW_SOCKET_EXCEPTION(SE3007);
                if (res == 1) throw USER_EXCEPTION(SE3012);
            }
            if (query_too_large) throw USER_EXCEPTION(SE6000); // statement is too large

            long_query_stream[query_size] = '\0';
            return long_query_stream;
        }
        else						// get one-socket-msg-length query
        {
            net_int2int(&query_portion_length, (*msg).body+2);
            memcpy(query_string, (*msg).body+6, query_portion_length);
            query_string[query_portion_length] = '\0';
            return query_string;
        }
    }catch(ANY_SE_EXCEPTION){
        if( long_query_stream != NULL)
        {
            se_free( long_query_stream );
            long_query_stream = NULL;
        }
        throw;
    }
}

/*
void
socket_client::set_result_type(msg_struct *msg) {
    print_type = (enum se_output_method) (*msg).body[0];
}
*/

void 
socket_client::get_file_from_client(std::vector<string>* filenames, 
                                    std::vector<client_file>* cf_vec)
{
    string tmp_file_path_str;
    unsigned int i = 0;
    int got, res;
    unsigned int written = 0;

    
    if ((filenames->size() > 1) && (p_ver.major_version < 2))
        throw USER_EXCEPTION2(SE2999, "Loading module from multiple files is not supported by current Sedna Client-server protocol.");

    try
    {
        while(i<filenames->size())
        {
            file_struct fs;
            client_file &cf = cf_vec->at(i);
            const char* client_filename = filenames->at(i).c_str();

            if (strcmp(client_filename, "/STDIN/") == 0)
            {
                sp_msg.instruction = 431;// BulkLoadFromStream
                sp_msg.length = 0;
                if(sp_send_msg(client_sock, &sp_msg) != 0) THROW_SOCKET_EXCEPTION(SE3006);
            }
            else
            {
                int filename_len = strlen(client_filename);
                sp_msg.instruction = 430;// BulkLoadFileName
                sp_msg.length = filename_len + 5;

                int2net_int(filename_len, sp_msg.body+1);
                sp_msg.body[0] = 0;
                memcpy(sp_msg.body+5, client_filename, filename_len);
                if(sp_send_msg(client_sock, &sp_msg) != 0) THROW_SOCKET_EXCEPTION(SE3006);
            }

            /* Create tmpfile for bulkload */
            tmp_file_path_str = string(SEDNA_DATA) + string("/data/") + string(tr_globals::db_name) + string("_files");
            res = uGetUniqueFileStruct(tmp_file_path_str.c_str(), &fs, tr_globals::sid, __sys_call_error);
            if(res == 0)
                throw USER_EXCEPTION(SE4052);

            res = sp_recv_msg(client_sock, &sp_msg);
            if(res == U_SOCKET_ERROR) THROW_SOCKET_EXCEPTION(SE3007);
            if(res == 1) throw USER_EXCEPTION(SE3012);

            try {
                while(sp_msg.instruction != se_BulkLoadEnd)           // while not BulkLoadEnd message
                {
                    if (sp_msg.instruction == se_BulkLoadError)       // BulkLoadError
                    {
                        throw USER_EXCEPTION(SE3013);
                    }
                    else if (sp_msg.instruction == se_BulkLoadPortion)// BulkLoadPortion message
                    {
                        got = uWriteFile(fs.f, sp_msg.body+5, sp_msg.length-5, &written, __sys_call_error);
                        if ( 0 == got || (unsigned int) (sp_msg.length-5) != written )
                            throw USER_EXCEPTION(SE4045); 
                    }
                    else 
                    {
                        throw USER_EXCEPTION(SE3009);
                    }

                    res = sp_recv_msg(client_sock, &sp_msg);
                    if (res == U_SOCKET_ERROR) THROW_SOCKET_EXCEPTION(SE3007);

                    if (res == 1) 
                        throw USER_EXCEPTION(SE3012);
                }
            } catch (SednaUserException) {
                uCloseFile(fs.f, __sys_call_error);
                if(uDeleteFile(fs.name, __sys_call_error) == 0) 
                    d_printf1("tmp file delete error");
                throw;
            }

            uCloseFile(fs.f, __sys_call_error);

            cf.f = fopen(string(fs.name).c_str(), "r");
            strcpy(cf.name, fs.name);
            if (uGetFileSizeByName(cf.name, &(cf.file_size), __sys_call_error) == 0)
                throw USER_EXCEPTION2(SE4050, cf.name);

            i++;

        }//while

    } catch (ANY_SE_EXCEPTION) {
        // close and delete all files from cf_vec
        for (unsigned int j=0; j<i; j++)
        {
            if (cf_vec->at(j).f && (fclose(cf_vec->at(j).f) != 0))
            {
                cf_vec->at(j).f = NULL;
                throw USER_EXCEPTION(SE3020);
            }
            cf_vec->at(j).f = NULL;
            if(uDeleteFile(cf_vec->at(j).name, __sys_call_error) == 0) d_printf1("tmp file delete error");
        }
        throw;
    }
}

void socket_client::close_file_from_client(client_file &cf)
{
    if (cf.f && (fclose(cf.f) != 0))
    {
        cf.f = NULL;
        throw USER_EXCEPTION(SE3020);
    }
    cf.f = NULL;	
    if(uIsFileExist(cf.name, __sys_call_error))
    {
        if(!uDeleteFile(cf.name, __sys_call_error)) throw USER_EXCEPTION(SE3021);
        elog(EL_DBG, ("Temporary file has been deleted %s", cf.name));
    }

}

void socket_client::respond_to_client(int instruction)
{
    /* If session is being closed by server - 
     * do not send the message to the client
     */
    if (is_on_stop) return;

    sp_msg.instruction = instruction;
    sp_msg.length = 0;

    /* Authentication transaction emulation */
    switch (instruction)
    {
    case se_BeginTransactionOk:
        if (read_msg_count == se_BeginAuthenticatingTransaction) {read_msg_count = se_Authentication; return;}
    case se_CommitTransactionOk:
        if (read_msg_count == se_CommitAuthenticatingTransaction) {read_msg_count = se_GetNextMessageFromClient; return;}
    }
    if( sp_send_msg(client_sock, &sp_msg)!=0 ) THROW_SOCKET_EXCEPTION(SE3006);
}

void 
socket_client::end_item(qepNextAnswer res) {
    /* Flush buffer and send ItemEnd message */
    if(is_output_enabled()) {
        out_s->end_item(res);
    }
}

void 
socket_client::begin_item(bool is_atomic, xmlscm_type st, t_item nt, const char* uri) {
    /* Flush buffer and send ItemEnd message */
    if(is_output_enabled()) {
        out_s->begin_item(is_atomic, st, nt, uri);
    }
}

/* Important! In this version trn get session parameters from gov, not from the client because gov has already got it from client*/
void socket_client::get_session_parameters()
{
    timeout.tv_sec = 50;
    timeout.tv_usec = 0;
    int select_res = uselect_read(gov_sock, &timeout, __sys_call_error);
    if (select_res == 0) throw USER_EXCEPTION(SE3047);
    if (select_res == U_SOCKET_ERROR) THROW_SOCKET_EXCEPTION(SE3007);
    
    while(!govCommunicator->receive());
     
    if (se_SessionParameters != govCommunicator->getInstruction()) {
        error(SE3009, string("Unknown Instruction from client. Authentication failed."));
        throw USER_EXCEPTION(SE3009);
    }
    
    p_ver.major_version = govCommunicator->readChar();
    p_ver.minor_version = govCommunicator->readChar();
    
    if (p_ver.major_version < 5) {
            error(SE3014, string("major version: ")+int2string(p_ver.major_version)+string(" minor version: ")+int2string(p_ver.minor_version)); 
            throw USER_EXCEPTION(SE3014);
    }
    
    govCommunicator->readString(tr_globals::login, SE_MAX_LOGIN_LENGTH);
    govCommunicator->readString(tr_globals::db_name, SE_MAX_DB_NAME_LENGTH);
    
    d_printf2("In authorization login = %s\n", tr_globals::login);
    d_printf2("In authorization db_name = %s\n", tr_globals::db_name);
    
    while(!govCommunicator->receive());
    if (se_AuthenticationParameters != govCommunicator->getInstruction()) {
            error(SE3009, string("Error: Unknown Instruction from client. Authentication failed.")); 
            throw USER_EXCEPTION(SE3009);
    }
    govCommunicator->readString(tr_globals::password, SE_MAX_PASSWORD_LENGTH);
    d_printf2("In authorization password = %s\n", tr_globals::password);
    
    /*!FIXME Trn responses the client always ok there, it shouldn't be done in this way*/
    
    clientCommunicator->beginSend(se_AuthenticationOK);
    clientCommunicator->endSend();
    
    tr_globals::query_type = TL_XQuery;

    out_s = se_new se_socketostream(client_sock, p_ver);
    nul_s = se_new se_nullostream();
    stream = out_s;
}

void socket_client::set_session_options(msg_struct *msg)
{
    int pos = 0;
    uint32_t option_len;
    uint32_t option;
    
    if (p_ver.major_version < 3) throw USER_EXCEPTION(SE3009);

    while (pos < msg->length)
    {
        net_int2int(&option, msg->body+pos);
        pos += 5;
        net_int2int(&option_len, msg->body+pos);
        pos += 4;
        switch (option)
        {
        case SEDNA_DEBUG_ON:
            tr_globals::debug_mode = 1;
            break;
        case SEDNA_DEBUG_OFF:
            tr_globals::debug_mode = 0;
            break;
        case SEDNA_READONLY_TRANSACTION:
            SwitchSessionToRO(true);
            break;
        case SEDNA_UPDATE_TRANSACTION:
            SwitchSessionToRO(false);
            break;
        case SEDNA_QUERY_EXEC_TIMEOUT:
            {
                uint32_t value;
                net_int2int(&value, msg->body+pos);
                tr_globals::query_timeout = value;
                break;
            }
        case SEDNA_MAX_RESULT_SIZE:
            {
                uint32_t value;
                net_int2int(&value, msg->body+pos);
                max_result_size_to_pass = value;
                out_s->set_max_result_size_to_pass(max_result_size_to_pass);
                break;
            }
        case SEDNA_LOG_AMOUNT:
            {
                uint32_t value;
                net_int2int(&value, msg->body+pos);
                if (SEDNA_LOG_LESS != value && SEDNA_LOG_FULL != value)
                    throw USER_EXCEPTION2(SE4617, "unknown log-less mode");
                SwitchLogMode(value);
                break;
            }
        default: 
            /* Unknown option */
            throw USER_EXCEPTION2(SE4619, int2string(option).c_str());
        }
        pos += option_len;
    }

    /* Disable ro-mode if log-less mode requested */
    if (tr_globals::is_log_less_mode)
        tr_globals::is_ro_mode = false;
    
    /* Send reply that option has been set succcessfully */
    sp_msg.instruction = se_SetSessionOptionsOk;
    sp_msg.length = 0; 
    if(sp_send_msg(client_sock, &sp_msg)!=0) THROW_SOCKET_EXCEPTION(SE3006);
}

void socket_client::reset_session_options()
{
    /* Set default session options' values */
    tr_globals::query_timeout = 0;
    max_result_size_to_pass = 0;
    out_s->set_max_result_size_to_pass(max_result_size_to_pass);
    tr_globals::debug_mode = 0;
    SwitchLogMode(SEDNA_LOG_FULL);
    SwitchSessionToRO(false);
    
    /* Send reply that options have been reset succcessfully */
    sp_msg.instruction = se_ResetSessionOptionsOk;
    sp_msg.length = 0;
    if(sp_send_msg(client_sock, &sp_msg)!=0) THROW_SOCKET_EXCEPTION(SE3006);
}


void socket_client::authentication_result(bool res, const string& body)
{
    if(res)
    {
        sp_msg.instruction = se_AuthenticationOK;// AuthenticationOk message
        sp_msg.length = 0; 
        if(sp_send_msg(gov_sock, &sp_msg)!=0) THROW_SOCKET_EXCEPTION(SE3006);
        read_msg_count = se_CommitAuthenticatingTransaction;		
    }
    else
    {
        if(sp_error_message_handler(client_sock, se_AuthenticationFailed, SE3006, body.c_str())!=0) 
            THROW_SOCKET_EXCEPTION(SE3006);
    }
}

void socket_client::process_unknown_instruction(int instruction, bool in_transaction)
{
    if(in_transaction)
    {
        switch (instruction)
        {
        case se_BeginTransaction:
            {
                error(SE4612, USER_EXCEPTION(SE4612).getMsg());
                break;
            }
        default: throw USER_EXCEPTION(SE3009);
        }
    }
    else
    {
        switch (instruction)
        {
        case se_CommitTransaction:
            {
                error(SE4610, USER_EXCEPTION(SE4610).getMsg());
                break;
            }
        case se_RollbackTransaction:
            {
                error(SE4611, USER_EXCEPTION(SE4611).getMsg());
                break;
            }
        case se_GetNextItem:
            {
                error(SE4614, USER_EXCEPTION(SE4614).getMsg());
                break;
            }
        case se_ExecuteLong:
            {
                error(SE4615, USER_EXCEPTION(SE4615).getMsg());
                break;
            }
        case se_Execute:
            {
                error(SE4615, USER_EXCEPTION(SE4615).getMsg());
                break;
            }
        case se_ExecuteSchemeProgram:
            {
                error(SE4615, USER_EXCEPTION(SE4615).getMsg());
                break;
            }
        default: throw USER_EXCEPTION(SE3009);
        }
    }
}

void socket_client::error(int code, const string& body)
{
    if(client_sock != U_INVALID_SOCKET)
    {
        if(sp_error_message_handler(client_sock, se_ErrorResponse, code, body.c_str())!=0) 
            THROW_SOCKET_EXCEPTION(SE3006);
    }
}

void socket_client::error()
{
    if(client_sock != U_INVALID_SOCKET)
    {
        if(sp_error_message_handler(client_sock, se_ErrorResponse, 0, "Unknown error")!=0) 
            THROW_SOCKET_EXCEPTION(SE3006);
    }
}

void socket_client::show_time(u_timeb qep_time)
{
    string ex_time = to_string(qep_time);
    d_printf2("Show time. Time %s\n secs", ex_time.c_str());

    sp_msg.instruction = se_LastQueryTime;// LastQueryTime message
    sp_msg.length = 1 + sizeof(int) + ex_time.length();

    sp_msg.body[0] = 0; //C-string
    int2net_int(ex_time.length(), sp_msg.body + 1);
    strcpy(sp_msg.body + 1 + sizeof(int), ex_time.c_str());

    if(sp_send_msg(client_sock, &sp_msg) != 0) THROW_SOCKET_EXCEPTION(SE3006);
}

void socket_client::set_keep_alive_timeout(int sec)
{
    /* Set socket's receive and send timeouts.
     * The best way to implement this is to use select().  
     * On the hand SO_RCVTIMEO and SO_SNDTIMEO options don't work only under 
     * Solaris (?) and Linux earlier than 2.3.41 kernel. Considering that the most 
     * important place where timeout must be properly implemented is 
     * socket_client::read_msg and that is actually done well using select(),
     * at present we can accept this solution. 
     */

    this->ka_timeout = sec;

#if !defined(SunOS)
    if(sec > 0)
    {
#ifdef _WIN32
        int timeout = sec * 1000;    /// Under Windows *int* should be provided, which defines timeout in milliseconds.
#else
        timeout.tv_sec = sec;        /// Use *struct timeval* under POSIX systems.
        timeout.tv_usec = 0;
#endif
        if (usetsockopt(this->client_sock, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout), __sys_call_error) == U_SOCKET_ERROR ||
            usetsockopt(this->client_sock, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout), __sys_call_error) == U_SOCKET_ERROR)
        {
            throw USER_EXCEPTION2(SE4623, (string("timeout value was: ") + int2string(sec)).c_str());
        }
    }
#endif
}

se_ostream* 
socket_client::get_debug_ostream() { 

    if (NULL == dbg_s || recreate_debug_stream) 
    {
        if(NULL != dbg_s) 
        {
            delete dbg_s;
            dbg_s = NULL;
            recreate_debug_stream = false;
        }
        dbg_s = stream->get_debug_ostream();
    }
    return dbg_s;
}

bool
socket_client::disable_output() {
    bool res = is_output_enabled();
    stream = nul_s;
    return res;
}

void 
socket_client::user_statement_begin()
{
    enable_output();
    recreate_debug_stream = true;
}
