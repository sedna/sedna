/*
 * File:  socket_client.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include <list>
#include <fstream>
#include <sstream>
#include <stdio.h>

#include "base.h"
#include "uhdd.h"
#include "utils.h"
#include "d_printf.h"
#include "socket_client.h"
#include "tr_functions.h"
#include "auc.h"
#include "tr_utils.h"


using namespace std;

socket_client::socket_client()
{
     p_ver.major_version = 1;
     p_ver.minor_version = 0;	
 
     read_msg_count = se_BeginAuthenticatingTransaction;
     has_next_item = true;
     is_on_stop = false;

     timeout.tv_sec = 1;
     timeout.tv_usec = 0;

	 Sock = U_INVALID_SOCKET;
     stream = NULL;
     long_query_stream = NULL;
}

void socket_client::init()
{
 	  //  Takes Socket handle from Environment Variable 
     char buffer[ENV_BUF_SIZE + 1];
     memset(buffer, 0, ENV_BUF_SIZE + 1);
     uGetEnvironmentVariable(CONNECTION_SOCKET_HANDLE, buffer, ENV_BUF_SIZE, __sys_call_error);
     //d_printf2("getenv variable %d \n",GetLastError());

     Sock = atoi(buffer);   // use Sock
     if (Sock == U_INVALID_SOCKET)  //INVALID_SOCKET
     {
#ifdef _WIN32
        d_printf2("accept failed %d\n",GetLastError());
#else
        d_printf1("accept failed \n");
#endif
        throw USER_EXCEPTION(SE3001); 
      }
      
}

void socket_client::release()
{
     if(Sock != U_INVALID_SOCKET)
         if(ushutdown_close_socket(Sock, __sys_call_error)!=0) Sock = U_INVALID_SOCKET;
   	 if(stream != NULL)
   	 {
   	 	 delete stream;
   	 	 stream = NULL;
   	 }
   	 if(long_query_stream != NULL)
   	 {
   	 	 free( long_query_stream );
   	 	 long_query_stream = NULL;
   	 }
}

void socket_client::read_msg(msg_struct *msg)
{
	int res;
	
	if(is_stop_session())            //session closed forcibly by stop_serv utility
	{
		(*msg).instruction = se_CloseConnection; //close session
		(*msg).length = 0;
        is_on_stop = true;
	}
	else if(read_msg_count == se_BeginAuthenticatingTransaction)           // emulate BeginTransaction
	{
		(*msg).instruction = se_BeginTransaction; //BeginTransaction
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
                        timeout.tv_sec = 1;
                        timeout.tv_usec = 0;

           	res = uselect_read(Sock, &timeout, __sys_call_error);
 
			if(res == 1) //ready to recv data
			{
				break;
			}
			else if(res == U_SOCKET_ERROR) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007, usocket_error_translator());}
		}
		res = sp_recv_msg(Sock, msg);//d_printf2("msg.instruction %d\n", (*msg).instruction);
		if(res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007, usocket_error_translator()); }
		if(res == 1) throw USER_EXCEPTION(SE3012);
	}
}

char* socket_client::get_query_string(msg_struct *msg)
{
	__int32 query_portion_length;
	int res, query_size = 0;
	int malloced_size = SE_SOCKET_MSG_BUF_SIZE*5;

    if(long_query_stream != NULL)
   	{
   	    free( long_query_stream );
   	    long_query_stream = NULL;
   	}
	
	if((*msg).instruction == se_ExecuteLong) //get long query
	{
		if ((long_query_stream = (char*)malloc(malloced_size+1)) == NULL) throw USER_EXCEPTION(SE4080);

		while((*msg).instruction != se_LongQueryEnd) //while not the end of long query 
		{
	   		net_int2int(&query_portion_length, (*msg).body+2);
			if ( (query_size + query_portion_length) > malloced_size )
			{
				if ( ( long_query_stream = (char*)realloc( long_query_stream,  malloced_size + SE_SOCKET_MSG_BUF_SIZE*2 + 1 )) == NULL)
				{
					free( long_query_stream );
					long_query_stream = NULL;
					throw USER_EXCEPTION(SE4080);
				}
				malloced_size += SE_SOCKET_MSG_BUF_SIZE*2;
			}
			memcpy(long_query_stream+query_size, (*msg).body+6, query_portion_length);
			query_size += query_portion_length;
	   		res = sp_recv_msg(Sock, msg);
	   		if (res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
	   		if (res == 1) throw USER_EXCEPTION(SE3012);
		}
		long_query_stream[query_size] = '\0';
		return long_query_stream;
		}
	else						//get one-socket-msg-length query
	{
		net_int2int(&query_portion_length, (*msg).body+2);
		memcpy(query_string, (*msg).body+6, query_portion_length);
		query_string[query_portion_length] = '\0';
		return query_string;
	}
}



t_print socket_client::get_result_type(msg_struct *msg)
{
	return ((*msg).body[0] == 0) ? xml : sxml;
}


QueryType socket_client::get_query_type()
{
  return TL_XQuery;//nor write impl
}


se_ostream* socket_client::get_se_ostream()
{
	return stream;
}


client_file socket_client::get_file_from_client(const char* filename)
{
  string tmp_file_path_str;

  file_struct fs;
  client_file cf;

  int i, got, written = 0, cmd_bl, len_int, res;
  __int64 res_pos;

     try
     { 
        	if (strcmp(filename, "/STDIN/") == 0)
        	{
        		sp_msg.instruction = 431;// BulkLoadFromStream 431 message
        		sp_msg.length = 0;
        		if(sp_send_msg(Sock, &sp_msg)!=0) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
        	}
        	else
        	{
        		sp_msg.instruction = 430;// BulkLoadFileName 430 message
        		sp_msg.length = strlen(filename) +5;
        		
        		int2net_int(strlen(filename), sp_msg.body+1);
        		sp_msg.body[0] = 0;
        		memcpy(sp_msg.body+5, filename, strlen(filename));
        		if(sp_send_msg(Sock, &sp_msg)!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
        	}
			// create tmpfile for bulkload
			
			tmp_file_path_str = string(SEDNA_DATA) + string("/data/") + string(db_name) + string("_files");
		    res = uGetUniqueFileStruct(tmp_file_path_str.c_str(), &fs, sid, __sys_call_error);
	    	if(res == 0) throw USER_EXCEPTION(SE4052);

    	    res = sp_recv_msg(Sock, &sp_msg);
			if(res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007, usocket_error_translator()); }
			if(res == 1) throw USER_EXCEPTION(SE3012);

        	while(sp_msg.instruction != se_BulkLoadEnd)    // while not BulkLoadEnd message
	        {
	        	if (sp_msg.instruction == se_BulkLoadError)     // BulkLoadError
	        	{
	           		throw USER_EXCEPTION(SE3013);
	        	}
	        	else if (sp_msg.instruction == se_BulkLoadPortion)// BulkLoadPortion message
	        	{
	        		got = uWriteFile(fs.f, sp_msg.body+5, sp_msg.length-5, &written, __sys_call_error);
	        		if ((got == 0)||(written!=sp_msg.length-5)) throw USER_EXCEPTION(SE4045); 
	        	}
	        	else throw USER_EXCEPTION(SE3009);
	        	res = sp_recv_msg(Sock, &sp_msg);
		   		if (res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
		   		if (res == 1) throw USER_EXCEPTION(SE3012);
	        } //end of while

         	got = uCloseFile(fs.f, __sys_call_error);
         	cf.f = fopen(string(fs.name).c_str(), "r");

            if (uGetFileSizeByName(fs.name, &(cf.file_size), __sys_call_error) == 0)
               throw USER_EXCEPTION2(SE4050, fs.name);

         	strcpy(cf.name, fs.name);
     } catch (...) {
      	  if(uCloseFile(fs.f, __sys_call_error) == 0) d_printf1("tmp file close error %d\n");
          if(uDeleteFile(string(fs.name).c_str(), __sys_call_error) == 0) d_printf1("tmp file delete error");
          throw;
     }
     
     return cf;
}

void socket_client::close_file_from_client(client_file cf)
{
	if(fclose(cf.f) != 0) throw USER_EXCEPTION(SE3020);
	if(!uDeleteFile(cf.name, __sys_call_error)) throw USER_EXCEPTION(SE3021);
}

void socket_client::respond_to_client(int instruction)
{
    // if session is closed by server stopping - do not send message to client
    if (is_on_stop) return;
    
    sp_msg.instruction = instruction;
    sp_msg.length = 0;
    
    switch (instruction)
    {
        case se_BeginTransactionOk:
            if (read_msg_count == se_BeginAuthenticatingTransaction) {read_msg_count = se_Authentication; return;}
        case se_CommitTransactionOk:
            if (read_msg_count == se_CommitAuthenticatingTransaction) {read_msg_count = se_GetNextMessageFromClient; return;}
    }
	if(sp_send_msg(Sock, &sp_msg)!=0) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
}

void socket_client::begin_item()
{
//	msg_struct msg;
	
	sp_msg.instruction = se_QuerySucceeded;
	sp_msg.length = 0;
	
	if(sp_send_msg(Sock, &sp_msg)!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
}

void socket_client::end_of_item(bool res) //res variable is ignored
{
    (*stream).end_of_data(res);   // flushes the buffer and sends ItemEnd message
}

void socket_client::get_session_parameters()
{
  sp_msg.instruction = se_SendSessionParameters;// SendSessionParameters message
  sp_msg.length = 0;
  if (sp_send_msg(Sock, &sp_msg)!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
  
  timeout.tv_sec = 50;
  timeout.tv_usec = 0;
  int select_res = uselect_read(Sock, &timeout, __sys_call_error);
  if (select_res == 0) throw USER_EXCEPTION(SE3047);
  if (select_res == U_SOCKET_ERROR) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
  int res = sp_recv_msg(Sock, &sp_msg);
  if (res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
  if (res == 1) throw USER_EXCEPTION(SE3012);
  
  if (sp_msg.instruction != se_SessionParameters) //SessionParameters
  {
     error(SE3009, string("Unknown Instruction from client. Authentication failed."));
     throw USER_EXCEPTION(SE3009);
  }
  int buf_position = 0;
  p_ver.major_version = sp_msg.body[buf_position];
  p_ver.minor_version = sp_msg.body[buf_position+1];
      
  if (!(((p_ver.major_version==1)||(p_ver.major_version==2))&&(p_ver.minor_version==0))) //version checking (version 1.0 and 2.0 are supported)
  {
      error(0, string("Error: Unknown Sedna Client-Server protocol version.")); 
      throw USER_EXCEPTION(SE3014);
  }
     
  buf_position += 3;
  __int32 length;
  net_int2int(&length, sp_msg.body+buf_position);

  buf_position += sizeof(__int32);
      
  if(length > SE_MAX_LOGIN_LENGTH)
  {
    error(SE3015, string("Error: Too long login")); 
    throw USER_EXCEPTION(SE3015);
  }

  memcpy(login, sp_msg.body+buf_position, length);   //!!!??? login - is a global parameter
      
  login[length] = '\0';   
  buf_position += length;
       
  d_printf3("In authorization length = %d login = %s\n", length, login);
      
  net_int2int(&length, sp_msg.body+buf_position+1);
  buf_position += 1 + sizeof(__int32);
	
  d_printf2("length =%d\n", length);
  if(length > SE_MAX_DB_NAME_LENGTH)
  {
     error(SE3015, string("Error: Too long db_name")); 
     throw USER_EXCEPTION(SE3015);
  }

  memcpy(db_name, sp_msg.body+buf_position, length);	  
  db_name[length] = '\0';
      
  d_printf2("In authorization db_name = %s\n", db_name);
      
  sp_msg.instruction = se_SendAuthParameters;// SendAuthenticationParameters message
  sp_msg.length = 0;
  if (sp_send_msg(Sock, &sp_msg)!=0) throw USER_EXCEPTION2(SE3006,usocket_error_translator());
  
  timeout.tv_sec = 50;
  timeout.tv_usec = 0;
  select_res = uselect_read(Sock, &timeout, __sys_call_error);
  if (select_res == 0) throw USER_EXCEPTION(SE3047);
  if (select_res == U_SOCKET_ERROR) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
  res = sp_recv_msg(Sock, &sp_msg);
  if (res == U_SOCKET_ERROR) { Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3007,usocket_error_translator());}
  if (res == 1) throw USER_EXCEPTION(SE3012);

  if (sp_msg.instruction != se_AuthenticationParameters) //AuthenticationParameters
  {
    error(SE3009, string("Error: Unknown Instruction from client. Authentication failed.")); 
    throw USER_EXCEPTION(SE3009);
  }

  buf_position = 1;
  net_int2int(&length, sp_msg.body+buf_position);
  buf_position += sizeof(__int32);

  if(length > SE_MAX_PASSWORD_LENGTH)
  {
     error(SE3015, string("Error: Too long password")); 
     throw USER_EXCEPTION(SE3015);
  }
  memcpy(password, sp_msg.body+buf_position, length); 	   
  password[length] = '\0';
      
  d_printf2("In authorization password = %s\n", password);

  query_type = TL_XQuery;
  
  stream = new se_socketostream(Sock, p_ver);
}


void socket_client::authentication_result(bool res, const string& body)
{
//   msg_struct auth_result_msg;
   
   if(res)
   {
   	   sp_msg.instruction = se_AuthenticationOK;// AuthenticationOk message
   	   sp_msg.length = 0; 
   	   if(sp_send_msg(Sock, &sp_msg)!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION2(SE3006,usocket_error_translator());}
	   read_msg_count = se_CommitAuthenticatingTransaction;		
   }
   else
   {
	   if(sp_error_message_handler(Sock, se_AuthenticationFailed, SE3006, body.c_str())!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION(SE3006);} 
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
        }//end switch
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
    if(Sock != U_INVALID_SOCKET)
        if(sp_error_message_handler(Sock, se_ErrorResponse, code, body.c_str())!=0) 
        {
            Sock = U_INVALID_SOCKET;
            throw USER_EXCEPTION2(SE3006,usocket_error_translator());
        }
}

void socket_client::error()
{
    if(Sock != U_INVALID_SOCKET)
        if(sp_error_message_handler(Sock, se_ErrorResponse, 0, "Unknown error")!=0) 
        {
            Sock = U_INVALID_SOCKET;
            throw USER_EXCEPTION2(SE3006,usocket_error_translator());
        }
}

void socket_client::show_time(string qep_time)
{
   d_printf2("Show time. Time %s\n",qep_time.c_str());

   sp_msg.instruction = se_LastQueryTime;// LastQueryTime message
   sp_msg.length = 1+4+qep_time.length();

   sp_msg.body[0] = 0; //C-string
   int2net_int(qep_time.length(), sp_msg.body+1);
   strcpy(sp_msg.body+5, qep_time.c_str());
   
   if(sp_send_msg(Sock, &sp_msg)!=0) {Sock = U_INVALID_SOCKET; throw USER_EXCEPTION(SE3006);}
}

void socket_client::write_user_query_to_log()
{
}