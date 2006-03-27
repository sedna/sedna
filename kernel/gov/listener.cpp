#include <iostream>
#include <stdio.h>
#include "listener.h"
#include "uprocess.h"
#include "base.h"
#include "d_printf.h"
#include "gov_globals.h"
#include "sp.h"


using namespace std;

static USOCKET sockfd;

int client_listener(bool is_background_mode)
{
   
   msg_struct msg;

   USOCKET socknew;

   sockfd = usocket(AF_INET, SOCK_STREAM, 0);
   if (sockfd == U_INVALID_SOCKET) throw SYSTEM_EXCEPTION ("Can't init socket");

   if (uNotInheritDescriptor(UHANDLE(sockfd)) != 0) throw USER_EXCEPTION(SE4080);

   if (ubind_tcp(sockfd, socket_port) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Can't bind socket");

   if (ulisten(sockfd, 100) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Can't set socket to a listening mode");

   if (!is_background_mode)
   {	   
     d_printf1("OK\n");
     fprintf(res_os, "GOVERNOR has been started successfully\n");
     fflush(res_os);
   }


   ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
   USemaphore started_sem;
   if (0 == USemaphoreOpen(&started_sem, CHARISMA_GOVERNOR_IS_READY))
   {
       USemaphoreUp(started_sem);
       USemaphoreClose(started_sem);
   }
   ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////


   int stop_serv = 0;
   int stop_db = 0;
   int res = 0, res2 = 0;
   int socket_optval = 1, socket_optsize = sizeof(int);
   for(;;)
   {
  	   
       //accept a call from a client
       socknew = uaccept(sockfd);

       if (socknew == U_INVALID_SOCKET)
       {
          d_printf1("Can't accept client's connection\n");
          continue;
       }
       
       if (usetsockopt(socknew, IPPROTO_TCP, TCP_NODELAY, (char*)&socket_optval, socket_optsize) == U_SOCKET_ERROR)
       {
          d_printf1("Can't accept client's connection: couldn't set socket option\n");
          ushutdown_close_socket(socknew);
          continue;
       }

       gov_table->erase_all_closed_pids();
       gov_table->put_all_free_sids_in_ids_table();
       stop_serv = gov_table->check_stop_gov();
       
       if (stop_serv == 0)
          stop_db = gov_table->check_stop_databases();

       //!!!! process msg from client !!!!
       res = sp_recv_msg(socknew, &msg);

       if (stop_serv == 0)
          if (res == -1)//socket error 
          {
             d_printf2("Connection with client lost: %s\n", usocket_error_translator());
             continue;
          }
          else if (res == -2)
          {
             d_printf1("Too large msg recieved\n");
             continue;
           }



       if (stop_serv == 1 )
       {
          if (msg.instruction != STOP)
          {
             sp_error_message_handler(socknew, 100, SE4608, "Transaction is rolled back because server is stopping");
             if (res2 == U_SOCKET_ERROR) d_printf1("Can't send msg to client that server is stopped\n");
          }

          //close session
          ushutdown_close_socket(socknew);
          break;
       }

       if (stop_db == 1)
       {
          ushutdown_close_socket(socknew);
          continue;
       }



		   
       switch (msg.instruction)
       {
               ////////////////////////////////////
          case CREATE_NEW_SESSION:
          {
              CreateNewSessionProcess(socknew, is_background_mode);
              break;
          }

               ////////////////////////////////////

          case REGISTER_NEW_SESSION:
          {

              sess_registering(socknew, msg.body);
              break;
          }
 
               ////////////////////////////////////
  
          case REGISTER_DB:
          {

              sm_registering(socknew, msg.body);
              break;
          }

               ////////////////////////////////////

          case RUNTIME_CONFIG:
          {
             send_runtime_config(socknew);
             break;
          }

               ////////////////////////////////////

          case  IS_RUN_SM:
          {
             check_sm_run(socknew, msg.body);
             break;
          }


               ////////////////////////////////////

          default:
          {
             d_printf1("unknown message from client\n");
             ushutdown_close_socket(socknew);
             break;
          }
        }
          
                
   }//end of for



   if (uclose_socket(sockfd) != 0) throw SYSTEM_EXCEPTION("Can't close listening socket");

   return 0;
}

void CreateNewSessionProcess(USOCKET socknew, bool is_background_mode)
{

try{
    UFlag window_mode;
    UPID pid;
    UPHANDLE proc_h;

    //check number of sessions
    if (gov_table->get_total_session_procs_num() > 2*MAX_SESSIONS_NUMBER)
       throw USER_EXCEPTION(SE3046);

#ifdef _WIN32
    USOCKET DuplicateSock;
    //DuplicateHandle is used only for WinSockets 
    //(as create process doesn't inherit socket handles correctly)
    // Duplicate the socket socknew to create an inheritable copy.
    if (!DuplicateHandle(GetCurrentProcess(),
                         (HANDLE)socknew,
                         GetCurrentProcess(),
                         (HANDLE*)&DuplicateSock,
                         0,
                         TRUE, // Inheritable
                         DUPLICATE_SAME_ACCESS))
    {
       d_printf2("dup error %d\n", GetLastError());
       throw SYSTEM_EXCEPTION("Can't duplicate socket handle");
    }

    // Sets SOCKET HANDLE to an evironment variable
    uSetEnvironmentVariable(CONNECTION_SOCKET_HANDLE,int2string((int)DuplicateSock).c_str());
#else           // no need to duplicate SOCKET HANDLE in Unix
    uSetEnvironmentVariable(CONNECTION_SOCKET_HANDLE,int2string((int)socknew).c_str());
#endif

    uSetEnvironmentVariable(SEDNA_SERVER_MODE, "1");

    // create security attributes for the new process
    USECURITY_ATTRIBUTES *sa;	
    if(0 != uCreateSA(&sa, 
                      U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 
                      0                  // new process will not inherit handle returned by CreateProcess
                      )) 
    throw USER_EXCEPTION(SE3060);

    // Spawn the child process.
    // Socket HANDLE are passed throught a environment variable
                 	 
    char buf[U_MAX_PATH + 10];
    string con_path_str = uGetImageProcPath() + string("/") + SESSION_EXE;
    strcpy(buf, con_path_str.c_str());
 
    if (is_background_mode)
       window_mode = 0;//U_NO_WINDOW; //process has no window for output
    else
       window_mode = 0;           //process is created without flags
                
    if (0 != uCreateProcess(buf,
                            true, // inherit handles
                            NULL,
                            window_mode,
                            &proc_h,
                            NULL,
                            &pid,
                            NULL,
                            sa
                           ))
    {
#ifdef _WIN32                  
       d_printf2("create process failed %d\n", GetLastError());
#else           
       d_printf1("create process failed\n");
#endif
       throw SYSTEM_EXCEPTION("Can't create process");
    }

   // release security attributes
   if(uReleaseSA(sa)!=0) throw USER_EXCEPTION(SE3063);

   uclose_socket(socknew);
#ifdef _WIN32
   uclose_socket(DuplicateSock);
#endif

   gov_table->add_pid(pid, proc_h);


   } catch (SednaUserException &e) {
       fprintf(stderr, "%s\n", e.getMsg().c_str());
       sp_error_message_handler(socknew, 100, e.get_code(), e.getMsg().c_str());
   } catch (SednaException &e) {
       sp_error_message_handler(socknew, 100, 0, "System error");
       sedna_soft_fault(e);
   } catch (...) {
       sp_error_message_handler(socknew, 100, 0, "System error");
       sedna_soft_fault();
   }


   return;
}


int sess_registering(USOCKET s, char* msg_buf)
{
	msg_struct reg_msg;
    UPID sess_pid;
	char db_name[SE_MAX_DB_NAME_LENGTH + 1];

   	__int32 length;
    int res, res2;
    UPHANDLE proc_h;
    bool is_child_process;
   	
   	net_int2int(&length, msg_buf+1);
   	if( length > SE_MAX_DB_NAME_LENGTH ) throw USER_EXCEPTION(SE3015);
   	
    memcpy(db_name, msg_buf+5, length);
    db_name[length] = '\0';
            	
  	d_printf2("Listener: register trn with db_name: %s\n",db_name);
  	char ptr[4];
  	memcpy(ptr, msg_buf+5+strlen(db_name), 4);
            	
//    sess_pid = ntohl(*(__int32*)ptr);
    sess_pid = *(__int32*)ptr;

  	d_printf2("Listener: register trn with pid: %d\n",sess_pid);

    session_id s_id;
	string db_name_str = string(db_name);

    if ((is_child_process = gov_table->find_pid(sess_pid, proc_h)) == true)
    {//session run by Governor       
       res = gov_table->insert_session(sess_pid, &proc_h, db_name_str, true, s_id);                                
    }
    else
    {//session run from command line
       res = gov_table->insert_session(sess_pid, NULL, db_name_str, false, s_id);                                
    }

    //d_printf2("Governor returned sid=%d\n", s_id);

    if ( res == 0 )
    {
    	reg_msg.instruction = 161;
		reg_msg.length = 4;
	    __int32 tmp = s_id;
	    memcpy(reg_msg.body,(char*)&(tmp),4);

	    if(sp_send_msg(s,&reg_msg)!=0) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));

        gov_table->remove_pid(sess_pid);
    }
    else if (res == -1)
    {
        d_printf1("no db name case\n");
        reg_msg.instruction = 171;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));
    }
    else if (res == -2)
    {
        d_printf1("currently there are maximum number of session in the system\n");
		reg_msg.instruction = 172;
		reg_msg.length = 0;
        res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));
    }
    else if (res == -3)
    {
	    d_printf1("internal server error\n");
   	    reg_msg.instruction = 173;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));
    }
    else
    {
        throw SYSTEM_EXCEPTION("Governor failed while registering a new session");
	}

    if (res != 0)
       gov_table->wait_remove_pid(sess_pid, is_child_process);
    
    ushutdown_close_socket(s);

	return 0;
	
}

int sm_registering(USOCKET s, char* msg_buf)
{
	msg_struct reg_msg;
    UPID sm_pid;
	char db_name[SE_MAX_DB_NAME_LENGTH + 1];
   	__int32 length;
   	int res, res2;
   	
   	net_int2int(&length, msg_buf+1);
   	d_printf2("sm_registering: %d\n", length);
   	if( length > SE_MAX_DB_NAME_LENGTH ) throw USER_EXCEPTION(SE3015);

    memcpy(db_name, msg_buf+5, length);
    db_name[length] = '\0';
            	
  	d_printf2("Listener: register SM with db_name: %s\n",db_name);
  	char ptr[4];
  	memcpy(ptr, msg_buf+5+strlen(db_name), 4);
    sm_pid = ntohl(*(__int32*)ptr);
                
  	d_printf2("Listener: register SM with pid: %d\n",sm_pid);

///////////////Trying to Register in SHARED MEMORY//////////////////////
	string db_name_str = string(db_name);
    res = gov_table->insert_database(sm_pid, db_name_str);

    if (res == 0)
    {
        reg_msg.instruction = 181;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));
    }
    else if (res == -2)
    {
        reg_msg.instruction = 182;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));  
    }
    else if (res == -3)
    {
        reg_msg.instruction = 182;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));  
    }
    else if (res == -4)
    {
        reg_msg.instruction = 182;
		reg_msg.length = 0;
		res2 = sp_send_msg(s, &reg_msg);
        if (res2 == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION(string(usocket_error_translator()));  
    }

    ushutdown_close_socket(s);

	return 0;
}


void send_runtime_config(USOCKET s)
{
    string rc;
    int res;
	msg_struct msg;

    rc = gov_table->get_rc();

    msg.instruction = 0;//not important
    msg.length = rc.size() + 1;

    if (msg.length > SE_SOCKET_MSG_BUF_SIZE)
    {
       d_printf1("Too large runtime configuration\n");
       ushutdown_close_socket(s);
       return;
    }

    strcpy(msg.body, rc.c_str());

    d_printf2("rc=%s\n", msg.body);

    res = sp_send_msg(s, &msg);

    if (res == U_SOCKET_ERROR)
       d_printf1("Can't send reply to rc utility\n");


    ushutdown_close_socket(s);
}

void check_sm_run(USOCKET s, char* msg_buf)
{
	msg_struct msg;
    bool run;
    int res;

    d_printf2("database name to check=%s\n", msg_buf);
    run = gov_table->is_database_run(msg_buf);

    if (run) (msg.body)[0] = 'y';
    else (msg.body)[0] = 'n';

    msg.length = 1;

    res = sp_send_msg(s, &msg);

    if (res == U_SOCKET_ERROR)
       d_printf1("Can't send reply to ddb utility\n");

    ushutdown_close_socket(s);
}
