/*
 * File:  tr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>
#include <string>
#include <string.h>

#include "base.h"
#include "utils.h"
#include "exceptions.h"
#include "SSMMsg.h"
#include "tr_globals.h"
#include "tr_functions.h"
#include "pq.h"
#include "persistent_db_data.h"
#include "exec_output.h"
#include "por2qep.h"
#include "d_printf.h"
#include "pping.h"
#include "cl_client.h"
#include "socket_client.h"
#include "usecurity.h"
#include "tr_utils.h"
#include "auc.h"


// only for MSDEV 6.0
#if (_MSC_VER == 1200) && (WINVER < 0x0500)
extern "C" long _ftol( double ); //defined by VC6 C libs
extern "C" long _ftol2( double dblSource ) { return _ftol(dblSource ); } 
#endif


using namespace std;


DECLARE_TIME_VARS

// variables for time measurement
u_timeb t_total1, t_total2, t_qep1, t_qep2, t_total, t_qep;


//u_timeb t_test1, t_test2, t_test3, t_test4;
string total_time = "0.000 secs", qep_time = "0.000 secs";

#ifdef _WIN32
BOOL TrnCtrlHandler(DWORD fdwCtrlType) 
{ 
    switch (fdwCtrlType) 
    { 
        case CTRL_C_EVENT		: // Handle the CTRL+C signal. 
        case CTRL_CLOSE_EVENT	: // CTRL+CLOSE: confirm that the user wants to exit. 
        case CTRL_BREAK_EVENT	: 
        case CTRL_LOGOFF_EVENT	: 
        case CTRL_SHUTDOWN_EVENT: 

                                  return TRUE; 
        default					: return FALSE; 
    } 
} 
#else
#include <signal.h>

void TrnCtrlHandler(int signo)
{
	if (   signo == SIGINT 
        || signo == SIGQUIT
        || signo == SIGTERM) 
	{
        //beep(); 
	}
}
#endif


//u_timeb ttt1, ttt2;


int main(int argc, char * argv[]) 
{
  int ret_code = 0;
  program_name_argv_0 = argv[0];
  pping_client ppc(5151);
  char buf[1024];
  SSMMsg *sm_server = NULL;
  int determine_vmm_region = 0;

  try {

     INIT_TOTAL_TIME_VARS
  	  
     u_ftime(&t_total1);

     if (uGetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, buf, 1024) != 0)
         determine_vmm_region = 0;
     else
         determine_vmm_region = atoi(buf);

     if (determine_vmm_region == 1)
     {
         vmm_determine_region();
         return 0;
     }
     else 
     {
         vmm_preliminary_call();
         OS_exceptions_handler::install_handler();
     }

#ifdef REQUIRE_ROOT
     if (!uIsAdmin()) throw USER_EXCEPTION(SE3064);
#endif

     if (uGetEnvironmentVariable(SEDNA_SERVER_MODE, buf, 1024) != 0)
        server_mode = 0;
     else
        server_mode = atoi(buf);

     if (server_mode == 1) 
        client = new socket_client();
     else//server mode  = 0 (run from command line)
     {
//        if (strcmp(ACTIVE_CONFIGURATION, "Release") == 0)
//           throw USER_EXCEPTION(SE4613);

        client = new command_line_client(argc, argv); 
     }


     if(uSocketInit()!=0) throw USER_EXCEPTION(SE3001);

//  u_ftime(&ttt1);
	 client->init();

//  u_ftime(&ttt2);
//  cerr << "TEST!!!!!!!!!!!!!: " << to_string(ttt2 - ttt1).c_str() << endl;
     //get session parameters (from socket or from cmd line)
     client->get_session_parameters();

     if(!check_database_existence(db_name))//check database consistency (all files exists)
       throw USER_EXCEPTION2(SE4609, db_name);

     //init global names
     set_global_names();
     set_global_names(db_name);

     gov_shared_mem = open_gov_shm(&gov_shm_dsc);
     is_init_gov_shm = true;
     socket_port = ((gov_header_struct*)gov_shared_mem)->lstnr_port_number;

     //register session on governer
     register_session_on_gov();

     SednaUserException e = USER_EXCEPTION(SE4400);
     ppc.startup(e);


#ifdef _WIN32
     BOOL fSuccess; 
     fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) TrnCtrlHandler, TRUE);                           // add to list 
     if (!fSuccess) throw USER_EXCEPTION(SE4207);
#else
     // For Control-C or Delete
     if ((int)signal(SIGINT, TrnCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
	 // For Control-backslash
     if ((int)signal(SIGQUIT, TrnCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
	 //For reboot or halt
     if ((int)signal(SIGTERM, TrnCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
#endif

     msg_struct client_msg;

//u_ftime(&t_test3);
        // transaction initialization
//u_ftime(&t_test1);
    on_session_begin(sm_server);
//u_ftime(&t_test2);
//d_printf2("TEST1: %s\n", to_string(t_test2 - t_test1).c_str());


#if (AUTH_SWITCH == 1)//if security is on
	 if(entry_point->is_first_transaction())
	 {
		entry_point->clear_first_transaction_flag();
		auth = 0;
	 }
#endif
	
    PPQueryEssence *qep_tree = NULL;//qep of current stmnt
    StmntsArray *st = NULL;

        for(;;)//cycle by transactions
        {
                client->read_msg(&client_msg);
                if (client_msg.instruction == se_BeginTransaction) //BeginTransaction
                {                    	
                 try {
                    on_transaction_begin(sm_server);
                    client->respond_to_client(se_BeginTransactionOk);
                    
                    qep_tree = NULL;//qep of current stmnt
                    st = NULL;
                    bool has_next_item = true;

                    d_printf1("============== Trn execution started ======================\n");


                    do //cycle by commands of one transaction
                    {
                        client->read_msg(&client_msg);
                        switch (client_msg.instruction)
                        {
                        case se_Authenticate: //authentication
                        {
                            authentication();
                            client->authentication_result(true, "");
                            break;
                        }
                        case se_ExecuteSchemeProgram://Execute Scheme program
                        {                                                       
                            d_printf2("Scheme program from file=%s is executed\n", client_msg.body);
							break;
                        }

						case se_ExecuteLong: //execute long query command 
                        case se_Execute: //execute query command
                        {
                        	u_ftime(&t_qep1);

                            //print for test system
                            d_printf1("\n============== statement =================\n");

                            // close previous statement
                            on_user_statement_end(qep_tree, st);

                            on_user_statement_begin(client->get_query_type(),
                                                    client->get_result_type(&client_msg),
                                                    client->get_se_ostream(),
                                                    client->get_query_string(&client_msg),
                                                    qep_tree,
                                                    st);


                            if (qep_tree->is_update())
                            {
                                GET_TIME(&t1_exec);
//getchar();
                                execute(qep_tree);
                                GET_TIME(&t2_exec);
                                client->respond_to_client(se_UpdateSucceeded);
                                on_user_statement_end(qep_tree, st);
                            } 
                            else if (!(qep_tree->supports_next()))
                            {
                                client->begin_item();
                                GET_TIME(&t1_exec);
                                execute(qep_tree);
                                GET_TIME(&t2_exec);

                                client->end_of_item(false);

                                on_user_statement_end(qep_tree, st);                            
                            }
                            else
                            {
                                client->begin_item();
                                GET_TIME(&t1_exec);
                                has_next_item = next(qep_tree);
                                GET_TIME(&t2_exec);
 
                                client->end_of_item(has_next_item);
                                if (!has_next_item) 
                                {
                                    on_user_statement_end(qep_tree, st);
   	                            }
                            }

                            u_ftime(&t_qep2);
                            t_qep = (t_qep2 - t_qep1);
                            qep_time = to_string(t_qep);

                            ADD_TIME(t_total_exec, t1_exec, t2_exec);

                            break;
                        }

                        case se_GetNextItem: //next portion command
                        {
                        	u_ftime(&t_qep1);
                            if(has_next_item) 
                            {
                               GET_TIME(&t1_exec);
                               has_next_item = next(qep_tree);

                               GET_TIME(&t2_exec);
                            }
                            client->end_of_item(has_next_item);
                        	u_ftime(&t_qep2);
                        	
                        	t_qep = (t_qep2 - (t_qep1 - t_qep));
                            qep_time = to_string(t_qep);
                            if (!has_next_item)
                            {
                                on_user_statement_end(qep_tree, st);
                            }

                            ADD_TIME(t_total_exec, t1_exec, t2_exec);

                            break;
                        }

                        case se_CommitTransaction: //commit command
                        {
                            on_user_statement_end(qep_tree, st);
                            on_transaction_end(sm_server, true/*COMMIT*/);
                            ret_code = 0;
                           
                            client->respond_to_client(se_CommitTransactionOk);
                            d_printf1("============== Trn execution finished ======================\n");
                            break; 
                        }

                        case se_RollbackTransaction: //rollback command
                        {
                            on_user_statement_end(qep_tree, st);
                            on_transaction_end(sm_server, false/*ROLLBACK*/);
                            ret_code = 0;

                            client->respond_to_client(se_RollbackTransactionOk);
                            d_printf1("============== Trn execution finished ======================\n");
                            break;
                        }
                        case se_ShowTime: //show time
                        {
                        	client->show_time(qep_time);
                            break;
                        }
                        case se_CloseConnection: //close connection
                        {
                            on_user_statement_end(qep_tree, st);
                            on_transaction_end(sm_server, false/*ROLLBACK*/);
                            ret_code = 1;

                            client->respond_to_client(se_TransactionRollbackBeforeClose);
                            d_printf1("============== Trn execution finished ======================\n");
                            break;
                        }
                        default: 
                        {
                            client->process_unknown_instruction(client_msg.instruction, true);
                            break;
                        }
                        }//end switch
   
                    } while(client_msg.instruction != se_CommitTransaction && client_msg.instruction != se_RollbackTransaction && client_msg.instruction != se_CloseConnection);

                 } catch (SednaUserException &e) {
                    on_user_statement_end(qep_tree, st);
                    on_transaction_end(sm_server, false/*ROLLBACK*/);
                    ret_code = 1;

                    d_printf1("\nTr is rolled back successfully\n");

                    if (e.get_code() == SE3053 || e.get_code() == SE3006 || e.get_code() == SE3007 || e.get_code() == SE3009)//session must be closed
                        {ret_code = 1; throw;}

                    fprintf(stderr, "%s\n", e.getMsg().c_str());
                    client->error(e.get_code(), e.getMsg());
                 } catch (SednaException &e) {
                     
                    sedna_soft_fault(e);
                 } catch (...) {
                    sedna_soft_fault();
                 }

               } 
               else
               if(client_msg.instruction == se_CloseConnection)  //CloseConnection
               {
                    client->respond_to_client(se_CloseConnectionOk);
                    client->release();
                    delete client;
                    break;
               }
               else if(client_msg.instruction == se_ShowTime)  //ShowTime
               {
                   	client->show_time(qep_time);
               }
               else
               {
                   client->process_unknown_instruction(client_msg.instruction, false);
               }


        } //end for(;;) by transactions

    on_session_end(sm_server);


                    u_ftime(&t_total2);
                    total_time = to_string(t_total2 - t_total1);
                    //qep_time = to_string(t_qep2 - t_qep1);

	            //   PRINT_DEBUG_TIME_RESULTS
	
                    if (show_time == 1)
                    {
                        cerr << "\nStatistics: total time: " << total_time.c_str() << "\n";
                        cerr << "<step>\t\t\t<time>\n";
                        //cerr << "query execution\t\t" << qep_time.c_str() << "\n";
                        cerr << endl;
#ifdef VMM_GATHER_STATISTICS
                        cerr << "vmm_different_blocks_touched : " << vmm_different_blocks_touched() << endl;
                        cerr << "vmm_blocks_touched           : " << vmm_blocks_touched() << endl;
                        cerr << "vmm_different_blocks_modified: " << vmm_different_blocks_modified() << endl;
                        cerr << "vmm_blocks_modified          : " << vmm_blocks_modified() << endl;
                        cerr << "vmm_data_blocks_allocated    : " << vmm_data_blocks_allocated() << endl;
                        cerr << "vmm_tmp_blocks_allocated     : " << vmm_tmp_blocks_allocated() << endl;
                        cerr << "vmm_number_of_checkp_calls   : " << vmm_number_of_checkp_calls() << endl;
                        cerr << "vmm_number_of_sm_callbacks   : " << vmm_number_of_sm_callbacks() << endl;
                        cerr << endl;
#endif
	               PRINT_DEBUG_TIME_RESULTS
                    }

   ppc.shutdown(); 
   
   set_session_finished();
   
   close_gov_shm(gov_shm_dsc, gov_shared_mem);
   
   uSocketCleanup();

   d_printf1("Transaction has been closed\n\n");

   } catch (SednaUserException &e) {
      fprintf(stderr, "%s\n", e.getMsg().c_str());

       on_session_end(sm_server);
       try{
          if (client != NULL) {
             if (e.get_code() == SE3053)
                client->authentication_result(false, e.getMsg());
             else
          	    if ((e.get_code()!= SE3007) &&(e.get_code()!= SE3006))
          	      client->error(e.get_code(), e.getMsg());
             client->release();
		     delete client;
             }
       } catch (...){
          d_printf1("Connection with client has been broken\n");
       }
       ppc.shutdown();   
       set_session_finished();
       if (is_init_gov_shm)close_gov_shm(gov_shm_dsc, gov_shared_mem);
       uSocketCleanup();
       ret_code = 1;   
   } catch (SednaException &e) {
       sedna_soft_fault(e);
   } catch (...) {
       sedna_soft_fault();
   }

  return ret_code;
}

