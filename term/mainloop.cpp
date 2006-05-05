/*
 * File:  mainloop.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "base.h"
#include "d_printf.h"
#include "uhdd.h"

#include "mainloop.h"
#include "term_globals.h"
#include "term_funcs.h"

int quit_term()
{
	term_output1("Closing session...");
	
    //closing session
    int res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
 	   term_output2("Session was closed with errors \n%s\n", SEgetLastErrorMsg(&conn));
	   return 1;
    }
	term_output1("Ok\n");
    
    return 0;
}

#ifdef _WIN32
BOOL TermCtrlHandler(DWORD fdwCtrlType) 
{ 
    switch (fdwCtrlType) 
    { 
        case CTRL_C_EVENT		: // Handle the CTRL+C signal. 
        case CTRL_CLOSE_EVENT	: // CTRL+CLOSE: confirm that the user wants to exit. 
        case CTRL_BREAK_EVENT	: 
        case CTRL_LOGOFF_EVENT	: 
        case CTRL_SHUTDOWN_EVENT: 
        {
            return TRUE; 
        }
        default					: return FALSE; 
    } 
} 
#else
#include <signal.h>

void TermCtrlHandler(int signo)
{
	if (   signo == SIGINT 
        || signo == SIGQUIT
        || signo == SIGTERM) 
	{
        //beep(); 
	}
}
#endif

int slash_commands_help()
{
	term_output1("    \\? - for help on internal slash commands\n");
	term_output1("    \\commit - to commit transaction\n");
	term_output1("    \\rollback - to rollback transaction\n");
	term_output1("    \\showtime - to show the time of the latest query execution\n");
	term_output1("    \\set - to set the terminal internal variable\n");
	term_output1("    \\unset - to unset the terminal internal variable\n");
	term_output1("    \\quit - to close session and quit the Sedna Terminal\n");
	term_output1("    XQuery/Update statements ended with ampersand+line feed\n ");
	return 0;
}

/*
 * Main processing loop for reading lines of input
 *	and sending them to the backend.
 */
int
MainLoop(FILE *source)
{
	char buffer[SE_SOCKET_MSG_BUF_SIZE];
	int item_len, error_code;
	char tmp_file_name[1024];

	int successResult = EXIT_SUCCESS;
	if(source == NULL) 
	{
		fprintf(stderr, "Failed to get input file\n");
		return 1;
	}
	
#ifdef _WIN32
     BOOL fSuccess; 
     fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) TermCtrlHandler, TRUE);                           // add to list 
     if (!fSuccess) throw USER_EXCEPTION(SE4207);
#else
     // For Control-C or Delete
     if ((int)signal(SIGINT, TermCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
	 // For Control-backslash
     if ((int)signal(SIGQUIT, TermCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
	 //For reboot or halt
     if ((int)signal(SIGTERM, TermCtrlHandler) == -1) throw USER_EXCEPTION(SE4207);
#endif

	//open session
   // strcat(strcat(host,":"),std::string(itoa(socket_port, buffer, 10)).c_str());
    int res = SEconnect(&conn, host, db_name, login, password);
	
    if(res != SEDNA_SESSION_OPEN)
    {
	   fprintf(stderr, "failed to open session \n%s\n", SEgetLastErrorMsg(&conn));
	   return 1;
    }

	set_sedna_data();
	
	term_output1("Welcome to term, the SEDNA Interactive Terminal. \n\n");
	term_output1("Type:\n");
	
	slash_commands_help();

	while(successResult == EXIT_SUCCESS)
	{
		term_output2("\n%s> ",db_name);

		successResult = get_input_item(source, buffer, &item_len, tmp_file_name);

		switch (successResult) {
        case EXIT_GOT_COMMAND:
			res = process_command(buffer);
            if(res == EXIT_USER)                      // normal exit
			{
				quit_term();
				goto OUT_OF_CYCLE;
			}
            else if(res == EXIT_CONNECTION_BROKEN)     // socket connection is broken
            {
                quit_term();
                return EXIT_CONNECTION_BROKEN;
            }
            else if((res == EXIT_STATEMENT_OR_COMMAND_FAILED) && (on_error_stop))
            {
                quit_term();
                return EXIT_STATEMENT_OR_COMMAND_FAILED;  // statement or command failed and on_error_stop is on
            }
			successResult = EXIT_SUCCESS;                 // cycle continues
            break;
            
		case EXIT_GOT_QUERY:
        case EXIT_GOT_LONG_QUERY:
			res = (successResult == EXIT_GOT_QUERY) ? process_query(buffer, false, tmp_file_name) : process_query(buffer, true, tmp_file_name);
            if((res == EXIT_STATEMENT_OR_COMMAND_FAILED) && (on_error_stop))
            {
                quit_term();
				return EXIT_STATEMENT_OR_COMMAND_FAILED;  // statement or command failed and on_error_stop is on
            }
            else if(res == EXIT_CONNECTION_BROKEN)
            {
                quit_term();
                return EXIT_CONNECTION_BROKEN;            // socket connection is broken
            }
            else if(res == EXIT_TERM_FAILED)
            {
                quit_term();
                return EXIT_TERM_FAILED;                  // terminal failed
            }
			successResult = EXIT_SUCCESS;                 // cycle continues
            break;
            
		case EXIT_EOF:
			if(show_time != 0)
			{
				term_output2("Time: %s\n",SEshowTime(&conn));
			}
			if(source != stdin)
			{
				if((SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE) && (!conn.autocommit))
				{
					term_output1("Committing transaction...");
        		    res = SEcommit(&conn);
	            	if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
		    		{
		    			fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
                        error_code = SEgetLastErrorCode(&conn);
                        // if socket is broken
                        if((error_code == 207) || (error_code == 208))
                            return EXIT_CONNECTION_BROKEN;           // socket connection is broken
                        else
                            return EXIT_STATEMENT_OR_COMMAND_FAILED; // statement or command failed and on_error_stop is on
	    			}
					term_output1("Ok\n");
				}
				quit_term();
			}
			goto OUT_OF_CYCLE;
            break;
            
         default: 
            break;
        }// end of switch
	} //end of while
	
OUT_OF_CYCLE:	return EXIT_SUCCESS;
}

// returns: EXIT_SUCCESS; EXIT_USER; EXIT_STATEMENT_OR_COMMAND_FAILED; EXIT_CONNECTION_BROKEN
int process_command(char* buffer)
{
    int res, error_code;

   	if(strcmp(buffer,"?") == 0)
	{
        slash_commands_help();
	    return EXIT_SUCCESS;
	}
   	if(strcmp(buffer,"showtime") == 0)
	{
		term_output2("Time: %s\n", SEshowTime(&conn));
	    return EXIT_SUCCESS;
	}
	if(strcmp(buffer,"commit") == 0)
	{
        if(conn.autocommit)
        {
		    term_output1("This session is in the autocommit mode.\nTo commit transactions manually turn AUTOCOMMIT off: \\unset AUTOCOMMIT.");
            return EXIT_SUCCESS;
        }
        else
        {
            term_output1("Committing transaction...");
            if(SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
            {
                res = SEcommit(&conn);
                if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
                {
                    fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
                    error_code = SEgetLastErrorCode(&conn);
                    // if socket is broken
                    if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
                    else return EXIT_STATEMENT_OR_COMMAND_FAILED;
                }
            }
            term_output1("Ok\n");
            return EXIT_SUCCESS;
        }
	}
	else if(strcmp(buffer,"rollback") == 0)
	{
        if(conn.autocommit)
        {
		    term_output1("This session is in the autocommit mode.\nTo rollback transactions manually turn AUTOCOMMIT off: \\unset AUTOCOMMIT.");
            return EXIT_SUCCESS;
        }
        else
        {
            term_output1("Rollback transaction...");
            if(SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
            {
               res = SErollback(&conn);
               if(res != SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED) 
               {
                   fprintf(stderr, "Rollback transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
                   error_code = SEgetLastErrorCode(&conn);
                   // if socket is broken
                   if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
                   return EXIT_STATEMENT_OR_COMMAND_FAILED;
               }
            }
            term_output1("Ok\n");
            return EXIT_SUCCESS;
        }
	}
	else if(strcmp(buffer,"quit") == 0)
	{
       if((SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE) && (!conn.autocommit))
       {
			term_output1("Committing transaction...");

    	    res = SEcommit(&conn);
		    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
	    	{
	    		fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
                error_code = SEgetLastErrorCode(&conn);
                // if socket is broken
                if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
	    		return EXIT_STATEMENT_OR_COMMAND_FAILED;
	    	}
			term_output1("Ok\n");
		}
		return EXIT_USER;
	}
	else if(strncmp(buffer,"set",3) == 0)
	{
        if((strcmp(buffer+4, "AUTOCOMMIT") != 0) && (strcmp(buffer+4, "ON_ERROR_STOP") != 0))
        {
	    	fprintf(stderr, "Unknown variable.\n");
		    return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
        if(strcmp(buffer+4, "AUTOCOMMIT") == 0)
        {
            int value = SEDNA_AUTOCOMMIT_ON;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));
            term_output1("Autocommit mode is on.");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+4, "ON_ERROR_STOP") == 0)
        {
            on_error_stop = true;
            term_output1("Variable is set.");
            return EXIT_SUCCESS;
        }
	}
	else if(strncmp(buffer,"unset",5) == 0)
	{
        if((strcmp(buffer+6, "AUTOCOMMIT") != 0) && (strcmp(buffer+6, "ON_ERROR_STOP") != 0))
        {
	    	fprintf(stderr, "Unknown variable.\n");
		    return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
        if(strcmp(buffer+6, "AUTOCOMMIT") == 0)
        {
            int value = SEDNA_AUTOCOMMIT_OFF;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));
            term_output1("Autocommit mode is off.");
            return EXIT_SUCCESS;
        }
        if(strcmp(buffer+6, "ON_ERROR_STOP") == 0)
        {
            on_error_stop = false;
            term_output1("Variable is unset.");
            return EXIT_SUCCESS;
        }
    }
	else 
	{
		fprintf(stderr, "Unknown command. Print \\? - for help on internal slash commands\n");
		
		return EXIT_STATEMENT_OR_COMMAND_FAILED;
	}
}

int process_query(char* buffer, bool is_query_from_file, char* tmp_file_name)
{
	int result, error_code;
	char buf[RESULT_MSG_SIZE+1];
	FILE* long_query;
	
    if((SEtransactionStatus(&conn) == SEDNA_NO_TRANSACTION) && (!conn.autocommit))
    {
		//begin transaction
		result = SEbegin(&conn);
		if(result != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
		{
			fprintf(stderr, "failed to begin transaction\n%s\n", SEgetLastErrorMsg(&conn));
            error_code = SEgetLastErrorCode(&conn);
            // if socket is broken
            if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
            else return EXIT_STATEMENT_OR_COMMAND_FAILED;
		}
	}
    // execute XQuery query	or update
    if(is_query_from_file)
    {
    	if( (long_query = fopen(tmp_file_name, "r")) == NULL)
        {
            fprintf(stderr,"failed to open file\n");
            return EXIT_TERM_FAILED;
        }
        result = SEexecuteLong(&conn, long_query); 
    	if(0 != fclose(long_query))
        {
            fprintf(stderr,"failed to close file\n");
            return EXIT_TERM_FAILED;
        }
    	if(0 == uDeleteFile(tmp_file_name, NULL))
        {
            fprintf(stderr,"failed to delete file\n");
            return EXIT_TERM_FAILED;
        }
    }
    else
    {
    	result = SEexecute(&conn, buffer); 
    }
    
    if(result == SEDNA_QUERY_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
        if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_UPDATE_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_BULK_LOAD_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_ERROR) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_QUERY_SUCCEEDED) 
    {
    	//iterate over the result sequece and retrieve the result data
    	int bytes_read;
    	result = SEnext(&conn);
    	term_output1("result:\n");
    	
    	while((result != SEDNA_RESULT_END)&&(result != SEDNA_ERROR))
    	{
    		bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
            if (bytes_read == SEDNA_ERROR)
            {
       	        fprintf(stderr, "\nNext item failed: \n%s\n", SEgetLastErrorMsg(&conn));
            	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
                error_code = SEgetLastErrorCode(&conn);
                // if socket is broken
                if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
                else return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
    		while(bytes_read > 0)
    		{
	    		buf[bytes_read] = '\0';
    			fprintf(res_os, "%s", buf);
    			bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
                if (bytes_read == SEDNA_ERROR)
                {
       	            fprintf(stderr, "\nNext item failed: \n%s\n", SEgetLastErrorMsg(&conn));
                	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
                    error_code = SEgetLastErrorCode(&conn);
                    // if socket is broken
                    if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
                    else return EXIT_STATEMENT_OR_COMMAND_FAILED;
                }
    		}
    		result = SEnext(&conn);
    	}
		fprintf(res_os, "\n");
    }
    else if(result == SEDNA_UPDATE_SUCCEEDED) 
    {
    	term_output1("Update succeeded\n");
    }
    else if(result == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
    	term_output1("Bulk load succeeded\n");
    }
    else 
    {
    	fprintf(stderr, "Unknown message from server\n");
        return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
	return EXIT_SUCCESS;
}

int get_input_item(FILE* source, char* buffer, int* item_len, char* tmp_file_name)
{
	int successResult = EXIT_SUCCESS;
	int i = 0;     //position in the buffer
	int number_of_msg_buf_size = 0;  //if a query is longer then socket_msg_buf_size
	FILE* f;
	//test if the read line is a command
	bool isCommand = false;
    buffer[i] = (char)getc(source); 
    if(buffer[i] == '\\') {isCommand = true; i--;}
    else 
    {
    	while((buffer[i] == '\n')||(buffer[i] == ' '))
    	{
    		buffer[i] = (char)getc(source);
    	}
    }
    if(buffer[i] == EOF)  return EXIT_EOF;
    
    i++;

	while(successResult==EXIT_SUCCESS)
	{
		buffer[i] = (char)getc(source);

		if((!isCommand)&&((buffer[i-1] == (char)';')||(buffer[i-1] == (char)'&'))&&((buffer[i] == (char)'\n')||(buffer[i] == EOF)))
		{
			*item_len = i-1;
			buffer[*item_len] = '\0';
			fflush(stdin);
			successResult = EXIT_GOT_QUERY; 
		}
		else if((isCommand)&&(buffer[i] == (char)'\n'))
		{
			*item_len = i;
			buffer[*item_len] = '\0';
			fflush(stdin);
			successResult = EXIT_GOT_COMMAND;
		}
		else if(i == SE_SOCKET_MSG_BUF_SIZE - 10)
		{
			if(!number_of_msg_buf_size)
			{
				std::string tmp_file_path_str = std::string(SEDNA_DATA) + std::string("/data/") + std::string(db_name) + std::string("_files");
				if(!uGetUniqueFileName(tmp_file_path_str.c_str(), tmp_file_name, NULL)) throw USER_EXCEPTION(SE4052);
				f = fopen(tmp_file_name,"w+t");
			}
			number_of_msg_buf_size++;
			fwrite(buffer, sizeof( char ), i+1, f);
			i = 0;
		}
		else if((isCommand)&&(buffer[i] == EOF))
		{
			*item_len = i;
			buffer[*item_len] = '\0';
			successResult = EXIT_GOT_COMMAND;
		}
		else if((!isCommand)&&(buffer[i] == EOF))
		{
			*item_len = i;
			buffer[*item_len] = '\0'; 
			successResult = EXIT_GOT_QUERY;
		}
		else if(buffer[i] != '\r')
		{
			i++;
		}
	}   // end of while
	
	*item_len = number_of_msg_buf_size*SE_SOCKET_MSG_BUF_SIZE + i;
	if(number_of_msg_buf_size)
	{
		fwrite(buffer, sizeof( char ), strlen(buffer), f);
		fclose(f);
		successResult = EXIT_GOT_LONG_QUERY;
	}	

	return successResult;
}


