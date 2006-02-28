/*
 * File:  mainloop.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>


#include "base.h"
#include "d_printf.h"
#include "uhdd.h"
#include "exceptions.h"

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
	term_output1("    \\quit - to close session and quit the Sedna Terminal\n");
	term_output1("    XQuery/Update statements ended with semicolon+line feed\n ");
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
	int item_len;
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
		if(successResult == EXIT_GOT_COMMAND)
		{
			res = process_command(buffer);
            if((res == EXIT_USER) || (res == EXIT_COMMIT_FAILED) || (res == EXIT_ROLLBACK_FAILED))
			{
				quit_term();
				break;
			}
			successResult = EXIT_SUCCESS;
		}
		else if(successResult == EXIT_GOT_QUERY)
		{
			res = process_query(buffer, false, tmp_file_name);
			successResult = res;
		}
		else if(successResult == EXIT_GOT_LONG_QUERY)
		{
			res = process_query(buffer, true, tmp_file_name);
			successResult = res;
		}
		else if(successResult == EXIT_EOF)
		{
			if(show_time != 0)
			{
				term_output2("Time: %s\n",SEshowTime(&conn));
			}
			if(source != stdin)
			{
				if(in_transaction)
				{
					term_output1("Committing transaction...");
        		    res = SEcommit(&conn);
	            	if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
		    		{
		    			fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
	    			}
					term_output1("Ok\n");
				}
				quit_term();
			}
			break;
		}
		else if(successResult == EXIT)
		{
			quit_term();
			break;
		}

	}
	
	return 0;
}

int process_command(char* buffer)
{
    int res;

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
		term_output1("Committing transaction...");
	
	    res = SEcommit(&conn);
	    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
	    {
	    	fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
	    	if(!in_transaction) return EXIT_SUCCESS;
	    	return EXIT_COMMIT_FAILED;
	    }
	    in_transaction = false;
	
		term_output1("Ok\n");
	    
	    return EXIT_SUCCESS;
	}
	else if(strcmp(buffer,"rollback") == 0)
	{
    	term_output1("Rollback transaction...");

	    res = SErollback(&conn);
	    if(res != SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED) 
	    {
	    	fprintf(stderr, "Rollback transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
	    	if(!in_transaction) return EXIT_SUCCESS;
	    	return EXIT_ROLLBACK_FAILED;
	    }
	    in_transaction = false;

		term_output1("Ok\n");

	    return EXIT_SUCCESS;
	}
	else if(strcmp(buffer,"quit") == 0)
	{
		if(in_transaction)
		{
			term_output1("Committing transaction...");

    	    res = SEcommit(&conn);
		    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
	    	{
	    		fprintf(stderr, "Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
	    		return EXIT_COMMIT_FAILED;
	    	}
	    	in_transaction = false;
	
			term_output1("Ok\n");
		}
		return EXIT_USER;
	}
	else 
	{
		fprintf(stderr, "Unknown command. Print \\? - for help on internal slash commands\n");
		
		return EXIT_NOT_COMMAND;
	}
}

int process_query(char* buffer, bool is_query_from_file, char* tmp_file_name)
{
	int result;
	char buf[RESULT_MSG_SIZE+1];
	FILE* long_query;
	
	if(!in_transaction) //begin transaction
	{
		//begin transaction
		result = SEbegin(&conn);
		if(result != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
		{
			fprintf(stderr, "failed to begin transaction\n%s\n", SEgetLastErrorMsg(&conn));
			return EXIT;
		}
		in_transaction = true;
	}
    // execute XQuery query	or update
    if(is_query_from_file)
    {
    	long_query = fopen(tmp_file_name, "r");
    	result = SEexecuteLong(&conn, long_query); 
    	fclose(long_query);
    	uDeleteFile(tmp_file_name);
    }
    else
    {
    	result = SEexecute(&conn, buffer); 
    }
    
    if(result == SEDNA_QUERY_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	term_output1("Rollback transaction...Ok \n");
    	in_transaction = false;
    	return EXIT_SUCCESS;
    }
    else if(result == SEDNA_UPDATE_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	term_output1("Rollback transaction...Ok \n");
    	in_transaction = false;
    	return EXIT_SUCCESS;
    }
    else if(result == SEDNA_BULK_LOAD_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	term_output1("Rollback transaction...Ok \n");
    	in_transaction = false;
    	return EXIT_SUCCESS;
    }
    else if(result == SEDNA_ERROR) 
    {
    	fprintf(stderr, "Failed: \n%s\n", SEgetLastErrorMsg(&conn));
        int res = SEgetLastErrorCode(&conn);
    	if ((res == 206) || (res == 207)) return EXIT;
    	term_output1("Rollback transaction...Ok \n");
    	in_transaction = false;
    	return EXIT_SUCCESS;
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
            	term_output1("Rollback transaction...Ok \n");
    	        in_transaction = false;
                return EXIT_SUCCESS;
            }
    		while(bytes_read > 0)
    		{
	    		buf[bytes_read] = '\0';
    			fprintf(res_os, "%s", buf);
    			bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
                if (bytes_read == SEDNA_ERROR)
                {
       	            fprintf(stderr, "\nNext item failed: \n%s\n", SEgetLastErrorMsg(&conn));
                	term_output1("Rollback transaction...Ok \n");
    	            in_transaction = false;
                    return EXIT_SUCCESS;
                }
    		}
    		result = SEnext(&conn);
    	}
		fprintf(res_os, "\n");
    }
    else if(result == SEDNA_UPDATE_SUCCEEDED) 
    {
    	term_output1("Update succeeded\n");    }
    else if(result == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
    	term_output1("Bulk load succeeded\n");
    }
    else 
    {
    	fprintf(stderr, "Unknown message from server\n");
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
		
		if((!isCommand)&&(buffer[i-1] == (char)';')&&(buffer[i] == (char)'\n'))
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
				if(!uGetUniqueFileName(tmp_file_path_str.c_str(), tmp_file_name)) throw USER_EXCEPTION(SE4052);
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
		else
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


