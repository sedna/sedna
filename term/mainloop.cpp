/*
 * File:  mainloop.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <cctype>
#include "common/sedna.h"

#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uhdd.h"
#include "common/u/uutils.h"
#include "sp_defs.h"
#include "mainloop.h"
#include "term_globals.h"
#include "term_funcs.h"
#include "term_ile.h"

int quit_term()
{
	if(show_time != 0)
	{
		fprintf(stderr, "total time: %s\n",SEshowTime(&conn));
        fflush(stderr);
	}
	
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

void se_term_debug_handler(se_debug_info_type subtype, const char *msg)
{
    term_debug_info_output(msg);
}

int slash_commands_help()
{
	term_output1("Terminate queries with ampersand + line feed.\n");
	term_output1("\\?        - for help\n");
	term_output1("\\commit   - to commit transaction\n");
	term_output1("\\rollback - to rollback transaction\n");
	term_output1("\\showtime - to show the time of the latest query execution\n");
	term_output1("\\set      - to set the terminal internal variable (on help for internal variables type \\set?)\n");
	term_output1("\\unset    - to unset the terminal internal variable\n");
	term_output1("\\quit     - to close session and quit the Sedna Terminal\n");

	if (interactive_mode)
	{
		term_output1("\nInteractive line editing enabled.\n");
		term_output1("Alt-Up and Alt-Down to browse the history.\n");
		term_output1("Ctrl-Enter submits input instantly.\n");
	}

	return 0;
}

int set_commans_help()
{
	term_output1("set and unset meta-commands are used for managing se_term internal variables. \n");
    term_output1("There are following se_term internal varibales:\n");
    term_output1("-----------------------------------------------------------------------\n");
    term_output1("AUTOCOMMIT -        when set, autocommit mode is on. \n");
    term_output1("                    When unset manual-commit mode is on. \n"); 
    term_output1("                    AUTOCOMMIT mode is set by default.\n");
    term_output1("-----------------------------------------------------------------------\n");
    term_output1("ON_ERROR_STOP -     when set, se_term returns with the code 3 when\n");
    term_output1("                    statement or meta-command fails. \n");
    term_output1("                    When unset se_term processing continues,\n");
    term_output1("                    unless it is the connection failure. \n");
    term_output1("-----------------------------------------------------------------------\n");
    term_output1("DEBUG -             when set, session debug mode is on.\n");
    term_output1("                    When unset, session debug mode is off.\n");
    term_output1("-----------------------------------------------------------------------\n");
    term_output1("TRANSACTION_READ_ONLY - when set, transactions are run as READ-ONLY.\n");
    term_output1("                        When unset, transactions are run as UPDATE-transactions.\n");
    term_output1("                        By default transactions are run as UPDATE-transactions.\n");
    term_output1("-----------------------------------------------------------------------\n");
    
   	return 0;
}

/*
 * Main processing loop for reading lines of input
 *	and sending them to the backend.
 */
int
MainLoop(FILE *source)
{
	std::vector<char> buffer;
	int res=0, error_code=0;
	int successResult = EXIT_SUCCESS;

	if(source == NULL) 
	{
		fprintf(stderr, "Failed to get input file\n");
        fflush(stderr);
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
   
    if (strpbrk(host, ":") == NULL) 
	{
		sprintf(host+strlen(host),":%d",socket_port);
	}
		
    res = SEconnect(&conn, host, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN)
    {
	   fprintf(stderr, "failed to open session \n%s\n", SEgetLastErrorMsg(&conn));
       fflush(stderr);
	   return 1;
    }
	/* if we read query from file, set session directory to the one file is located in */ 
    if (strcmp(filename,"???") != 0)
    {
        char file_abs_path[U_MAX_PATH+1];
        if (uGetAbsoluteFilePath(filename, file_abs_path, U_MAX_PATH, __sys_call_error) == NULL)
        {
	        fprintf(stderr, "failed to get an absolute path of the script file \n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
			quit_term();
	        return 1;
        }
        if (uGetDirectoryFromFilePath(file_abs_path, session_dir, U_MAX_DIR, __sys_call_error) == NULL)
        {
	        fprintf(stderr, "failed to get a directory from the file path \n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
			quit_term();
	        return 1;
        }
	    if (SEsetConnectionAttr(&conn, SEDNA_ATTR_SESSION_DIRECTORY, session_dir, strlen(session_dir)) != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
        {
	        fprintf(stderr, "failed to set the Sedna session directory attribute \n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
			quit_term();
	        return 1;
        }
    }

    SEsetDebugHandler(&conn, (debug_handler_t)se_term_debug_handler);

    int debug_option = debug_mode ? SEDNA_DEBUG_ON : SEDNA_DEBUG_OFF;
    if (SEsetConnectionAttr(&conn, SEDNA_ATTR_DEBUG, (void*)&debug_option, sizeof(int)) != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
    {
        fprintf(stderr, "failed to set the Sedna debug mode attribute \n%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
		quit_term();
        return 1;
    }
    
	term_output1("Welcome to term, the SEDNA Interactive Terminal. ");
	term_output1("Type \\? for help.\n");
	
	//slash_commands_help();

	sprintf(prompt,"%.13s> ",db_name);

	while(successResult == EXIT_SUCCESS)
	{
		successResult = get_input_item(source, buffer);

		switch (successResult) {
        case EXIT_GOT_COMMAND:
			res = process_command(&buffer[0]);
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
			res = process_query(&buffer[0], false, "");
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
		    term_output1("This session is in the autocommit mode.\nTo commit transactions manually turn AUTOCOMMIT off: \\unset AUTOCOMMIT.\n");
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
                    fflush(stderr);
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
		    term_output1("This session is in the autocommit mode.\nTo rollback transactions manually turn AUTOCOMMIT off: \\unset AUTOCOMMIT.\n");
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
                   fflush(stderr);
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
                fflush(stderr);
                error_code = SEgetLastErrorCode(&conn);
                // if socket is broken
                if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
	    		return EXIT_STATEMENT_OR_COMMAND_FAILED;
	    	}
			term_output1("Ok\n");
		}
		return EXIT_USER;
	}
  	else if(strcmp(buffer,"set?") == 0)
    {
        set_commans_help();
        return EXIT_SUCCESS;
    }

	else if(strncmp(buffer,"set ",4) == 0)
	{
        if(strcmp(buffer+4, "AUTOCOMMIT") == 0)
        {
            int value = SEDNA_AUTOCOMMIT_ON;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));
            term_output1("Autocommit mode is on.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+4, "ON_ERROR_STOP") == 0)
        {
            on_error_stop = true;
            term_output1("Variable is set.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+4, "DEBUG") == 0)
        {
            int value = SEDNA_DEBUG_ON;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_DEBUG, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set debug mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output1("Debug mode is on.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+4, "TRANSACTION_READ_ONLY") == 0)
        {
            int value = SEDNA_READONLY_TRANSACTION;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_CONCURRENCY_TYPE, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set transaction mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            if (SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
                term_output1("The current and next transactions will be run as READ-ONLY.\n");
            else
                term_output1("The next transactions will be run as READ-ONLY.\n");
            
            return EXIT_SUCCESS;
        }
        else
        {
   	    	fprintf(stderr, "Unknown variable.\n");
            fflush(stderr);
		    return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
	}
	else if(strncmp(buffer,"unset ",6) == 0)
	{
        if(strcmp(buffer+6, "AUTOCOMMIT") == 0)
        {
            int value = SEDNA_AUTOCOMMIT_OFF;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));
            term_output1("Autocommit mode is off.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+6, "ON_ERROR_STOP") == 0)
        {
            on_error_stop = false;
            term_output1("Variable is unset.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+6, "DEBUG") == 0)
        {
            int value = SEDNA_DEBUG_OFF;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_DEBUG, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set debug mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output1("Debug mode is off.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+6, "TRANSACTION_READ_ONLY") == 0)
        {
            int value = SEDNA_UPDATE_TRANSACTION;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_DEBUG, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set transaction mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            if (SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
                term_output1("The current and following transactions will be run as UPDATE-transactions.\n");
            else
                term_output1("The following transactions will be run as UPDATE-transactions.\n");
            return EXIT_SUCCESS;
        }
        else
        {
	    	fprintf(stderr, "Unknown variable.\n");
            fflush(stderr);
		    return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
    }
	else 
	{
		fprintf(stderr, "Unknown command. Print \\? - for help on internal slash commands\n");
        fflush(stderr);
		
		return EXIT_STATEMENT_OR_COMMAND_FAILED;
	}
}

int process_query(char* buffer, bool is_query_from_file, char* tmp_file_name)
{
	int result=0, error_code=0, have_results=0;
	char buf[RESULT_MSG_SIZE+1];
	FILE* long_query;

    if((SEtransactionStatus(&conn) == SEDNA_NO_TRANSACTION) && (!conn.autocommit))
    {
		//begin transaction
		result = SEbegin(&conn);
		if(result != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
		{
			fprintf(stderr, "failed to begin transaction\n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
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
            fflush(stderr);
            return EXIT_TERM_FAILED;
        }
        result = SEexecuteLong(&conn, long_query); 
    	if(0 != fclose(long_query))
        {
            fprintf(stderr,"failed to close file\n");
            fflush(stderr);
            return EXIT_TERM_FAILED;
        }
    	if(0 == uDeleteFile(tmp_file_name, NULL))
        {
            fprintf(stderr,"failed to delete file\n");
            fflush(stderr);
            return EXIT_TERM_FAILED;
        }
    }
    else
    {
    	result = SEexecute(&conn, buffer); 
    }
    
    if(result == SEDNA_QUERY_FAILED) 
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
        if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_UPDATE_FAILED) 
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_BULK_LOAD_FAILED) 
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_ERROR) 
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
    	fflush(stderr);
        if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_QUERY_SUCCEEDED) 
    {
    	//iterate over the result sequece and retrieve the result data
    	const char * i;
	int bytes_read, res_next;

	if (buffer && (i=strchr(buffer,'\n')) && i[1])
        term_output1("\n");

 //       term_debug_info_output(); // output debug info if there was any

    	res_next = SEnext(&conn);
        if((res_next == SEDNA_NEXT_ITEM_FAILED) || (res_next == SEDNA_ERROR))
        {
            fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
            return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
        
    	while((res_next != SEDNA_RESULT_END)&&(res_next != SEDNA_ERROR))
    	{
			have_results=1;
    		bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
            if (bytes_read == SEDNA_ERROR)
            {
       	        fprintf(stderr, "Next item failed: \n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
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
                fflush(res_os);
    			bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
                if (bytes_read == SEDNA_ERROR)
                {
       	            fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
                    fflush(stderr);
                	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
                    error_code = SEgetLastErrorCode(&conn);
                    // if socket is broken
                    if((error_code == 207) || (error_code == 208)) return EXIT_CONNECTION_BROKEN;
                    else return EXIT_STATEMENT_OR_COMMAND_FAILED;
                }
    		}
 //           term_debug_info_output(); // output debug info if there was any

    		res_next = SEnext(&conn);
            if((res_next == SEDNA_NEXT_ITEM_FAILED) || (res_next == SEDNA_ERROR))
            {
                fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                break;
            }
    	}
	if (1 || have_results) term_output1("\n\n");
    }
    else if(result == SEDNA_UPDATE_SUCCEEDED) 
    {
 //       term_debug_info_output(); // output debug info if there was any
        
    	fprintf(res_os, "UPDATE is executed successfully\n");
        fflush(res_os);
    }
    else if(result == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
//        term_debug_info_output(); // output debug info if there was any
        
    	fprintf(res_os, "Bulk load succeeded\n");
        fflush(res_os);
    }
    else 
    {
    	fprintf(stderr, "Unknown message from server\n");
        fflush(stderr);
        return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
	return EXIT_SUCCESS;
}

static size_t append_line_to_buffer(FILE* source, std::vector<char> & buffer, size_t sz)
{
	const char * str=0;
	while(1)
	{
		buffer.resize(sz+1024);
		str=fgets(&buffer[sz],buffer.size()-sz,source);
		if (0==str) break;
		sz+=strlen(str);
		if(buffer[sz-1]=='\n') break;				
	}
	buffer[sz]='\0';
	return sz;
}

size_t translate_amps_sequence(const char * str, size_t start, bool & is_terminator)
{
	size_t sstart=0;
	char test[3]={0,0,0};
	if (sstart+3<start) sstart=start-3;
	memcpy(test+3-(start-sstart),str+sstart,start-sstart);
	is_terminator=true;
	if(test[2]=='&') return start-1;
	if(test[1]=='&' && test[2]=='\n') return start-2;
	if(test[0]=='&' && test[1]=='\r' && test[2]=='\n') return start-3;
	is_terminator=false;	
	return start;
}

int get_input_item(FILE* source, std::vector<char> & buffer)
{
	const char * str=0, * estr = 0;
	int pos=0, error=0, ret=0;
	bool is_terminator=false;
	size_t sz;

	if (interactive_mode) 
	{
		while(1)
		{
			str=ile_gets(&sz);
			if(!str) return EXIT_EOF;
			error=sscanf(str,"%*1s%n",&pos);
			if (error>=0 && error!=EOF) break;
		}
		estr=str+sz;
		str+=pos-1;
		sz=0;
		if (*str=='\\') 
		{
			do str++; while(str!=estr&&isspace(*str));
			ret=EXIT_GOT_COMMAND;
		}
		else 
		{
			ret=EXIT_GOT_QUERY;
		}
		sz=estr-str;
		buffer.resize(sz+1);
		memcpy(&buffer[0],str,sz);
		str=estr=0;
		if (ret==EXIT_GOT_QUERY) sz=translate_amps_sequence(&buffer[0],sz,is_terminator);		
	}
	else 
	{
		while(1)
		{
			term_output2("%s",prompt);
			sz=append_line_to_buffer(source,buffer,0);
			str=&buffer[0];
			error=sscanf(str,"%*1s%n",&pos);
			if (error>=0 && error!=EOF) break;
			if (feof(source) || ferror(source)) return EXIT_EOF;
		}
		estr=str+sz;
		str+=pos-1;
		sz=0;
		if (*str=='\\')
		{
			str++;
			ret=EXIT_GOT_COMMAND;
		}
		else ret=EXIT_GOT_QUERY;
		while(str!=estr&&isspace(*str))++str;
		sz=estr-str;
		memmove(&buffer[0],str,sz); /* data overlaps! */ 
		str=estr=0;
		if (ret==EXIT_GOT_QUERY) while(1)
		{
			sz=translate_amps_sequence(&buffer[0],sz,is_terminator);
			if(is_terminator || feof(source) || ferror(source)) break;
			sz=append_line_to_buffer(source,buffer,sz);
		}
	}
	while(sz>0&&isspace(buffer[sz-1]))--sz;
	buffer.resize(sz+1);
	buffer[sz]='\0';
	return ret;
}
