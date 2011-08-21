/*
 * File:  mainloop.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <cctype>
#include <assert.h>
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

using namespace term_globals;

int quit_term()
{
	if(show_time)
	{
		fprintf(stderr, "total time: %s\n secs",SEshowTime(&conn));
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
	term_output1("\\set      - to set the terminal internal variable\n");
    term_output1("             (on help for internal variables type \\set?)\n");
	term_output1("\\unset    - to unset the terminal internal variable\n");
	term_output1("\\quit     - to close session and quit the Sedna Terminal\n");

	if (interactive_mode)
	{
		term_output1("\nInteractive line editing enabled.\n");
		term_output1("Up and Down to browse the history.\n");
        term_output1("Left and Right to move through history-summoned query.\n");
        term_output1("TAB to try auto-completion.\n");
    }

	return 0;
}

int set_commans_help()
{
	term_output1("set and unset meta-commands are used for managing se_term internal variables. \n");
    term_output1("There are aliases for some commands.\nFor example, \\ac is equal to \\set AUTOCOMMIT. \n");
    term_output1("There are following se_term internal varibales:\n");
    term_output1("\nAUTOCOMMIT (\\ac - set, \\nac - unset)\n");
    term_output1("  When set, autocommit mode is on.\n  When unset manual-commit mode is on. \n");
    term_output1("  By default AUTOCOMMIT mode is set on.\n");
    term_output1("\nON_ERROR_STOP\n");
    term_output1("  When set, terminal returns with the code 3 when\n  statement or meta-command fails.\n");
    term_output1("  When unset terminal processing continues, unless\n  it is the connection failure. \n");
    term_output1("\nDEBUG\n");
    term_output1("  When set, session debug mode is on.\n  When unset, session debug mode is off.\n");
    term_output1("  In debug mode physical operations stack is printed when error is raised.\n");
    term_output1("\nTRANSACTION_READ_ONLY (\\ro - set, \\upd - unset)\n");
    term_output1("  When set, transactions are run as READ-ONLY.\n");
    term_output1("  When unset, transactions are run as UPDATE-transactions.\n");
    term_output1("  By default transactions are run as UPDATE-transactions.\n");
    term_output1("  (Setting this option will immediately commit any current transaction)\n");
    term_output1("\nLOG_LESS_MODE (\\ll - set, \\fl - unset) \n");
    term_output1("  When set, every bulkload will be less logged.\n  When unset, every bulkload will be\n");
    term_output1("  fully logged. By default transactions are run in full log mode.\n");
    term_output1("\n  Caveats:\n");
    term_output1("   1) this will commit any current transaction in this session!\n");
    term_output1("   2) every commit might be much longer!\n");
    term_output1("   3) transaction activity will be almost stalled since we're entering exclusive mode!\n");
    term_output1("\nQUERY_TIMEOUT=<time in secs>\n");
    term_output1("  When set, every query execution will be dropped on server if it lasts\n  longer than timeout set.\n");
    term_output1("  Default value = 0 (no timeout, query is executed as long as needed).\n");

   	return 0;
}

/*
 * Main processing loop for reading lines of input
 * and sending them to the backend.
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
     if (signal(SIGINT, TermCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
	 // For Control-backslash
     if (signal(SIGQUIT, TermCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
	 //For reboot or halt
     if (signal(SIGTERM, TermCtrlHandler) == SIG_ERR) throw USER_EXCEPTION(SE4207);
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
	/*
     * If we are going to query from file,
     * set session directory to the one file is located in.
     */
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
        if (uGetDirectoryFromFilePath(file_abs_path, session_dir, U_MAX_PATH, __sys_call_error) == NULL)
        {
	        fprintf(stderr, "failed to get a directory from the file path \n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
			quit_term();
	        return 1;
        }
	    if (SEsetConnectionAttr(&conn, SEDNA_ATTR_SESSION_DIRECTORY, session_dir, (int)strlen(session_dir)) != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
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
			res = process_query(&buffer[0], false, (char *)"");
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
                        if((error_code == SE3006) || (error_code == SE3007))
                            return EXIT_CONNECTION_BROKEN;           // socket connection is broken
                        else
                            return EXIT_STATEMENT_OR_COMMAND_FAILED; // statement or command failed and on_error_stop is on
	    			}
					term_output1("Ok\n");
				}
			}
            quit_term();
			goto OUT_OF_CYCLE;
            break;

         default:
            break;
        }// end of switch
	} //end of while

OUT_OF_CYCLE:	return EXIT_SUCCESS;
}

struct comm_alias
{
    const char *alias;
    const char *real;
};

comm_alias aliases[] =
{
    {"ro", "set TRANSACTION_READ_ONLY"},
    {"upd", "unset TRANSACTION_READ_ONLY"},
    {"ac", "set AUTOCOMMIT"},
    {"nac", "unset AUTOCOMMIT"},
    {"ll", "set LOG_LESS_MODE"},
    {"fl", "unset LOG_LESS_MODE"},
    {"q", "quit"},
    {NULL, NULL}
};

// very primitive alias finder
static
const char *find_alias(char *cmd)
{
    size_t i;

    if (!strlen(cmd)) return NULL;

    // first, trim trailing spaces
    for (i = strlen(cmd) - 1; i > 0 && isspace(cmd[i]); i--)
        ;
    if(i == 0 && isspace(cmd[0])) cmd[0] = '\0';
    else cmd[i + 1] = '\0';

    // then, look for alias
    for (i = 0; aliases[i].alias != NULL; i++)
    {
        if (strcmp(aliases[i].alias, cmd) == 0)
        {
            return aliases[i].real;
        }
    }

    // haven't found alias
    return NULL;
}

// returns: EXIT_SUCCESS; EXIT_USER; EXIT_STATEMENT_OR_COMMAND_FAILED; EXIT_CONNECTION_BROKEN
int process_command(char* buffer)
{
    int res, error_code;
    const char *alias = NULL;

    // resolve aliases; buffer may be changed!
    alias = find_alias(buffer);
    if (alias)
        buffer = (char *)alias;

    if(strcmp(buffer,"?") == 0)
	{
        slash_commands_help();
	    return EXIT_SUCCESS;
	}
   	if(strcmp(buffer,"showtime") == 0)
	{
		term_output2("Time: %s\n secs", SEshowTime(&conn));
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
                    if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
                   if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
                if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
            /*
            if (SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
                term_output1("The current and next transactions will be run as READ-ONLY.\n");
            else
                term_output1("The next transactions will be run as READ-ONLY.\n");
            */
            term_output1("The next transactions will be run as READ-ONLY.\n");

            return EXIT_SUCCESS;
        }
        else if(strncmp(buffer+4, "QUERY_TIMEOUT", 13) == 0)
        {
            char* p = strrchr(buffer, '=');
            if(!p)
            {
                fprintf(stderr, "Invalid use of se_term command. See help (\\set?)\n");
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            int value = (*(p+1) == ' ') ? atoi(p+2) : atoi(p+1);
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_QUERY_EXEC_TIMEOUT, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set query timeout.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output2("Timeout for query execution is set to %d.\n", (const void*)value);
            return EXIT_SUCCESS;
        }
        else if (strncmp(buffer+4, "LOG_LESS_MODE", 13) == 0)
        {
            int value = SEDNA_LOG_LESS;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_LOG_AMOUNT, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set transaction mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output1("The following transactions will be run in bulkload logless mode.\n");

            return EXIT_SUCCESS;
        }
        else
        {
   	    	fprintf(stderr, "Unknown variable. See \\set? for help\n");
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
        else if(strcmp(buffer+6, "QUERY_TIMEOUT") == 0)
        {
            int value = 0;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_QUERY_EXEC_TIMEOUT, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to unset query timeout.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output1("Timeout for query execution is unset.\n");
            return EXIT_SUCCESS;
        }
        else if(strcmp(buffer+6, "TRANSACTION_READ_ONLY") == 0)
        {
            int value = SEDNA_UPDATE_TRANSACTION;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_CONCURRENCY_TYPE, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set transaction mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            /*
            if (SEtransactionStatus(&conn) == SEDNA_TRANSACTION_ACTIVE)
                term_output1("The current and following transactions will be run as UPDATE-transactions.\n");
            else
                term_output1("The following transactions will be run as UPDATE-transactions.\n");
            */
            term_output1("The following transactions will be run as UPDATE-transactions.\n");
            return EXIT_SUCCESS;
        }
        else if (strncmp(buffer+6, "LOG_LESS_MODE", 13) == 0)
        {
            int value = SEDNA_LOG_FULL;
            res = SEsetConnectionAttr(&conn, SEDNA_ATTR_LOG_AMOUNT, (void*)&value, sizeof(int));
            if (res != SEDNA_SET_ATTRIBUTE_SUCCEEDED)
            {
                fprintf(stderr, "Failed to set transaction mode.\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
            term_output1("The following transactions will be run in full log mode.\n");

            return EXIT_SUCCESS;
        }
        else
        {
	    	fprintf(stderr, "Unknown variable. See \\set? for help\n");
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
            if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
            else return EXIT_STATEMENT_OR_COMMAND_FAILED;
		}
	}

    // execute XQuery query	or update
    if(is_query_from_file)
    {
        result = SEexecuteLong(&conn, tmp_file_name);

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
        if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_UPDATE_FAILED)
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_BULK_LOAD_FAILED)
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
        else return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    else if(result == SEDNA_ERROR)
    {
    	fprintf(stderr, "%s\n", SEgetLastErrorMsg(&conn));
    	fflush(stderr);
        if(!conn.autocommit) term_output1("Rollback transaction...Ok \n");
        error_code = SEgetLastErrorCode(&conn);
        // if socket is broken
        if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
                if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
                    if((error_code == SE3006) || (error_code == SE3007)) return EXIT_CONNECTION_BROKEN;
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
//		fprintf(res_os, "\n");
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
		str=fgets(&buffer[sz],(int)(buffer.size()-sz),source);
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
	char *str=0, *estr = 0, *fstr = 0;
	int pos=0, error=0, ret=0;
	bool is_terminator=false;
	size_t sz;

	if (interactive_mode)
	{
		while(1)
		{
			fstr = str = ile_gets(&sz);
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
        free(fstr); // we need to free readline-returned string
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
