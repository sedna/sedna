/*
 * File:  term_funcs.cpp
 * Copyright (C) 2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"

#include "libsedna.h"
#include "common/base.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "term_funcs.h"
#include "term_globals.h"
#include "mainloop.h"

using namespace term_globals;

void term_output1(const char *buf)
{
    if (term_globals::echo)
    {
        fprintf(res_os, "%s", buf);
        fflush(res_os);
    }
}

void term_output2(const char *buf, const char* arg1)
{
    if (term_globals::echo)
    {
        char echo_buf[256];
        sprintf(echo_buf,buf,arg1);
        fprintf(res_os, "%s", echo_buf);
        fflush(res_os);
    }
}

void term_output2(const char *buf, int arg1)
{
    if (term_globals::echo)
    {
        char echo_buf[256];
        sprintf(echo_buf,buf,arg1);
        fprintf(res_os, "%s", echo_buf);
        fflush(res_os);
    }
}

void term_output3(const char *buf, const void* arg1, const void* arg2)
{
    if (term_globals::echo)
    {
    	char echo_buf[256];
    	sprintf(echo_buf,buf,arg1,arg2);
    	fprintf(res_os, "%s", echo_buf);
        fflush(res_os);
    }
}

void term_debug_info_output(const char *msg)
{
    if (term_globals::debug_output)
    {
        fprintf(res_os, "\n%s\n", msg);
        fflush(res_os);
    }
}

int process_commandline_query()
{
    SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
    char buf[RESULT_MSG_SIZE+1];
    
   	term_output2("%s> ",db_name);
    strcat(strcat(host,":"),std::string(u_itoa(socket_port, buf, 10)).c_str());
    
    int res = SEconnect(&conn, host, db_name, login, password);
	
    if(res != SEDNA_SESSION_OPEN){
    	fprintf(stderr, "failed to open session \n%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
	
    // execute XQuery query	or update
    res = SEexecute(&conn, query); 
    if(res == SEDNA_QUERY_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_UPDATE_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_BULK_LOAD_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
        fflush(stderr);
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_ERROR) 
    {
    	fprintf(stderr, "\n%s\n%s", SEgetLastErrorMsg(&conn),query);
        fflush(stderr);
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_QUERY_SUCCEEDED) 
    {
    	//iterate over the result sequece and retrieve the result data
    	int bytes_read;
        
//        term_debug_info_output(); // output debug info if there was any

    	res = SEnext(&conn);
        if((res == SEDNA_NEXT_ITEM_FAILED) || (res == SEDNA_ERROR))
        {
            fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
            fflush(stderr);
            return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }

    	while((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR))
    	{
    		bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
            if (bytes_read == SEDNA_ERROR)
            {
       	        //closing session
    	        SEclose(&conn);
    	        return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
    		while(bytes_read > 0)
    		{
	    		buf[bytes_read] = '\0';
    			fprintf(res_os, "%s", buf);
                fflush(res_os);
    			bytes_read = SEgetData(&conn, buf, RESULT_MSG_SIZE);
                if (bytes_read == SEDNA_ERROR)
                {
       	           //closing session
    	           SEclose(&conn);
    	           return EXIT_STATEMENT_OR_COMMAND_FAILED;
                }
    		}
//            term_debug_info_output(); // output debug info if there was any

    		res = SEnext(&conn);
            if((res == SEDNA_NEXT_ITEM_FAILED) || (res == SEDNA_ERROR))
            {
                fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
                fflush(stderr);
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
    	}
		fprintf(res_os, "\n");
        fflush(res_os);
    }
    if(res == SEDNA_UPDATE_SUCCEEDED) 
    {
//        term_debug_info_output(); // output debug info if there was any

    	fprintf(res_os, "UPDATE is executed successfully\n");
        fflush(res_os);
    }
	if(res == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
//        term_debug_info_output(); // output debug info if there was any

    	fprintf(res_os, "Bulk load succeeded\n");
        fflush(res_os);
    }
    
    if(show_time)
    {
    	fprintf(stderr, "total time: %s\n secs",SEshowTime(&conn));
        fflush(res_os);
    }
	

    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
	   fprintf(stderr, "session was closed with errors \n%s\n", SEgetLastErrorMsg(&conn));
       fflush(stderr);
	   return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }

return EXIT_SUCCESS;
}

