
#include "common/sedna.h"

#include "libsedna.h"
#include "common/base.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "term_funcs.h"
#include "term_globals.h"
#include "mainloop.h"

void term_output1(const char *buf)
{
    if (echo) fprintf(res_os, "%s", buf);
}

void term_output2(const char *buf, const void* arg1)
{
    if (echo) 
    {
    	char echo_buf[256];
    	sprintf(echo_buf,buf,arg1);
    	fprintf(res_os, "%s", echo_buf);
    }
}

void term_output3(const char *buf, const void* arg1, const void* arg2)
{
    if (echo) 
    {
    	char echo_buf[256];
    	sprintf(echo_buf,buf,arg1,arg2);
    	fprintf(res_os, "%s", echo_buf);
    }
}

void term_debug_info_output(const char *msg)
{
    if (debug_output)
    {
        int i = 0;
        int msg_length = strlen(msg);
        if (strncmp(msg, "\nSEDNA TRACE", 12) == 0)
        {
            i = 13;
            while((i<523) && (i<msg_length) && (msg[i]!='\n'))
            {
                i++;
            }
            memcpy(debug_indent, msg+13, i);
            debug_indent[i-13]='\0';
			i++;
        }

        fprintf(res_os, "\n%s", debug_indent);

        while(i<msg_length)
        {
            fprintf(res_os, "%c", msg[i]);
            if (msg[i]=='\n') fprintf(res_os, "%s", debug_indent);
            i++;
        }
    }
}

int process_commandline_query()
{
    SednaConnection conn;
    char buf[RESULT_MSG_SIZE+1];
    
   	term_output2("%s> ",db_name);
    strcat(strcat(host,":"),std::string(u_itoa(socket_port, buf, 10)).c_str());
    
    int res = SEconnect(&conn, host, db_name, login, password);
	
    if(res != SEDNA_SESSION_OPEN){
    	fprintf(stderr, "failed to open session \n%s\n", SEgetLastErrorMsg(&conn));
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
	
    //begin transaction
    res = SEbegin(&conn);
    if(res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
    {
    	fprintf(stderr, "failed to begin transaction\n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    
    // execute XQuery query	or update
    res = SEexecute(&conn, query); 
    if(res == SEDNA_QUERY_FAILED) 
    {
    	fprintf(stderr, "\n%s\n%s", SEgetLastErrorMsg(&conn));
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_UPDATE_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_BULK_LOAD_FAILED) 
    {
    	fprintf(stderr, "\n%s\n", SEgetLastErrorMsg(&conn));
    	//closing session
    	SEclose(&conn);
    	return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }
    if(res == SEDNA_ERROR) 
    {
    	fprintf(stderr, "\n%s\n%s", SEgetLastErrorMsg(&conn),query);
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
                return EXIT_STATEMENT_OR_COMMAND_FAILED;
            }
    	}
		fprintf(res_os, "\n");
    }
    if(res == SEDNA_UPDATE_SUCCEEDED) 
    {
//        term_debug_info_output(); // output debug info if there was any

    	fprintf(res_os, "UPDATE is executed successfully\n");
    }
	if(res == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
//        term_debug_info_output(); // output debug info if there was any

    	fprintf(res_os, "Bulk load succeeded\n");
    }
    
    if(show_time != 0)
    {
    	fprintf(stderr, "total time: %s\n",SEshowTime(&conn));
    }
	

    if(!conn.autocommit)
    {
        //commiting the transaction
        res = SEcommit(&conn);
        if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
        {
            fprintf(stderr, "failed to commit transaction \n%s\n", SEgetLastErrorMsg(&conn));
            //closing session
            SEclose(&conn);
            return EXIT_STATEMENT_OR_COMMAND_FAILED;
        }
    }
	
    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
	   fprintf(stderr, "session was closed with errors \n%s\n", SEgetLastErrorMsg(&conn));
	   return EXIT_STATEMENT_OR_COMMAND_FAILED;
    }

return EXIT_SUCCESS;
}

