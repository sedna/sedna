#include "libsedna.h"

#include "base.h"
#include "uutils.h"
#include "d_printf.h"


#include "term_funcs.h"
#include "term_globals.h"
#include "mainloop.h"

void term_output1(const char *buf)
{
    if (echo) printf("%s", buf);
}

void term_output2(const char *buf, const void* arg1)
{
    if (echo) 
    {
    	char echo_buf[256];
    	sprintf(echo_buf,buf,arg1);
    	printf("%s", echo_buf);
    }
}

void term_output3(const char *buf, const void* arg1, const void* arg2)
{
    if (echo) 
    {
    	char echo_buf[256];
    	sprintf(echo_buf,buf,arg1,arg2);
    	printf("%s", echo_buf);
    }
}

int process_commandline_query()
{
    SednaConnection conn;
    
    char buf[1024];
//    char* hosttemp;
//    strcpy(hosttemp, string(host).c_str());
    strcat(strcat(host,":"),std::string(itoa(socket_port, buf, 10)).c_str());
//    d_printf2("host: %s\n", host);
    
    int res = SEconnect(&conn, host, db_name, login, password);
	
    if(res != SEDNA_SESSION_OPEN){
    	printf("failed to open session \n%s\n", SEgetLastErrorMsg(&conn));
    	return -1;
    }
	
    //begin transaction
    res = SEbegin(&conn);
    if(res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
    {
    	term_output2("failed to begin transaction\n%s\n", SEgetLastErrorMsg(&conn));
    	return -1;
    }
    
    // execute XQuery query	or update
    res = SEexecute(&conn, query); 
    if(res == SEDNA_QUERY_FAILED) 
    {
    	printf("Query failed \n%s\n%s", SEgetLastErrorMsg(&conn), query);
    	//closing session
    	SEclose(&conn);
    	return -1;
    }
    if(res == SEDNA_UPDATE_FAILED) 
    {
    	printf("Update failed \n%s\n", SEgetLastErrorMsg(&conn));
    	//closing session
    	SEclose(&conn);
    	return -1;
    }
    if(res == SEDNA_BULK_LOAD_FAILED) 
    {
    	printf("Bulk load failed \n%s\n", SEgetLastErrorMsg(&conn));
    	//closing session
    	SEclose(&conn);
    	return -1;
    }
    if(res == SEDNA_ERROR) 
    {
    	printf("Error \n%s\n%s", SEgetLastErrorMsg(&conn),query);
    	//closing session
    	SEclose(&conn);
    	return -1;
    }
    if(res == SEDNA_QUERY_SUCCEEDED) 
    {
    	//iterate over the result sequece and retrieve the result data
    	int bytes_read;
    	res = SEnext(&conn);
    	
    	while((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR))
    	{
    		bytes_read = SEgetData(&conn, buf, 1024);
            if (bytes_read == SEDNA_ERROR)
            {
   	            printf("%s\n", SEgetLastErrorMsg(&conn));
       	        //closing session
    	        SEclose(&conn);
    	        return -1;
            }
    		buf[bytes_read] = '\0';
    		printf("%s\n", buf);
    		res = SEnext(&conn);
    	}
    }
    if(res == SEDNA_UPDATE_SUCCEEDED) 
    {
    	term_output1("Update succeeded\n");
    }
	if(res == SEDNA_BULK_LOAD_SUCCEEDED) 
    {
    	term_output1("Bulkload succeeded\n");
    }
    
    if(show_time != 0)
    {
    	term_output2("Time: %s\n",SEshowTime(&conn));
    }
	

    //commiting the transaction
    res = SEcommit(&conn);
    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
    {
	term_output2("Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
	return -1;
    }
	
    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
	term_output2("Session was closed with errors \n%s\n", SEgetLastErrorMsg(&conn));
	return -1;
    }

return 0;
}

int process_file_commands()
{
	int ret_code = MainLoop(fopen(filename, "r"));
    return ret_code;
}


