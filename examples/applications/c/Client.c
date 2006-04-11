/*
 * File:  Client.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "sample-db" database, begins a transaction, loads document "region.xml"
 * to "sample-db" database, executes XQuery query, retrieves every item of the result data, commits the 
 * transaction and closes the session.
 */

#include "libsedna.h"
#include <stdio.h>

int main()
{
    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
    int bytes_read, res;
    char buf[1024];

    const char* url = "localhost";
    const char* db_name = "sample-db";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";

    printf("Client started.\n");
    
    //connecting to database "sample-db" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
	
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }
	
    //begin transaction
    res = SEbegin(&conn);
    if(res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
    {
        printf("Begin transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
   
    // load data from file "region.xml" into the document "regions"
    res = SEexecute(&conn, "LOAD \"region.xml\" \"region\""); 
    if(res != SEDNA_BULK_LOAD_SUCCEEDED) 
    {
        printf("Bulk load failed \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
	
    // execute XQuery query	
    res = SEexecute(&conn, "document(\"region\")/*/*"); 
    if(res != SEDNA_QUERY_SUCCEEDED) 
    {
        printf("Query failed \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
 
    //iterate over the result sequence and retrieve the result data
    res = SEnext(&conn);
    
    while((res != SEDNA_RESULT_END)&&(res != SEDNA_ERROR))
    {
     printf("result item:\n");
        do
        {
            bytes_read = SEgetData(&conn, buf, 1024 - 1);
            if((bytes_read == SEDNA_ERROR) || (bytes_read == SEDNA_GET_DATA_FAILED))
            {
                printf("Failed to get result from server\n%s\n", SEgetLastErrorMsg(&conn));
                //closing session
                SEclose(&conn);
                return -1;
            }
           	buf[bytes_read] = '\0';
         	printf("%s", buf);

        }while(bytes_read > 0);
    	printf("\n");
        res = SEnext(&conn);
    }

    if(res == SEDNA_RESULT_END) 
    {
        printf("Result end \n");
    }

    //commiting the transaction
    res = SEcommit(&conn);
    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
    {
        printf("Commit transaction failed \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
	
    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
        printf("Session was closed with errors \n%s\n", SEgetLastErrorMsg(&conn));
        return -1;
    }
    return 0;
} 	
