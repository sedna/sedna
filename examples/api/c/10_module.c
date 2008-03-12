/*
 * File:  10_module.c
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, loads module "math" from file stored locally, 
 * and executes a query that uses module.
 */

#include "libsedna.h"
#include <stdio.h>

int main()
{
    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
    int bytes_read, res, value;
    char buf[1024];

    const char* url = "localhost";
    const char* db_name = "testdb";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";

    printf("10_module started.\n");

    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // load module from file "math.xqlib"
    res = SEexecute(&conn, "LOAD MODULE \"math.xqlib\""); 
    if(res != SEDNA_BULK_LOAD_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Module has been loaded successfully.\n");

    // execute query that uses module
    res = SEexecute(&conn, "import module namespace math = \"http://example.org/math\"; math:increment(math:square($math:pi))"); 
    if(res != SEDNA_QUERY_SUCCEEDED) 
    {
        printf("Query failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }

    // iterate over the result sequence and retrieve the result data
    while((res = SEnext(&conn)) != SEDNA_RESULT_END)
    {
        if (res == SEDNA_ERROR)
        {
            printf("Failed to get next result item from server: \n%s\n", SEgetLastErrorMsg(&conn));
            //closing session
            SEclose(&conn);
            return -1;
        }
        do
        {
            bytes_read = SEgetData(&conn, buf, 1024 - 1);
            if(bytes_read == SEDNA_ERROR)
            {
                printf("Failed to get result data from server: \n%s\n", SEgetLastErrorMsg(&conn));
                //closing session
                SEclose(&conn);
                return -1;
            }
           	buf[bytes_read] = '\0';
         	printf("%s", buf);

        }while(bytes_read > 0);
    	printf("\n");
    }

    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
        printf("An error occured while trying to close session: \n%s\n", SEgetLastErrorMsg(&conn));
        return -1;
    }
    
    return 0;
} 

