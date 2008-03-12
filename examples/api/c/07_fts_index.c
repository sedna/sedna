/*
 * File:  07_fts_index.c
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application creates full-text index, and executes a query using this index.
 * 
 * Note: to run this example you need to switch Full-Text Search support in Sedna on. 
 * See Sedna Programmer's Guide how to do it.
 * This example uses data from collection 'wikidb' and document 'categories'. To run this example 
 * you must first load data using examples: 02_load and 03_load_col.
 *
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

    printf("07_fts_index started.\n");
    
    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // create full-text index on 'page' nodes in collection 'wikidb'. Type "xml" means that the indexed nodes are considered as XML nodes.
    res = SEexecute(&conn, "CREATE FULL-TEXT INDEX \"pages\" ON collection(\"wikidb\")/page TYPE \"xml\""); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Create ft-index failed: \n%s\n", SEgetLastErrorMsg(&conn));
        // closing session
        SEclose(&conn);
        return -1;
    }

    // execute XQuery query	using full-text index: list all pages titles that contains word 'science' and does not contain word 'computer'.
    res = SEexecute(&conn, "ftindex-scan(\"pages\", \"science and not computer\")"); 
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

