/*
 * File:  08_update.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, updates data in the database:
 * (1) Deletes 'comment' nodes in collection 'wikidb';
 * (2) Replaces 'link' nodes that refer to the page that does not exist in collection 'wikidb' on to 'i' node;
 * (3) Inserts current date/time and contributor name 'sedna-user' in the beginning of page 'NASA'.
 *
 * Note: this example uses data from collection 'wikidb' and document 'categories'. To run this example 
 * you must first load data using examples: 02_load and 03_load_col
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
    
    const char* update_delete = "UPDATE delete collection('wikidb')//comment";

    const char* update_replace = "UPDATE\n"
                                 "replace $l in \n"
                                 " let $p:=collection(\"wikidb\")/page/title\n"
                                 " return\n"
                                 " collection(\"wikidb\")//link[@label != $p]\n"
                                 " with <i>{$l/text()}</i>";
    
    const char* update_insert = "UPDATE\n"
                                "insert (<timestamp>{current-dateTime()}</timestamp>,\n"
                                "<contributor>sedna-user</contributor>)\n"
                                "into collection(\"wikidb\")/page[title=\"NASA\"]/revision";

    printf("08_update started.\n");
    
    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // delete all 'comment' nodes in collection 'wikidb'
    res = SEexecute(&conn, update_delete); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Update-delete executed successfully.\n");

    // replace 'link' nodes that refer to the page that does not exist in collection 'wikidb' on to 'i' node
    res = SEexecute(&conn, update_replace); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Update-replace executed successfully.\n");
    
    // insert current date/time and contributor name 'sedna-user' in the beginning of page 'NASA'.
    res = SEexecute(&conn, update_insert); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Update-insert executed successfully.\n");
    
    // query to see result of insert
    res = SEexecute(&conn, "let $p:=collection(\"wikidb\")/page[title=\"NASA\"] return <contributors>{($p/revision/contributor, $p/revision/timestamp)}</contributors>"); 
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

