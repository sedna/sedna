/*
 * File:  02_load.c
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, loads document "categories.xml" as a standalone
 * document.
 */

#include "libsedna.h"
#include <stdio.h>

int main()
{
    int res;

    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;

    const char* url = "localhost";
    const char* db_name = "testdb";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";

    printf("02_load started.\n");
    
    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // load data from file "categories.xml" into the document "categories"
    res = SEexecute(&conn, "LOAD \"../data/categories.xml\" \"categories\""); 
    if(res != SEDNA_BULK_LOAD_SUCCEEDED) 
    {
        printf("Bulk load failed: \n%s\n", SEgetLastErrorMsg(&conn));
        // closing session
        SEclose(&conn);
        return -1;
    }

    printf("Document 'categories.xml' has been successfully loaded into 'testdb' database.\n");

    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
        printf("An error occured while trying to close session: \n%s\n", SEgetLastErrorMsg(&conn));
        return -1;
    }
    
    return 0;
} 
