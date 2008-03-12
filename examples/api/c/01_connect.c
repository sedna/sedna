/*
 * File:  01_connect.c
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database and closes the session.
 */

#include "libsedna.h"

int main()
{
	int res;
    
    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;

    const char* url = "localhost";
    const char* db_name = "testdb";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";

    printf("01_connect started.\n");

    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }
    printf("Session opened successfully.\n");

    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
        printf("A error occured while trying to close session: \n%s\n", SEgetLastErrorMsg(&conn));
        return -1;
    }

    printf("Session closed successfully.\n");

    return 0;
} 
