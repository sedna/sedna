/*
 * File:  05_trans.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, sets session into 'manual-commit' mode, 
 * begins and commits transaction.
 */

#include "libsedna.h"
#include <stdio.h>

int main()
{
    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
    int res, value;

    const char* url = "localhost";
    const char* db_name = "testdb";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";

    printf("05_trans started.\n");
    
    value = SEDNA_AUTOCOMMIT_OFF;
    SEsetConnectionAttr(&conn, SEDNA_ATTR_AUTOCOMMIT, (void*)&value, sizeof(int));
    
    printf("Session is set into manual commit mode.\n");

    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    //begin transaction
    res = SEbegin(&conn);
    if(res != SEDNA_BEGIN_TRANSACTION_SUCCEEDED) 
    {
        printf("Begin transaction failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Transaction is started.\n");

    //commiting transaction
    res = SEcommit(&conn);
    if(res != SEDNA_COMMIT_TRANSACTION_SUCCEEDED) 
    {
        printf("Commit transaction failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
    printf("Transaction is committed.\n");
	
    //closing session
    res = SEclose(&conn);
    if(res != SEDNA_SESSION_CLOSED) 
    {
        printf("An error occured while trying to close session: \n%s\n", SEgetLastErrorMsg(&conn));
        return -1;
    }
    
    return 0;
} 

