/*
 * File:  03_load_coll.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, loads all the documents from the file system
 * directory into a collection.
 */

#include "libsedna.h"
#include <stdio.h>

int main()
{
    int res, i;

    struct SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;

    const char* url = "localhost";
    const char* db_name = "testdb";
    const char* login = "SYSTEM";
    const char* password = "MANAGER";
    
    char docname[8];
    char query[64];

    printf("03_load_coll started.\n");
    
    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // create collection wikidb
    res = SEexecute(&conn, "CREATE COLLECTION \"wikidb\""); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Create collection failed: \n%s\n", SEgetLastErrorMsg(&conn));
        // closing session
        SEclose(&conn);
        return -1;
    }

    printf("Collection 'wikidb' has been created successfully.\n");

    // We have a set of documents with similar structure -
    // load them into a collection to save space!
    //
    // In real application you can load unlimited number of documents
    // into a single collection (we tried 500 000+).
    for(i=1; i<11; i++)
    {
       	strcpy(docname, "page");
        (i < 10) ? sprintf(docname+4, "0%d", i) : sprintf(docname+4, "%d", i);

        strcpy(query, "LOAD \"../data/");
        strcat(query, docname);
        strcat(query, ".xml\" \"");
        strcat(query, docname);
        strcat(query, "\" \"wikidb\"");

        res = SEexecute(&conn, query); 

        if(res != SEDNA_BULK_LOAD_SUCCEEDED)
        {
            printf("Bulk load failed: \n%s\n", SEgetLastErrorMsg(&conn));
            // closing session
            SEclose(&conn);
            return -1;
        }
        printf("Document %s has been loaded into collection.\n", docname);
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
