/*
 * File:  06_value_index.c
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application opens a session to "testdb" database, 
 * creates value index named "page-by-category" on collection "wikidb". 
 * Then queries data using index "page-by-category".
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

    // this query builds small HTML document 
    // using data from collection 'wikidb', 
    // To get pages that belong to a certain category (Category:Anarchism) 
    // we use value index 'page-by-category'
    const char* query = "let $c:=\"Category:Anarchism\"\n"
                        "return\n"
                        " \n"
                        "<html>\n"
                        "<h1>{$c}</h1>\n"
                        "<br/>\n"
                        "<i>Parent categories:</i>\n"
                        "<ul>\n"
                        "{for $p in doc(\"categories\")/categories/category[@id=$c]/parent\n"
                        "return <li>{$p//@id/string()}</li>}\n"
                        "</ul>\n"
                        " \n"
                        "<i>Articles in this category:</i>\n"
                        "<ul>\n"
                        "{for $a in index-scan(\"page-by-category\", $c, \"EQ\")\n"
                        "return <li>{$a/title/text()}</li>}\n"
                        "</ul>\n"
                        "</html>";

    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // Create index on 'page' by its category 'catlink/@href'. Index covers the  whole collection 'wikidb'
    res = SEexecute(&conn, "CREATE INDEX \"page-by-category\" ON collection(\"wikidb\")/page BY .//catlink/@href AS xs:string"); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Create index failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }

    // execute query that uses newly create index 
    res = SEexecute(&conn, query); 
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

