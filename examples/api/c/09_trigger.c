/*
 * File:  09_trigger.c
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * 
 * This is an example application that works with Sedna XML DBMS through C-API using libsedna library.
 * The application creates triggers:
 * (1) Trigger "trigger-on-insert-link" watches if the inserted links are valid. 
 *     On insertion of 'link' node into page 'summary', check if there is a page that link refers. 
 *     If there is no such a page, trigger changes 'link' node that is being inserted onto 'i' node with the same content.
 *
 * (2) Trigger "trigger-on-delete-category" watches that category list in document 'categories' is valid.
 *     On deletion of category from the 'categories' document, the trigger deletes all references to this category in pages.
 *
 * Then example demostrates how triggers work when you update data.
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
    
    const char* create_trigger_on_insert_link = "CREATE TRIGGER \"trigger-on-insert-link\"\n"
                                                "BEFORE INSERT\n"
                                                "ON collection(\"wikidb\")/page//summary/link\n"
                                                "FOR EACH NODE\n"
                                                "DO\n"
                                                "{\n"
                                                " let $p:=collection(\"wikidb\")/page/title\n"
                                                " return\n"
                                                "   if($NEW/@label != $p)\n"
                                                "   then <i>{$NEW/text()}</i>\n"
                                                "   else $NEW;\n"
                                                "}";

    const char* create_trigger_on_delete_category = 
                                               "CREATE TRIGGER \"trigger-on-delete-category\"\n"
                                               "AFTER DELETE\n"
                                               "ON doc(\"categories\")/categories/category\n"
                                               "FOR EACH NODE\n"
                                               "DO\n"
                                               "{\n"
                                               "  UPDATE delete collection(\"wikidb\")//catlink[@href=$OLD/@id];\n"
                                               "  $OLD;\n"
                                               "}";

    printf("09_trigger started.\n");
    
    //connecting to database "testdb" with login "SYSTEM", password "MANAGER"
    res = SEconnect(&conn, url, db_name, login, password);
    if(res != SEDNA_SESSION_OPEN) 
    {
        printf("Session starting failed: \n%s\n", SEgetLastErrorMsg(&conn));
		return -1;
    }

    // create trigger on insert
    res = SEexecute(&conn, create_trigger_on_insert_link); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Create trigger failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }
 
    // see how trigger works: insert 'link' node that refers page Greece, that does not exist in wikidb collection
    res = SEexecute(&conn, "UPDATE insert <link label=\"Greece\">Greece</link> into collection(\"wikidb\")/page[title=\"Aristotle\"]//summary"); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }

    // retrieve summary of 'Aristotle' page to see the effect of trigger job: instead of inserted 'link' node there is 'i' node.
    res = SEexecute(&conn, "collection(\"wikidb\")/page[title=\"Aristotle\"]//summary"); 
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

    // create trigger on delete
    res = SEexecute(&conn, create_trigger_on_delete_category); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Create trigger failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }

    // see how trigger works: delete category named 'Category:Aristotle' - the trigger must delete all references to this categories from pages in "wikidb" collection
    res = SEexecute(&conn, "UPDATE delete doc(\"categories\")//category[@id=\"Category:Aristotle\"]"); 
    if(res != SEDNA_UPDATE_SUCCEEDED) 
    {
        printf("Update failed: \n%s\n", SEgetLastErrorMsg(&conn));
        //closing session
        SEclose(&conn);
        return -1;
    }

    // retrieve categories of 'Aristotle' page to see the effect of trigger job: 
    // page does not contain "Category:Aristotle" reference.
    res = SEexecute(&conn, "collection(\"wikidb\")/page[title=\"Aristotle\"]//catlinks"); 
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

