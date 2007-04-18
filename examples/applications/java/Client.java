
/*
 * File:  Client.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/**
 *
 * This is an example of a client application that uses the Java API
 * to work with Sedna.
 * This application connects to the Sedna DBMS, opens a session with parameters
 * specified. The session consists of one transaction. In the
 * transaction application loads data from file 'region.xml' and executes
 * three statements: two XQuery queries and one update statement. When statements
 * are executed, application commits the transaction and closes the session.
 */

import ru.ispras.sedna.driver.*;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

import java.net.*;

import java.util.*;

//~--- classes ----------------------------------------------------------------

class Client {
    public Client() {}

    //~--- methods ------------------------------------------------------------

    public static void main(String args[]) {
        System.out.println("Client started\n");

        // set up arguments for the call to the getConnection method.
        String          url      = "localhost";
        String          dbname   = "sample-db";
        String          user     = "SYSTEM";
        String          password = "MANAGER";
        String          item;
        int             count;
        SednaConnection con = null;

        // get a connection
        try {
            con = DatabaseManager.getConnection(url, dbname, user, password);

            // begin transaction
            con.begin();

            // creates statements
            SednaStatement st1 = con.createStatement();
            SednaStatement st2 = con.createStatement();

            // execute a statement. the statement is a bluk load.
            System.out.println("Loading data");

            boolean call_res = st1.execute("LOAD \"region.xml\" \"region\"");

            if (!call_res)    // if call_res is false the statement was an update
            {
                System.out.println("Bulk load succeeded");
                System.out.println("=====================================\n");
            }

            // execute query.
            System.out.println("Executing query");
            call_res = st1.execute("document(\"region\")/*/*");

            if (call_res)    // if call_res is true the statement was not an update

            // and we can use SednaSerializedResult object
            {
                System.out.println("Result:");

                SednaSerializedResult pr1 = st1.getSerializedResult();

                item  = null;
                item  = pr1.next();
                count = 1;

                while (item != null) {
                    System.out.println(count + " item: " + item);
                    item = pr1.next();
                    System.out.println(
                        "=====================================\n");
                    count++;
                }
            }

            // execute update.
            System.out.println("Executing update");
            call_res =
                st2.execute("UPDATE delete document(\"region\")//africa");
            System.out.println("=====================================\n");

            // execute query.
            System.out.println("Executing query");
            call_res = st2.execute("document(\"region\")/*/*");

            if (call_res)    // if call_res is true the statement was not an update

            // and we can use PlaneResult object
            {
                System.out.println("Result:");

                SednaSerializedResult pr3 = st2.getSerializedResult();

                item  = null;
                item  = pr3.next();
                count = 1;

                while (item != null) {
                    System.out.println(count + " item: " + item);
                    item = pr3.next();
                    System.out.println(
                        "=====================================");
                    count++;
                }
            }

            // commit the transaction
            con.commit();

            // break the connection
            con.close();
        } catch (DriverException e) {
            System.out.println(e.toString());
        }
    }
}
