/**
 * File: Client.java
 * Copyright (C) 2009 The Institute for System Programming of the 
 * Russian Academy of Sciences (ISP RAS)
 *
 * This is an example of a client application that uses the Java API 
 * to work with Sedna.
 *
 * This application connects to the Sedna DBMS, opens a session. 
 * The session consists of one transaction. Application loads data from the 
 * file 'region.xml' and executes three statements: two XQuery queries and 
 * one update statement. When statements are executed, application commits 
 * the transaction and closes the session.
 */

import ru.ispras.sedna.driver.*;

class Client {

    /* Some database connection arguments. */
    final static String  url      = "localhost";
    final static String  dbname   = "testdb";
    final static String  user     = "SYSTEM";
    final static String  password = "MANAGER";

    public static void main(String args[]) {

        SednaConnection con = null;
        System.out.println("Client has been started ...");

        try {
            /* Get a connection */
            con = DatabaseManager.getConnection(url, dbname, user, password);

            /* Begin a new transaction */
            con.begin();

            /* Create statement */
            SednaStatement st = con.createStatement();

            /* Load XML into the database */
            System.out.println("Loading data ...");
            boolean call_res = st.execute("LOAD 'C:/region.xml' 'region'");

            /* If call_res is false the statement was an update */
            if (!call_res) {
                System.out.println("Document 'region.xml' has been loaded successfully");
                System.out.println("==================================================\n");
            }

            /* Execute query */
            System.out.println("Executing query");
            call_res = st.execute("doc('region')/*/*");

            /* If call_res is true the statement was not an update
             * and we can use SednaSerializedResult object. */
            if (call_res) printQueryResults(st);

            /* Execute update. */
            System.out.println("Executing update");
            call_res = st.execute("UPDATE delete doc('region')//africa");

            /* If call_res is false the statement was an update */
            if (!call_res) {
                System.out.println("Update succeeded");
                System.out.println("==================================================\n");
            }

            /* Execute query */
            System.out.println("Executing query");
            call_res = st.execute("doc('region')/*/*");

            if (call_res) printQueryResults(st);

            /* Remove document */
            System.out.println("Removing document ...");
            call_res = st.execute("DROP DOCUMENT 'region'");

            if (!call_res) {
                System.out.println("Document 'region' has been dropped successfully");
                System.out.println("==================================================\n");
            }
            /* Commit current transaction */
            con.commit();
        }
        catch(DriverException e) {
            e.printStackTrace();
        }
        finally {
            /* Properly close connection */
            try {
                if(con != null) con.close();
            }
            catch(DriverException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Pretty printing for query results
     */
    private static void printQueryResults(SednaStatement st)
            throws DriverException {

        int count = 1;
        String item;
        System.out.println("Result:");
        SednaSerializedResult pr = st.getSerializedResult();
        while ((item = pr.next()) != null) {
            System.out.println(count + " item: " + item);
            System.out.println("==================================================");
            count++;
        }
    }
}
