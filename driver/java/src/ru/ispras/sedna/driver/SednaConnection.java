/*
 * File:  SednaConnection.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

package ru.ispras.sedna.driver;

/**
 * Provides interface for managing transactions and connections. It is not guranteed to be thread safe. 
 * @see ru.ispras.sedna.driver.DatabaseManager
 */
public interface SednaConnection {

    /**
     * Begins a new transaction. For example:
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.begin();
     * </pre>
     *
     * @throws DriverException if the transaction has not begun successfully.
     * Exception contains details about the problem occured.
     */
    public void begin() throws DriverException;

    /**
     * Closes Sedna connection.
     * @throws DriverException if Sedna server has not managed to close connection properly.
     * Exception contains details about the problem occured.
     */
    public void close() throws DriverException;

    /**
     * Commits transaction if transaction is running. Otherwise throws exception. For example:
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.begin();
     *     con.commit();
     * </pre>
     *
     * @throws DriverException if the transaction has not been commited successfully.
     * Exception contains details about the problem occured.
     */
    public void commit() throws DriverException;

    /**
     * Rollback transaction if transaction is running. Otherwise throws exception. For example:
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.begin();
     *     con.rollback();
     * </pre>
     *
     * @throws DriverException if the transaction has not been rollback successfully.
     * Exception contains details about the problem occured.
     */
    public void rollback() throws DriverException;

    /**
     * Creates {@link ru.ispras.sedna.driver.SednaStatement} object that allows to execute queries, updates
     * and to load XML documents into the Sedna database.
     * @throws DriverException if driver failed to create statement.
     * Exception contains details about the problem occured.
     * @return new {@link ru.ispras.sedna.driver.SednaStatement} instance
     */
    public SednaStatement createStatement() throws DriverException;

    /**
     * Sedna supports fn:trace function for debugging purpose (see Sedna Programmer's Guide for details).
     * By default trace output is included into XQuery query result.
     * You can turn trace output on/off using this method
     * @param doTrace set to true if you want to get trace output (default case). Set to false otherwise.
     * @throws DriverException if failed to set trace output.
     * Exception contains details about the problem occured.
     */
    public void setTraceOutput(boolean doTrace) throws DriverException;

    /**
     * Setting connection into debug mode allows getting debug information when XQuery query
     * fails due to some reason (see Sedna Programmer's Guide for details for details).
     * When the query fails debug information is accessible through {@link ru.ispras.sedna.driver.DriverException#getDebugInfo()}.
     * To set the connection into debug mode use this method. For example:
     *
     * <pre>
     * try {
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.setDebugMode(true);
     *     con.begin();
     *     SednaStatement st1 = con.createStatement();
     *     boolean call_res = st1.execute("doc(\"region\")/regions/*");
     *     con.commit();
     *     con.close();
     *
     *  } catch (DriverException e) {
     *          System.out.println(e.toString());
     *          System.out.println(e.getDebugInfo());
     *  }
     * </pre>
     *
     * @param debug set to true if you want to set connection into debug mode,
     * otherwise set to false (default case).
     * @throws DriverException If some problems occured while trying to set session mode
     */
    public void setDebugMode(boolean debug) throws DriverException;


    /**
     * Retrieves whether this connection is already closed or not.
     * A connection is closed if the method {@link SednaConnection#close()}
     * was called on it or if fatal errors have occurred.
     * @return <code>true</code> is connection has been closed or there has been some fatal error on the connection;
     * <code>false</code> is connection is ok.
     */
    public boolean isClose();
}
