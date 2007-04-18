
/*
 * File:  SednaConnection.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

/**
 * <code>SednaConnection</code> interface provides methods 
 * for managing transactions, setting Sedna connection options and closing the connection. 
 * SednaConnection is not necessarily safe for multithreaded access. 
 *
 * @see DatabaseManager
 */
public interface SednaConnection {

/**
 * Begins a new transaction. 
 * <p> 
 * Simple example:
 * <br>
 * <code>
 *  SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *  con.begin();
 * </code>
 * <p> 
 * @throws DriverException If the transaction has not begun successfully. <code>DriverException</code> contains
 * details about the problem occured.
 */
    public void begin() throws DriverException;

/**
 * Closes Sedna connection. 
 * @throws DriverException If Sedna server has not managed to close connection properly.
 */
    public void close() throws DriverException;

/**
 * Commits transaction if transaction is running. Otherwise throws exception.
 * <p> 
 * Simple example:
 * <p>
 * <code>
 *  SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *  con.begin();
 * <br> 
 *  con.commit();
 * </code>
 *
 * @throws DriverException If the transaction has not been commited successfully. <code>DriverException</code> contains
 * details about the problem occured.
 */
    public void commit() throws DriverException;

/**
 * Rollback transaction if transaction is running. Otherwise throws exception.
 * <p> 
 * Simple example:
 * <p> 
 *
 * <code>
 *  SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *  con.begin();
 * <br> 
 *  con.rollback();
 * </code>
 *
 * @throws DriverException If the transaction has not been rollback successfully. <code>DriverException</code> contains
 * details about the problem occured.
 */
    public void rollback() throws DriverException;

/**
 * Creates <code>SednaStatement</code> object 
 * that allows to execute queries and updates 
 * and to load XML documents into the Sedna database.
 */
    public SednaStatement createStatement() throws DriverException;

/**
 * Sedna supports fn:trace function for debugging purpose (see Sedna Programmers Guide for details).
 * By default trace output is included into XQuery query result. 
 * You can turn trace output on/off using this method 
 *
 * @param doTrace set to true if you want to get trace output (default case). Set to false otherwise.
 */    
    public void setTraceOutput(boolean doTrace) throws DriverException;

/**
 * Setting connection into debug mode allows getting debug information when XQuery query 
 * fails due to some reason (see Sedna Programmers Guide for details for details). 
 * When the query fails debug information is accessible through 
 * <code>getDebugInfo</code> method of the <code>DriverException</code> object.
 * To set the connection into debug mode use this method.
 * <p> 
 *
 * For example:
 * <p> 
 *
 * <code>
 * try{
 * <br> 
 *   SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 * <br> 
 *   con.setDebugMode(true);
 * <br> 
 * <br> 
 *   con.begin();
 * <br> 
 *   SednaStatement st1 = con.createStatement();
 * <br> 
 * <br> 
 *   boolean call_res = st1.execute("doc(\"region\")/regions/*");
 * <br> 
 *   con.commit();
 * <br> 
 *   con.close();
 * <br> 
 * <br> 
 *  } catch (DriverException e) {
 * <br> 
 *          System.out.println(e.toString());
 * <br> 
 *          System.out.println(e.getDebugInfo());
 * <br> 
 *  }
 * </code>
 *
 *
 *
 * @param debug set to true if you want to set connection into debug mode,
 * otherwise set to false (default case).
 * @throws DriverException If some problems occured while trying to set session mode
 */    
    public void setDebugMode(boolean debug) throws DriverException;

    //~--- get methods --------------------------------------------------------

/**
 * Retrieves whether this connection has been closed or not. 
 * A connection is closed if the method <code>close</code> has been called on 
 * it or if fatal errors have occurred.
 * @return <code>true</code> is connection has been closed or there has been some fatal error on the connection; <code>false</code> is connection is ok. 
 */
    public boolean isClose();
}
