
/*
 * File:  SednaConnection.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

/**
 * <code>SednaConnection</code> interface provides methods 
 * for managing transactions, setting session options and closing the session. 
 * SednaConnection is not necessarily safe for multithreaded access. 
 *
 * @see DatabaseManager
 */
public interface SednaConnection {

/**
 * Begins a new transaction. 
 * If the transaction has not begun successfully the method throws <code>DriverException</code>
 * with details about the problem occured.
 */
    public void begin() throws DriverException;

/**
 * Closes Sedna connection. 
 * If Sedna server has not managed to close connection properly 
 * this method throws <code>DriverException</code>.
 */
    public void close() throws DriverException;

/**
 * Commits transaction if transaction is running. Otherwise throws exception.
 */
    public void commit() throws DriverException;

/**
 * Rollback transaction if transaction is running. Otherwise throws exception.
 */
    public void rollback() throws DriverException;

/**
 * creates <code>SednaStatement</code> object 
 * that allows to execute queries and updates 
 * and to load XML documents into the Sedna database.
 */
    public SednaStatement createStatement() throws DriverException;

/**
 * Sedna supports fn:trace function for debugging purpose (see Sedna Programmers Guide for details).
 * By default trace output is included into XQuery query result. 
 * You can turn trace output on/off using this method 
 *
 * @param doTrace set to true if you want to get trace output (default case). Set to false if you do not want.
 */    
    public void setTraceOutput(boolean doTrace) throws DriverException;

/**
 * Setting connection into debug mode allows getting debug information when XQuery query 
 * fails due to some reason (see Sedna Programmers Guide for details for details). 
 * To set the connection into debug mode use this method.
 *
 * @param debug set to true if you want to set connection into debug mode,
 * otherwise set to false (default case).
 */    
    public void setDebugMode(boolean debug) throws DriverException;

    //~--- get methods --------------------------------------------------------

/**
 * This method retrieves whether this connection has been closed or not. 
 * A connection is closed if the method <code>close</code> has been called on 
 * it or if certain fatal errors have occurred.
 */
    public boolean isClose();
}
