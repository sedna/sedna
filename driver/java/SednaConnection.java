
/*
 * File:  SednaConnection.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

public interface SednaConnection {
    public void begin() throws DriverException;

    public void close() throws DriverException;

    public void commit() throws DriverException;

    public SednaStatement createStatement() throws DriverException;

    public void rollback() throws DriverException;

    //~--- get methods --------------------------------------------------------

    public boolean isClose();
}
