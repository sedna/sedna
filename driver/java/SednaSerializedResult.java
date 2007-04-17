
/*
 * File:  SednaSerializedResult.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

//~--- interfaces -------------------------------------------------------------
/**
 * <code>SednaSerializedResult</code> interface represents the result of 
 * the XQuery statement evaluation, it is used to get the result items from server. 
 * Every item is represented as a string.
 *
 * Application can obtain the <code>SednaSerializedResult</code> object
 * using the <code>getSerializedResult</code> 
 * method of the <code>SednaStatement</code> interface.
 */
public interface SednaSerializedResult {

/**
 * Used to iterate over the XQuery statement result sequence.
 * @return next item of the result of the XQuery statement or null if the sequence has ended.
 * @throws <code>DriverException</code> if some error occured while tring to get result item from server.
 */
     public String next() throws DriverException;

/**
 * Used to iterate over the XQuery statement result sequence. 
 * Next item of the XQuery statement result will be written into the <code>writer</code>.
 * @param writer a writer to write an result item to. 
 * @return 0 if an item was retrieved and written successful, and 1 if the result sequence has ended.
 * @throws <code>DriverException</code> if some error occured while tring to get result item from server.
 */
    public int next(Writer writer) throws DriverException;
}
