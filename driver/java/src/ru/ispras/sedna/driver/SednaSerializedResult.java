/*
 * File:  SednaSerializedResult.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;

/**
 * Represents the result of the XQuery statement evaluation, it is used to get
 * the result items from the server. Every item is represented as a string.
 *
 * Application can obtain the <code>SednaSerializedResult</code> object
 * using the {@link SednaStatement#getSerializedResult()} method.
 *
 * <p>
 * <b>Note</b>: Application working with Sedna through this Java API has to execute statements 
 * and use the results of their execution <b>sequentially</b>. For example the next is incorrect:
 * <p>
 *
 * <pre>
 *    SednaStatement st1 = con.createStatement();
 *    SednaStatement st2 = con.createStatement();
 *
 *    boolean call_res = st1.execute("doc('region')/regions/*");
 *    call_res = st2.execute("doc('b')/regions/a");
 *
 *    if (call_res) {
 *        SednaSerializedResult pr1 = st1.getSerializedResult();
 *    }
 * </pre>
 *
 * @see ru.ispras.sedna.driver.SednaStatement#getSerializedResult()
 */
public interface SednaSerializedResult {

    /**
     * Used to iterate over the XQuery statement result sequence.
     * @return next item of the result of the XQuery statement or null if the sequence has ended.
     * @throws DriverException if some error occurred while trying to get result item from server.
     * @see ru.ispras.sedna.driver.SednaStatement#getSerializedResult()
     */
    public String next() throws DriverException;

    /**
     * Used to iterate over the XQuery statement result sequence.
     * Next item of the XQuery statement result will be written into the <code>writer</code>.
     * @param writer a <code>java.io.Writer</code> object to write an result item to. 
     * @return 0 if an item was retrieved and written successful, and 1 if the result sequence has ended.
     * @throws DriverException if some error occurred while trying to get result item from server.
     * @see ru.ispras.sedna.driver.SednaStatement#getSerializedResult()
     */
    public int next(Writer writer) throws DriverException;
}
