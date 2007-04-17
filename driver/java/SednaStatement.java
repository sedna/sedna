
/*
 * File:  SednaStatement.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

//~--- interfaces -------------------------------------------------------------
/**
 * <code>SednaStatement</code> interface provides methods for loading documents 
 * into the database, executing statements of the database language 
 * (see section 2 of the sedna Programmer's Guide to read about the Sedna Database Language)
 * and retrieving results that statements produce.
 *
 * <code>SednaStatement</code> object is created using the <code>createStatement</code> 
 * method of the <code>SednaConnection</code> interface.
 */
public interface SednaStatement {
	
/**
 * Reads XQuery statement from the input stream and executes it.
 *
 * @param in is some input stream to read an XQuery statement from
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 */
    public boolean execute(InputStream in) throws DriverException, IOException;

/**
 * Executes XQuery statement.
 *
 * @param queryText is an XQuery statement.
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 */
    public boolean execute(String queryText) throws DriverException;

/**
 * Executes XQuery statement.
 *
 * @param in is some input stream to read an XQuery statement from
 * @param resultType is one of the two possible result formats 
 * (one of the <code>ResultType.XML</code> or <code>ResultType.SXML</code>).
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 */
    public boolean execute(InputStream in, ResultType resultType)
            throws DriverException, IOException;

/**
 * Executes XQuery statement.
 *
 * @param queryText is an XQuery statement.
 * @param resultType is one of the two possible result formats 
 * (one of the <code>ResultType.XML</code> or <code>ResultType.SXML</code>).
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 */
    public boolean execute(String queryText, ResultType resultType)
            throws DriverException;

/**
 * Loads XML document into the Sedna database.
 * @param
 */
    public void loadDocument(InputStream in, String doc_name)
            throws DriverException, IOException;

    public void loadDocument(InputStream in, String doc_name, String col_name)
            throws DriverException, IOException;

    //~--- get methods --------------------------------------------------------

    public SednaSerializedResult getSerializedResult();
}
