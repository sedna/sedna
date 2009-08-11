
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
 * @see SednaConnection#createStatement
 */
public interface SednaStatement {
	
/**
 * Reads XQuery statement from the input stream and executes it.
 * <p>
 * Simple example: XQuery statement 'CREATE COLLECTION 'col1'' is taken from file '1.xquery'
 * <p>
 * <code>
 * <p> 
 *       <code>SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *       con.begin();
 * <br> 
 *       SednaStatement st1 = con.createStatement();
 * <br> 
 *       FileInputStream f = new FileInputStream("1.xquery");                               
 * <br> 
 *       boolean call_res = st1.execute(f);
 * <br> 
 *       if(!call_res) System.out.println("Collection has been created");
 * <br> 
 *       con.commit();
 * <br> 
 *       con.close();
 * </code>
 * <p> 
 * @param in is <code>java.io.InputStream</code> object to read an XQuery statement from
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 * @see SednaSerializedResult
 */
    public boolean execute(InputStream in) throws DriverException, IOException;

/**
 * Executes XQuery statement.
 * <p> 
 * Simple example:
 * <p> 
 *
 * <code>
 *    SednaConnection con = DatabaseManager.getConnection("localhost", "db1", "SYSTEM", "MANAGER");
 * <br> 
 *    con.begin();
 * <br> 
 *    SednaStatement st1 = con.createStatement();
 * <br> 
 *    boolean call_res = st1.execute("LOAD 'region.xml' 'region'");
 * <br> 
 *    if (!call_res) System.out.println("Document has been loaded");
 * <br> 
 *    con.commit();
 * <br> 
 *    con.close();
 * </code>
 * <p> 
 *
 * @param queryText is an XQuery statement.
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 * @see SednaSerializedResult
 */
    public boolean execute(String queryText) throws DriverException;

/**
 * Executes XQuery statement.
 * <p> 
 *
 * @param in is <code>java.io.InputStream</code> object to read an XQuery statement from
 * @param resultType is one of the two possible result formats 
 * (one of the <code>ResultType.XML</code> or <code>ResultType.SXML</code>).
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 * @see SednaSerializedResult
 */
    public boolean execute(InputStream in, ResultType resultType)
            throws DriverException, IOException;

/**
 * Executes XQuery statement.
 * <p> 
 *
 * @param queryText is an XQuery statement.
 * @param resultType is one of the two possible result formats 
 * (one of the <code>ResultType.XML</code> or <code>ResultType.SXML</code>).
 *
 * @return Returns true if the XQuery statement produced the result, 
 * and the result can be obtained using <code>SerializedResult</code> object.
 * In case of statements that do not produce the result (such as updates or bulk load), this method returns false. 
 * @throws <code>DriverException</code> if some error occured.
 * @see SednaSerializedResult
 */
    public boolean execute(String queryText, ResultType resultType)
            throws DriverException;

/**
 * Loads XML document as a stand-alone document into the Sedna database.
 * @param in some input stream to read XML document from
 * @param doc_name the name of the document in database
 * @throws DriverException, IOException in case of error
 */
    public void loadDocument(InputStream in, String doc_name)
            throws DriverException, IOException;
/**
 * Loads XML document as a stand-alone document into the Sedna database.
 * @param xmldoc XML document as a string
 * @param doc_name the name of the document in database
 * @throws DriverException, IOException in case of error
 */
    public void loadDocument(String xmldoc, String doc_name)
            throws DriverException, IOException;

/**
 * Loads XML document into the collection of documents in Sedna database.
 * <p> 
 * Below is the simple example of how to load XML document from 
 * file "region.xml" into a collection "col1"
 * <p> 
 *
 * <code>
 *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *     con.begin();
 * <br> 
 *     SednaStatement st1 = con.createStatement();
 * <br> 
 *     st1.execute("CREATE COLLECTION 'col'");
 * <br> 
 *     st1.loadDocument((InputStream)new FileInputStream("E:/data/region.xml"), "target", "col");
 * <br> 
 *     System.out.println("Document has been loaded into collection");
 * <br> 
 *     con.close();
 * </code>
 * <p> 
 * @param in some input stream to rad XML document from
 * @param doc_name the name of the document in database
 * @param col_name the name of the existing collection in database where the document is loading
 * @throws DriverException, IOException in case of error
 */
    public void loadDocument(InputStream in, String doc_name, String col_name)
            throws DriverException, IOException;

/**
 * Loads XML document into the collection of documents in Sedna database.
 * @param xmldoc XML document as a string
 * @param doc_name the name of the document in collection
 * @param col_name the name of the collection
 * @throws DriverException, IOException in case of error
 */
    public void loadDocument(String xmldoc, String doc_name, String col_name)
            throws DriverException, IOException;
    
    //~--- get methods --------------------------------------------------------
/**
 * This method is used to obtain result data from when the query 
 * has been successfully executed with <code>execute</code> method.
 * <p> 
 *
 * For example:
 * <p> 
 *
 * <code>
 *    SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * <br> 
 *      con.begin();
 * <br> 
 *      SednaStatement st1 = con.createStatement();
 * <br> 
 *      // execute XQuery query.
 * <br> 
 *      boolean call_res = st1.execute("doc(\"region\")/regions/*");
 * <br> 
 *           
 * <br> 
 *      // if call_res is true the statement was not an update
 * <br> 
 *      // and we can use SednaSerializedResult object
 * <br> 
 *      if (call_res)    
 * <br> 
 *      {
 * <br> 
 *          System.out.println("Result:");
 * <br> 
 *          SednaSerializedResult pr1 = st1.getSerializedResult();
 * <br> 
 * <br> 
 *          item  = pr1.next();
 * <br> 
 * <br> 
 *          while (item != null) {
 * <br> 
 *              System.out.println(item);
 * <br> 
 *              System.out.println("=====================================\n");
 * <br> 
 *              item = pr1.next();
 * <br> 
 *          }
 * <br> 
 *      }
 * <br> 
 *      con.close();
 * </code>
 * <p> 
 *
 * @return <code>SednaSerializedResult</code> object that can be used 
 * to obtain result by items from this point on.
 * @see SednaSerializedResult
 */
    public SednaSerializedResult getSerializedResult();
}
