/*
 * File:  SednaStatement.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;

/**
 * Provides interface methods for loading documents
 * into the database, executing statements of the database language.
 * and retrieving results that statements produce.
 * {@link ru.ispras.sedna.driver.SednaStatement} instance is created with
 * {@link SednaConnection#createStatement()} method
 * @see SednaConnection#createStatement
 */
public interface SednaStatement {

    /**
     * Reads XQuery statement from the input stream and executes it. For example let's
     * XQuery statement 'CREATE COLLECTION 'col1'' is taken from file the '1.xquery':
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.begin();
     *     SednaStatement st1 = con.createStatement();
           FileInputStream f = new FileInputStream("1.xquery");
     *     boolean call_res = st1.execute(f);
     *     if(!call_res) System.out.println("Collection has been created");
     *     con.commit();
     *     con.close();
     * </pre>
     *
     * @param in is {@link java.io.InputStream} object to read an XQuery statement from
     * @return <code>true</code> if the XQuery statement produced the result,
     * and the result can be obtained using {@link ru.ispras.sedna.driver.SednaSerializedResult} object.
     * In case of statements that do not produce the result (such as updates or bulk load), this method
     * returns <code>false</code>.
     * @throws DriverException if some error occurred during the statement execution.
     * @throws IOException if failed to read from the given input stream.
     * @see ru.ispras.sedna.driver.SednaSerializedResult
     */
    public boolean execute(InputStream in) throws DriverException, IOException;

    /**
     * Executes XQuery statement specified by string. For example:
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "db1", "SYSTEM", "MANAGER");
     *     con.begin();
     *     SednaStatement st1 = con.createStatement();
     *     boolean call_res = st1.execute("LOAD 'region.xml' 'region'");
     *     if (!call_res) System.out.println("Document has been loaded");
     *     con.commit();
     *     con.close();
     * </pre>
     *
     * @param queryText is an XQuery statement.
     * @return <code>true</code> if the XQuery statement produced the result,
     * and the result can be obtained using {@link ru.ispras.sedna.driver.SednaSerializedResult} object.
     * In case of statements that do not produce the result (such as updates or bulk load), this method
     * returns <code>false</code>.
     * @throws DriverException if some error occurred during the statement execution.
     * @see ru.ispras.sedna.driver.SednaSerializedResult
     */
    public boolean execute(String queryText) throws DriverException;

    /**
     * Reads XQuery statement from the input stream, executes it and returns result of the specified type.
     * See {@link SednaStatement#execute(InputStream)} for example of code.
     * @param in server connected input stream to read an XQuery statement from
     * @param resultType is one of the two possible result formats, either
     * {@link ru.ispras.sedna.driver.ResultType#XML} or {@link ru.ispras.sedna.driver.ResultType#SXML}
     * @return <code>true</code> if the XQuery statement produced the result,
     * and the result can be obtained using {@link SednaSerializedResult} object returned by
     * {@link SednaStatement#getSerializedResult()} method. In case of statements that do not produce
     * the result (such as updates or bulk load), this method returns <code>false</code>.
     * @throws DriverException if some error occurred during the statement execution..
     * @throws java.io.IOException if failed to read from the given input stream.
     * @see ru.ispras.sedna.driver.SednaSerializedResult
     */
    public boolean execute(InputStream in, ResultType resultType)
            throws DriverException, IOException;

    /**
     * Executes an XQuery statement and returns result of the provided type.
     * See {@link SednaStatement#execute(String)} for example of code.
     * @param queryText is an XQuery statement.
     * @param resultType is one of the two possible result formats, either
     * {@link ru.ispras.sedna.driver.ResultType#XML} or {@link ru.ispras.sedna.driver.ResultType#SXML}*
     * @return <code>true</code> if the XQuery statement produced the result,
     * and the result can be obtained using {@link SednaSerializedResult} object returned by
     * {@link SednaStatement#getSerializedResult()} method. In case of statements that do not produce
     * the result (such as updates or bulk load), this method returns <code>false</code>.
     * @throws DriverException if some error occurred during the statement execution.
     * @see ru.ispras.sedna.driver.SednaSerializedResult
     */
    public boolean execute(String queryText, ResultType resultType)
            throws DriverException;

    /**
     * Loads XML document from the input stream as a stand-alone document into the Sedna database.
     * See {@link SednaStatement#loadDocument(InputStream, String, String)} for example of code.
     * @param in input stream to read XML document from
     * @param docName name of the document in database
     * @throws DriverException in case of error during the bulk load
     * @throws java.io.IOException if failed to read from the given input stream.
     */
    public void loadDocument(InputStream in, String docName)
            throws DriverException, IOException;
    /**
     * Loads XML document as a stand-alone document into the Sedna database.
     * See {@link SednaStatement#loadDocument(InputStream, String, String)} for example of code.
     * @param xmlDoc XML document as a string
     * @param docName name of the document in database
     * @throws DriverException in case of error during bulk load
     * @throws java.io.IOException if failed to read from the given input stream.
     */
    public void loadDocument(String xmlDoc, String docName)
            throws DriverException, IOException;

    /**
     * Loads XML document into the collection of documents in Sedna database.
     * See {@link SednaStatement#loadDocument(InputStream, String, String)} for example of code.
     * @param xmlDoc XML document as a string
     * @param docName name of the document in database
     * @param colName name of the existing collection in database the document will be loaded into
     * @throws DriverException in case of error during bulk load
     * @throws java.io.IOException if failed to read from the given input stream.
     */
    public void loadDocument(String xmlDoc, String docName, String colName)
            throws DriverException, IOException;

    /**
     * Loads XML document into the collection of documents in Sedna database.
     * <p>
     * Below is the simple example of how to load XML document from
     * file <i>region.xml</i> into the collection <i>col</i>.
     *
     * <pre>
     *     SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *     con.begin();
     *     SednaStatement st1 = con.createStatement();
     *     st1.execute("CREATE COLLECTION 'col'");
     *     st1.loadDocument((InputStream) new FileInputStream("E:/data/region.xml"), "target", "col");
     *     System.out.println("Document has been loaded into collection");
     *     con.close();
     * </pre>
     *
     * @param in input stream to read XML document from
     * @param docName name of the document in the database
     * @param colName name of the existing collection in database the document will be loaded into
     * @throws DriverException in case of error during bulk load
     * @throws IOException if failed to read from the given input stream.
     */
    public void loadDocument(InputStream in, String docName, String colName)
            throws DriverException, IOException;

    /**
     * This method is used to obtain result data from when the query has been successfully
     * executed with one of the <code>execute</code> methods of the
     * {@link ru.ispras.sedna.driver.SednaStatement} interface implementation instance.
     * For example:
     *
     * <pre>
     *      SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *      con.begin();
     *      SednaStatement st1 = con.createStatement();
     *
     *      // Execute XQuery query.
     *      boolean res = st1.execute("doc('region')/regions/*");
     *
     *      if (res) {
     *          System.out.println("Result:");
     *          SednaSerializedResult pr1 = st1.getSerializedResult();
     *          item  = pr1.next();
     *          while (item != null) {
     *              System.out.println(item);
     *              System.out.println("=====================================\n");
     *              item = pr1.next();
     *          }
     *      }
     *      con.close();
     *</pre>
     *
     * @return <code>SednaSerializedResult</code> object that can be used to obtain result by items.
     * @see ru.ispras.sedna.driver.SednaSerializedResult
     */
    public SednaSerializedResult getSerializedResult();


    /**
     * Interceptors provide custom result processing logic.
     * Use cases include: filtration, aggregation, redirect output to the file, etc.
     * For example:
     *
     * <pre>
     *      SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
     *      con.begin();
     *      SednaStatement st = con.createStatement();
     *
     *      final FileChannel out = new FileOutputStream("/tmp/result").getChannel();
     *
     *      // next() will return empty result every time, actual result
     *      // will be redirected into file
     *      ResultInterceptor interceptor = new ResultInterceptor() {
     *          private final ByteBuffer empty = ByteBuffer.allocate(0);
     *
     *          public ByteBuffer handle(ByteBuffer res) {
     *              try {
     *                  out.write(res);
     *              } catch (IOException ignore) {
     *                  //Never! Never! do exception handling this way in your code :)
     *              }
     *              return empty;
     *          }
     *      };
     *
     *      st.setResultInterceptor(interceptor);
     *
     *      // execute XQuery
     *      boolean res = st.execute("for $i in (1 to 1000) return $i");
     *
     *      if (res) {
     *          SednaSerializedResult pr = st.getSerializedResult();
     *          item = pr.next();
     *          while (item != null) {
     *              item = pr.next();
     *          }
     *      }
     *      con.close();
     *</pre>
     *
     * @see ru.ispras.sedna.driver.ResultInterceptor
     */
    public void setResultInterceptor(ResultInterceptor interceptor);
}
