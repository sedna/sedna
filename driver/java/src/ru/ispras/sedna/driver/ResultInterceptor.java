/*
 * File: ResultInterceptor.java
 * Copyright (C) 2004-2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.nio.ByteBuffer;

/**
 * Interceptors provide custom result processing logic.
 * Use cases include: filtering, aggregation, redirecting output to the file, etc.
 * <br/><br/>
  For example. To completely redirect all results to the file:
 * <br/><br/>
 * <pre>
 * SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 * con.begin();
 * SednaStatement st = con.createStatement();
 *
 * final FileChannel out = new FileOutputStream("/tmp/result").getChannel();
 *
 * // next() will return empty result every time, actual result
 * // will be redirected into file
 * ResultInterceptor interceptor = new ResultInterceptor() {
 *
 *     public ByteBuffer handle(ByteBuffer res) {
 *         try {
 *             out.write(res);
 *         } catch (IOException ignore) {
 *             //Never! Never! do exception handling this way in your code :)
 *         }
 *         return null;
 *     }
 * };
 *
 * st.setResultInterceptor(interceptor);
 *
 * // execute XQuery
 * boolean res = st.execute("for $i in (1 to 1000) return $i");
 *
 * if (res) {
 *     SednaSerializedResult pr = st.getSerializedResult();
 *     item = pr.next();
 *     while (item != null) {
 *         item = pr.next();
 *     }
 * }
 * con.close();
 *</pre>
 */
public abstract class ResultInterceptor {

    /**
     * Called every time next portion of the result item is received from Sedna.
     * @param res ready to use (don't need to flip, rewind, etc)
     *            read-only buffer which contains raw UTF-8 byte array with the
     *            part of the result item
     * @return post-processed buffer, it will be appended to the string returned
     * by the <code>next()</code> call. If <code>null</code> is returned result
     * part will be skipped.
     */
    abstract public ByteBuffer handle(ByteBuffer res);

    /**
     * Called every time if the end of the item is reached.
     * Default empty implementation is provided.
     */
    public void itemEnd() {}

}
