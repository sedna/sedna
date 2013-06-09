/*
 * File:  SednaStatementImpl.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;
import java.lang.System;

/**
 * Implementation of the {@link ru.ispras.sedna.driver.SednaStatement} for the
 * client/server protocol 3.0
 */
class SednaStatementImpl implements SednaStatement {

    private volatile SednaSerializedResultImpl serializedResult = null;
    private final BufferedInputStream sednaConnectionInputStream;
    private final OutputStream sednaConnectionOutputStream;
    private final boolean doTraceOutput;
    private static final ResultInterceptor defaultInterceptor = new DefaultResultInterceptor();
    private volatile ResultInterceptor resultInterceptor = defaultInterceptor;

    SednaStatementImpl(OutputStream sednaConnectionOutputStream,
                       BufferedInputStream sednaConnectionInputStream,
                       boolean doTraceOutput) {
        this.sednaConnectionOutputStream = sednaConnectionOutputStream;
        this.sednaConnectionInputStream = sednaConnectionInputStream;
        this.doTraceOutput = doTraceOutput;
    }

    public boolean execute(InputStream in)
            throws DriverException, IOException {
        return execute(in, ResultType.XML);
    }

    public boolean execute(String queryText) throws DriverException {
        return execute(queryText, ResultType.XML);
    }

    public boolean execute(InputStream in, ResultType resultType)
            throws DriverException, IOException {

        NetOps.Message msg = new NetOps.Message();
        int call_res = 1;

        while (call_res > 0)
        {
            call_res = in.read(msg.body, 6,
                               NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6);

            if (call_res > 0) {
                msg.instruction = NetOps.se_ExecuteLong;
                msg.length      = call_res + 6;
                /* String format */
                msg.body[1] = 0;

                setQueryResultType(msg, resultType);
                NetOps.writeInt(call_res, msg.body, 2);
                NetOps.writeMsg(msg, sednaConnectionOutputStream);
            }
        }

        msg.instruction = NetOps.se_LongQueryEnd;
        msg.length      = 0;
        NetOps.writeMsg(msg, sednaConnectionOutputStream);

        return executeResponseAnalyze(msg);
    }

    public boolean execute(String queryText, ResultType resultType)
            throws DriverException {

        NetOps.Message msg = new NetOps.Message();

        try {
            byte query_bytes[] = queryText.getBytes("utf8");

            if (query_bytes.length > NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6) {
                int bytes_sent = 0;

                while (bytes_sent < query_bytes.length) {

                    int bytes_to_send = Math.min(query_bytes.length - bytes_sent, NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6);

                    msg.instruction = NetOps.se_ExecuteLong;
                    msg.body[1] = 0;
                    msg.length = bytes_to_send + 6;
                    setQueryResultType(msg, resultType);

                    System.arraycopy(query_bytes, bytes_sent,
                                     msg.body, 6,
                                     bytes_to_send);
                    NetOps.writeInt(bytes_to_send, msg.body, 2);

                    bytes_sent += bytes_to_send;
                    NetOps.writeMsg(msg, sednaConnectionOutputStream);
                }

                msg.instruction = NetOps.se_LongQueryEnd;
                msg.length      = 0;
                NetOps.writeMsg(msg, sednaConnectionOutputStream);
            } else {
                msg.instruction = NetOps.se_Execute;
                msg.length      = query_bytes.length + 6;
                msg.body[1] = 0;
                setQueryResultType(msg, resultType);

                NetOps.writeInt(query_bytes.length, msg.body, 2);
                System.arraycopy(query_bytes, 0, msg.body, 6, query_bytes.length);
                NetOps.writeMsg(msg, sednaConnectionOutputStream);
            }

            return executeResponseAnalyze(msg);

        } catch (UnsupportedEncodingException uex) {
            throw new DriverException(ErrorCodes.SE5502, "");
        }
    }

    private boolean executeResponseAnalyze(NetOps.Message msg) throws DriverException {
        StringBuffer debugInfo = new StringBuffer();

        NetOps.readMsg(msg, sednaConnectionInputStream);

        /* Read debug information if any */
        boolean gotDebug = NetOps.readDebugInfo(msg, sednaConnectionInputStream, debugInfo);

        if (msg.instruction == NetOps.se_QuerySucceeded) {

            NetOps.StringItem sItem =
                    NetOps.readStringItem(sednaConnectionInputStream, doTraceOutput, resultInterceptor);

            serializedResult = new SednaSerializedResultImpl(sItem.item,
                    sItem.hasNextItem,
                    sednaConnectionInputStream,
                    sednaConnectionOutputStream,
                    doTraceOutput,
                    resultInterceptor);
            return true;
        } else if ( msg.instruction == NetOps.se_QueryFailed  ||
                msg.instruction == NetOps.se_UpdateFailed ||
                    msg.instruction == NetOps.se_ErrorResponse ) {

            DriverException ex = new DriverException(
                    NetOps.getErrorInfo(msg.body, msg.length),
                    NetOps.getErrorCode(msg.body));

            if (gotDebug) {
                ex.setDebugInfo(debugInfo);
            }
            throw ex;
        } else if (msg.instruction == NetOps.se_UpdateSucceeded) {
            return false;
        } else if (msg.instruction == NetOps.se_BulkLoadFileName) {

            return NetOps.bulkLoad(new String(msg.body, 5, msg.length - 5),
                    sednaConnectionInputStream,
                    sednaConnectionOutputStream);

        } else if (msg.instruction == NetOps.se_BulkLoadFromStream) {

            return NetOps.bulkLoad(System.in, sednaConnectionInputStream, sednaConnectionOutputStream);

        }
        else {
            /* Unknown message from the server */
            DriverException ex = new DriverException(ErrorCodes.SE3008,
                                                     Integer.toString(msg.instruction));
            if (gotDebug) {
                ex.setDebugInfo(debugInfo);
            }
            throw ex;
        }
    }

    public void loadDocument(InputStream in, String doc_name)
            throws DriverException, IOException {
        String queryText = "LOAD STDIN '" + doc_name + "'";
        NetOps.Message msg       = new NetOps.Message();

        msg.instruction = NetOps.se_Execute;
        msg.length      = queryText.length() + 6;    // body contains: result format (sxml=1 or xml=0) - 1 byte)

        msg.body[0] = 0;    // result format code
        msg.body[1] = 0;    // string format
        NetOps.writeInt(queryText.length(), msg.body, 2);

        byte query_bytes[] = queryText.getBytes();

		System.arraycopy(query_bytes, 0, msg.body, 6, queryText.length());
        NetOps.writeMsg(msg, sednaConnectionOutputStream);
        NetOps.readMsg(msg, sednaConnectionInputStream);

        if (msg.instruction == NetOps.se_BulkLoadFromStream) {
            NetOps.bulkLoad(in, this.sednaConnectionInputStream, this.sednaConnectionOutputStream);
        } else if (msg.instruction == NetOps.se_ErrorResponse ||
                   msg.instruction == NetOps.se_QueryFailed) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length),
                                      NetOps.getErrorCode(msg.body));
        } else {
            throw new DriverException(ErrorCodes.SE3008, Integer.toString(msg.instruction));
        }
    }

    public void loadDocument(String xmlDoc, String doc_name)
            throws DriverException, IOException {
            	
        loadDocument(new ByteArrayInputStream(xmlDoc.getBytes("utf8")), doc_name);
    }

    public void loadDocument(InputStream in, String doc_name, String col_name)
            throws DriverException, IOException {
        loadDocument(in, doc_name + "' '" + col_name);
    }
    
    public void loadDocument(String xmlDoc, String doc_name, String col_name)
            throws DriverException, IOException {
        loadDocument(new ByteArrayInputStream(xmlDoc.getBytes("utf8")), doc_name + "' '" + col_name);
    }

    public SednaSerializedResult getSerializedResult() {
        return this.serializedResult;
    }

    public synchronized void setResultInterceptor(ResultInterceptor interceptor) {
        this.resultInterceptor = interceptor;
    }

    public void resetResultInterceptor() {
        setResultInterceptor(defaultInterceptor);
    }

    private void setQueryResultType(NetOps.Message message,
                                    ResultType resultType) {
        if (resultType == ResultType.SXML) {
            message.body[0] = 1;
        } else {
            message.body[0] = 0;
        }
    }
}
