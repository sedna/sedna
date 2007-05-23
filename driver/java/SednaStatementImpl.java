
/*
 * File:  SednaStatementImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

import java.lang.System;

import java.net.Socket;

//~--- classes ----------------------------------------------------------------

class SednaStatementImpl implements SednaStatement {
    private SednaSerializedResultImpl serializedResult = null;
    private SednaSerializedResult     currentResult    = null;
    private BufferedInputStream       bufInputStream;
    private OutputStream              outputStream;
    private boolean                   doTraceOutput;

    //~--- constructors -------------------------------------------------------

    SednaStatementImpl(OutputStream outputStream,
                       BufferedInputStream bufInputStream,
                       SednaSerializedResult currentResult,
                       boolean doTraceOutput) {
        this.outputStream   = outputStream;
        this.bufInputStream = bufInputStream;
        this.currentResult  = currentResult;
        this.doTraceOutput = doTraceOutput;
    }

    //~--- methods ------------------------------------------------------------

    // @param - Reader to take the query from  
    // returns true if PlaneResult of this Statement is a result of a query
    // false if it was Update
    // throws Exception if there were errors
    public boolean execute(InputStream in)
            throws DriverException, IOException {
        return execute(in, ResultType.XML);
    }

    // @param - query as a String  
    // returns true if PlaneResult of this Statement is a result of a query
    // false if it was Update
    // throws Exception if there were errors
    public boolean execute(String queryText) throws DriverException {
        return execute(queryText, ResultType.XML);
    }

    // @param - Reader to take the query from  
    // returns true if PlaneResult of this Statement is a result of a query
    // false if it was Update
    // throws Exception if there were errors
    public boolean execute(InputStream in, ResultType resultType)
            throws DriverException, IOException {

        NetOps.Message msg    = new NetOps.Message();
        StringBuffer debugInfo = new StringBuffer();
        boolean gotDebug = false;

        try {
            int call_res = 1;

            while (call_res > 0)    // 0 - array is full; -1 - end of stream or error
            {
                call_res = in.read(msg.body, 6,
                                   NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6);

                if (call_res > 0) {
                    msg.instruction = NetOps.se_ExecuteLong;
                    msg.length      = call_res + 6;    // body containes: result format (sxml=1 or xml=0) - 1 byte)

                    // string format - 1 byte;
                    // string length - 4 bytes
                    // string

                    // writing queryText
                    setQueryResultType(msg, resultType);
                    msg.body[1] = 0;    // string format
                    NetOps.writeInt(call_res, msg.body, 2);
                    NetOps.writeMsg(msg, outputStream);
                }
            }

            msg.instruction = NetOps.se_LongQueryEnd;
            msg.length      = 0;    // body containes: result format (sxml=1 or xml=0) - 1 byte)
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);

            // read debug information if any
            gotDebug = NetOps.readDebugInfo(msg, bufInputStream, debugInfo);

            if (msg.instruction == NetOps.se_QuerySucceeded) {
                NetOps.String_item sitem =
                    NetOps.readStringItem(bufInputStream, this.doTraceOutput);

                this.serializedResult =
                    new SednaSerializedResultImpl(sitem.item,
                                                  sitem.hasNextItem,
                                                  this.bufInputStream,
                                                  this.outputStream,
                                                  this.doTraceOutput);

                return true;
            } else if (msg.instruction == NetOps.se_QueryFailed) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;

            } else if (msg.instruction == NetOps.se_UpdateFailed) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;

            } else if (msg.instruction == NetOps.se_ErrorResponse) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;
                
            } else if (msg.instruction == NetOps.se_UpdateSucceeded) {
 
                return false;
 
            } else if (msg.instruction == NetOps.se_BulkLoadFileName) {
            	
                String          file_name = new String(msg.body, 5,
                                                msg.length - 5);
                FileInputStream fis       = new FileInputStream(file_name);
                boolean         res       = NetOps.bulkLoad(fis,
                                                this.bufInputStream,
                                                this.outputStream);
                return res;
 
            } else if (msg.instruction == NetOps.se_BulkLoadFromStream) {
 
                boolean res = NetOps.bulkLoad(System.in, this.bufInputStream, this.outputStream);
                return res;
 
            }
            else
            {
            	DriverException ex = new DriverException(ErrorCodes.SE3008, "");
            	if (gotDebug) ex.setDebugInfo(debugInfo);
            	throw ex;
            }
        } catch (FileNotFoundException fnfe) {
            msg.instruction = NetOps.se_BulkLoadError;
            NetOps.writeMsg(msg, outputStream);

            throw new DriverException(ErrorCodes.SE4042, "");
        } catch (UnsupportedEncodingException uex) {
            throw new DriverException(ErrorCodes.SE5502, "");
        } catch (DriverException de) {
            throw de;
        }
    }

    // @param - query as a String  
    // returns true if PlaneResult of this Statement is a result of a query
    // false if it was Update
    // throws Exception if there were errors
    public boolean execute(String queryText, ResultType resultType)
            throws DriverException {
        NetOps.Message msg = new NetOps.Message();
        StringBuffer debugInfo = new StringBuffer();
        boolean gotDebug = false;

        try {
            byte query_bytes[] = queryText.getBytes("utf8");

            if (query_bytes.length > NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6) {
                int bytes_sent = 0;

                while (bytes_sent < query_bytes.length) {
                    msg.instruction = NetOps.se_ExecuteLong;
                    msg.length      = NetOps.SEDNA_SOCKET_MSG_BUF_SIZE;    // body containes: result format (sxml=1 or xml=0) - 1 byte)

                    // string format - 1 byte;
                    // string length - 4 bytes
                    // string
                    // writing queryText
                    setQueryResultType(msg, resultType);
                    msg.body[1] = 0;    // string format
                    NetOps.writeInt(NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6,
                                    msg.body, 2);

                    if ((query_bytes.length - bytes_sent)
                            > (NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6)) {
                        System.arraycopy((Object) query_bytes, bytes_sent,
                                         (Object) msg.body, 6,
                                         NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6);
                        bytes_sent += NetOps.SEDNA_SOCKET_MSG_BUF_SIZE - 6;
                    } else {
                        System.arraycopy((Object) query_bytes, bytes_sent,
                                         (Object) msg.body, 6,
                                         (query_bytes.length - bytes_sent));
                        msg.length = (query_bytes.length - bytes_sent) + 6;    // body containes: result format (sxml=1 or xml=0) - 1 byte)
                        bytes_sent += (query_bytes.length - bytes_sent);
                    }

                    NetOps.writeMsg(msg, outputStream);
                }

                msg.instruction = NetOps.se_LongQueryEnd;
                msg.length      = 0;
                NetOps.writeMsg(msg, outputStream);
            } else {
                msg.instruction = NetOps.se_Execute;
                msg.length      = query_bytes.length + 6;    // body containes: result format (sxml=1 or xml=0) - 1 byte)

                // string format - 1 byte;
                // string length - 4 bytes
                // string
                // writing queryText
                setQueryResultType(msg, resultType);
                msg.body[1] = 0;    // string format
                NetOps.writeInt(query_bytes.length, msg.body, 2);
                System.arraycopy((Object) query_bytes, 0, (Object) msg.body,
                                 6, query_bytes.length);
                NetOps.writeMsg(msg, outputStream);
            }

            NetOps.readMsg(msg, bufInputStream);

            // read debug information if any
            gotDebug = NetOps.readDebugInfo(msg, bufInputStream, debugInfo);
            if (msg.instruction == NetOps.se_QuerySucceeded) {
                NetOps.String_item sitem =
                    NetOps.readStringItem(bufInputStream, this.doTraceOutput);

                this.serializedResult =
                    new SednaSerializedResultImpl(sitem.item,
                                                  sitem.hasNextItem,
                                                  this.bufInputStream,
                                                  this.outputStream,
                                                  this.doTraceOutput);

                return true;
            } else if (msg.instruction == NetOps.se_QueryFailed) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;

            } else if (msg.instruction == NetOps.se_UpdateFailed) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;

            } else if (msg.instruction == NetOps.se_ErrorResponse) {
            	
            	DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), 
            	                                         NetOps.getErrorCode(msg.body));
            	if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;
                
            } else if (msg.instruction == NetOps.se_UpdateSucceeded) {
 
                return false;
 
            } else if (msg.instruction == NetOps.se_BulkLoadFileName) {
            	
                String          file_name = new String(msg.body, 5,
                                                msg.length - 5);
                FileInputStream fis       = new FileInputStream(file_name);
                boolean         res       = NetOps.bulkLoad(fis,
                                                this.bufInputStream,
                                                this.outputStream);
                return res;
 
            } else if (msg.instruction == NetOps.se_BulkLoadFromStream) {
 
                boolean res = NetOps.bulkLoad(System.in, this.bufInputStream, this.outputStream);
                return res;
 
            }
            else
            {
            	DriverException ex = new DriverException(ErrorCodes.SE3008, "");
            	if (gotDebug) ex.setDebugInfo(debugInfo);
            	throw ex;
            }
        } catch (FileNotFoundException fnfe) {
            msg.instruction = NetOps.se_BulkLoadError;
            NetOps.writeMsg(msg, outputStream);

            throw new DriverException(ErrorCodes.SE4042, "");
        } catch (UnsupportedEncodingException uex) {
            throw new DriverException(ErrorCodes.SE5502, "");
        } catch (DriverException de) {
            throw de;
        }
    }

    public void loadDocument(InputStream in, String doc_name)
            throws DriverException, IOException {
        String         queryText = "LOAD STDIN \"" + doc_name + "\"";
        NetOps.Message msg       = new NetOps.Message();

        msg.instruction = NetOps.se_Execute;
        msg.length      = queryText.length() + 6;    // body containes: result format (sxml=1 or xml=0) - 1 byte)

        // string format - 1 byte;
        // string length - 4 bytes
        // string

        // writing queryText
        msg.body[0] = 0;    // result format code
        msg.body[1] = 0;    // string format
        NetOps.writeInt(queryText.length(), msg.body, 2);

        byte query_bytes[] = queryText.getBytes();

		System.arraycopy((Object) query_bytes, 0, (Object) msg.body, 6, queryText.length());
        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);

        if (msg.instruction == NetOps.se_BulkLoadFromStream) {
            NetOps.bulkLoad(in, this.bufInputStream, this.outputStream);
        } else if (msg.instruction == NetOps.se_ErrorResponse) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction == NetOps.se_QueryFailed) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else {
            throw new DriverException(ErrorCodes.SE3008, "");
        }
    }

    public void loadDocument(String xmldoc, String doc_name)
            throws DriverException, IOException {
            	
        loadDocument(new ByteArrayInputStream(xmldoc.getBytes("utf8")), doc_name);
    }

    public void loadDocument(InputStream in, String doc_name, String col_name)
            throws DriverException, IOException {
        loadDocument(in, doc_name + "\" \"" + col_name);
    }
    
    public void loadDocument(String xmldoc, String doc_name, String col_name)
            throws DriverException, IOException {
        loadDocument(new ByteArrayInputStream(xmldoc.getBytes("utf8")), doc_name + "\" \"" + col_name);
    }

    //~--- get methods --------------------------------------------------------

    public SednaSerializedResult getSerializedResult() {
        return this.serializedResult;
    }

    //~--- set methods --------------------------------------------------------

    private void setQueryResultType(NetOps.Message message,
                                    ResultType resultType) {
        if (resultType == ResultType.SXML) {
            message.body[0] = 1;
        } else {
            message.body[0] = 0;
        }
    }
}
