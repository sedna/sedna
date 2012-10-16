/*
 * File:  NetOps.java
 * Copyright (C) 2004-2012 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;
import java.lang.*;
import java.nio.*;
import java.nio.charset.*;

/**
 * Static functions to organize message exchange between client application and the Sedna server
 */
class NetOps {
    /* This driver supports version 3.0 of the Sedna Client/Server protocol */
    final static int           majorProtocolVer        = 3;
    final static int           minorProtocolVer        = 0;

    final static int           se_QueryTrace           = 0;
    final static int           se_QueryDebug           = 1;

    final static int SEDNA_SOCKET_MSG_BUF_SIZE         = 10240;

    /* Size of bulk load portion */
    final static int SEDNA_BULK_LOAD_PORTION           = 5120;

    /* Protocol instructions */
    final static int se_ErrorResponse                  = 100;
    final static int se_StartUp                        = 110;
    final static int se_SessionParameters              = 120;
    final static int se_SendSessionParameters          = 140;
    final static int se_SendAuthParameters             = 150;
    final static int se_RollbackTransactionOk          = 255;
    final static int se_RollbackTransactionFailed      = 265;
    final static int se_RollbackTransaction            = 225;
    final static int se_QuerySucceeded                 = 320;
    final static int se_QueryFailed                    = 330;
    final static int se_LongQueryEnd                   = 302;
    final static int se_GetNextItem                    = 310;
    final static int se_ExecuteLong                    = 301;
    final static int se_Execute                        = 300;
    final static int se_DebugInfo                      = 325;
    final static int se_CommitTransactionOk            = 250;
    final static int se_CommitTransactionFailed        = 260;
    final static int se_CommitTransaction              = 220;
    final static int se_BeginTransactionOk             = 230;
    final static int se_BeginTransactionFailed         = 240;
    final static int se_BeginTransaction               = 210;
    final static int se_AuthenticationParameters       = 130;
    final static int se_AuthenticationOK               = 160;
    final static int se_AuthenticationFailed           = 170;
    final static int se_UpdateSucceeded                = 340;
    final static int se_UpdateFailed                   = 350;
    final static int se_TransactionRollbackBeforeClose = 520;
    final static int se_ResultEnd                      = 375;
    final static int se_ItemPart                       = 360;
    final static int se_ItemEnd                        = 370;
    final static int se_CloseConnectionOk              = 510;
    final static int se_CloseConnection                = 500;
    final static int se_BulkLoadSucceeded              = 440;
    final static int se_BulkLoadPortion                = 410;
    final static int se_BulkLoadFromStream             = 431;
    final static int se_BulkLoadFileName               = 430;
    final static int se_BulkLoadFailed                 = 450;
    final static int se_BulkLoadError                  = 400;
    final static int se_BulkLoadEnd                    = 420;
    final static int se_SetSessionOptions              = 530;
    final static int se_SetSessionOptionsOk            = 540;

    /* Session option switches  */
    final static int se_Session_Debug_Off              = 0;
    final static int se_Session_Debug_On               = 1;
    final static int se_Session_Readonly_On            = 2;
    final static int se_Session_Readonly_Off           = 3;

    /* Value session option switches  */
    final static int se_Query_Exec_Timeout             = 4;

    /**
     * Bulk loads data to the server from the given input stream.
     * Sends se_BulkLoadEnd on the end in case of success.
     * @param in                input stream to load from.
     * @param bufInputStream    stream from the server.
     * @param outputStream      stream to the server.
     * @throws DriverException  given stream is broken or
     *                          something is wrong with protocol state.
     */
    private static void loadStream(InputStream in,
                                   BufferedInputStream bufInputStream,
                                   OutputStream outputStream)
            throws DriverException {

        NetOps.Message msg = new NetOps.Message();
        int bytes_read = 0;

        try {

            while (bytes_read != -1) {
                bytes_read = in.read(msg.body, 5, SEDNA_BULK_LOAD_PORTION);

                if (bytes_read != -1) {
                    msg.instruction = se_BulkLoadPortion;
                    msg.length      = bytes_read + 5;
                    msg.body[0]     = 0;
                    NetOps.writeInt(bytes_read, msg.body, 1);
                    NetOps.writeMsg(msg, outputStream);
                }
            }

        } catch (DriverException de) {
            msg.instruction = se_BulkLoadError;
            msg.length      = 0;
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);
            throw de;
        } catch (IOException ioe) {
            msg.instruction = se_BulkLoadError;
            msg.length      = 0;
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);
            throw new DriverException(ErrorCodes.SE3007, ioe.toString());
        }

        msg.instruction = se_BulkLoadEnd;
        msg.length      = 0;
        NetOps.writeMsg(msg, outputStream);
    }

    /**
     * Bulk loads data to the server from the given input stream.
     * Handles status response from the server.
     * @param in                input stream to load from.
     * @param bufInputStream    stream from the server.
     * @param outputStream      stream to the server.
     * @return                  false on a success.
     * @throws DriverException  given stream is broken, bulk load failed or
     *                          something is wrong with protocol state.
     */
    static boolean bulkLoad(InputStream in,
                            BufferedInputStream bufInputStream,
                            OutputStream outputStream)
            throws DriverException {

        NetOps.Message msg = new NetOps.Message();

        loadStream(in, bufInputStream, outputStream);

        NetOps.readMsg(msg, bufInputStream);

        if ( msg.instruction == se_BulkLoadSucceeded ||
                msg.instruction == se_UpdateSucceeded ) {
            return false;
        } else if (msg.instruction == se_BulkLoadFailed ||
                msg.instruction == se_UpdateFailed   ||
                msg.instruction == se_ErrorResponse ) {
            throw new DriverException(getErrorInfo(msg.body, msg.length),getErrorCode(msg.body));
        } else {
            throw new DriverException(ErrorCodes.SE3008, "");
        }
    }

    /**
     * Bulk loads one or more files to the server (either module or XML
     * document). After the first file has been loaded server may send
     * se_BulkLoadFileName messages with additional files to be loaded.
     * @param fileName          path to the file to be loaded.
     * @param bufInputStream    stream from the server.
     * @param outputStream      stream to the server.
     * @return                  false on a success.
     * @throws DriverException  either file was not found, bulk load failed or
     *                          something wrong with client server protocol.
     */
    static boolean bulkLoad(String fileName,
                            BufferedInputStream bufInputStream,
                            OutputStream outputStream)
            throws DriverException {

        NetOps.Message msg = new NetOps.Message();
        FileInputStream fis = null;
        String previousFileName = null;

        try {
            while (true) {
                if (fis != null) {
                    fis.close();
                    fis = null;
                }

                fis = new FileInputStream(fileName);
                loadStream(fis, bufInputStream, outputStream);
                NetOps.readMsg(msg, bufInputStream);

                if (msg.instruction == se_BulkLoadFileName) {
                    previousFileName = fileName;
                    fileName = new String(msg.body, 5, msg.length - 5);
                } else if ( msg.instruction == se_BulkLoadSucceeded ||
                        msg.instruction == se_UpdateSucceeded ) {
                    return false;
                } else if (msg.instruction == se_BulkLoadFailed ||
                        msg.instruction == se_UpdateFailed   ||
                        msg.instruction == se_ErrorResponse ) {
                    throw new DriverException(getErrorInfo(msg.body, msg.length),
                            getErrorCode(msg.body));
                } else {
                    /* Unknown message from the server */
                    throw new DriverException(ErrorCodes.SE3008,
                            Integer.toString(msg.instruction));
                }
            }
        }
        catch (IOException ex){
            msg.instruction = NetOps.se_BulkLoadError;
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);
            if (ex instanceof FileNotFoundException)
                /* Can't open file to load*/
                throw new DriverException(ErrorCodes.SE4042, fileName);
            else
                /* Can't close file which has been loaded */
                throw new DriverException(ErrorCodes.SE4043, previousFileName);
        }
        finally {
            try {
                if (fis != null) fis.close();
            } catch (IOException ignore) {}
        }
    }


    static void driverErrOut(String str) {
        if (Debug.DEBUG)
            System.err.println(str);
    }

    private static int readInt(BufferedInputStream bufInputStream)
            throws DriverException {
        int call_res, integer;
        byte int_array[] = new byte[4];

        try {
            call_res = bufInputStream.read(int_array, 0, 4);
            if (call_res != 4) throw new DriverException(ErrorCodes.SE3007, "");

            integer = (((int_array[0] & 0xff) << 24)
                    | ((int_array[1] & 0xff) << 16)
                    | ((int_array[2] & 0xff) << 8) | (int_array[3] & 0xff));
        }catch (IOException ioe) {
            throw new DriverException(ErrorCodes.SE3007, ioe.toString());
        }

        return integer;
    }

    static void readMsg(Message msg, BufferedInputStream bufInputStream)
            throws DriverException {

        try {
            msg.instruction = NetOps.readInt(bufInputStream);
            msg.length      = NetOps.readInt(bufInputStream);
            if (msg.length > SEDNA_SOCKET_MSG_BUF_SIZE) {
                throw new DriverException(ErrorCodes.SE3012, "");
            }

            if (msg.length != 0) {
                int count;
                int pos   = 0;

                while (pos < msg.length) {
                    count = bufInputStream.read(msg.body, pos,
                            msg.length - pos);

                    if (count != -1) {
                        pos += count;
                    }
                }
            }
        } catch (IOException ioe) {
            throw new DriverException(ErrorCodes.SE3007, ioe.toString());
        }
    }

    /**
     * Reads query debug information.
     * @param msg initial message from the server which may contain debug info
     * @param is connection stream with the server to read messages from
     * @param item buffer to write information into
     * @return if there were any information returns <code>true</code> and appends it to the <i>item</i> buffer,
     * otherwise returns <code>false</code>.
     * @throws DriverException if failed to get debug information or something wrong with connection
     */
    static boolean readDebugInfo(NetOps.Message msg, BufferedInputStream is, StringBuffer item) throws DriverException
    {
        ByteBuffer  byteBuf;
        CharBuffer  charBuf = CharBuffer.allocate(SEDNA_SOCKET_MSG_BUF_SIZE);
        CharsetDecoder csd  = Charset.forName("utf8").newDecoder();

        boolean gotDebug;

        int debug_type = net_int2int(msg.body);

        gotDebug = ((msg.instruction == NetOps.se_DebugInfo) && (debug_type == se_QueryDebug));

        /* Read debug information if any */
        while ((msg.instruction == NetOps.se_DebugInfo) && (debug_type == se_QueryDebug))
        {
            byteBuf = ByteBuffer.wrap(msg.body, 9, msg.length - 9);
            csd.decode(byteBuf, charBuf, false);
            item.ensureCapacity(charBuf.length());

            try {
                item.append(charBuf.flip());
            } catch (OutOfMemoryError ignore) {}

            charBuf.clear();

            NetOps.readMsg(msg, is);
            debug_type = net_int2int(msg.body);
        }

        return gotDebug;
    }

    /**
     * Reads query trace information.
     * @param msg initial message from the server which may contain trace info
     * @param is connection stream with the server to read messages from
     * @param item buffer to write information into
     * @return if there were any information returns <code>true</code> and appends it to the <i>item</i> buffer,
     * otherwise returns <code>false</code>.
     * @throws DriverException if failed to get trace information or something wrong with connection
     */
    private static boolean readTrace(NetOps.Message msg, BufferedInputStream is, StringBuffer item) throws DriverException
    {

        ByteBuffer  byteBuf;
        CharBuffer  charBuf = CharBuffer.allocate(SEDNA_SOCKET_MSG_BUF_SIZE);
        CharsetDecoder csd  = Charset.forName("utf8").newDecoder();

        boolean gotTrace;

        int debug_type = net_int2int(msg.body);

        gotTrace = (msg.instruction == NetOps.se_DebugInfo) && (debug_type == NetOps.se_QueryTrace);

        // read debug information if any 
        while ((msg.instruction == NetOps.se_DebugInfo) && (debug_type == NetOps.se_QueryTrace))
        {
            byteBuf = ByteBuffer.wrap(msg.body, 9, msg.length - 9);
            csd.decode(byteBuf, charBuf, false);
            item.ensureCapacity(charBuf.length());

            try {
                item.append(charBuf.flip());
            } catch (OutOfMemoryError ignore) {}

            charBuf.clear();

            NetOps.readMsg(msg, is);
            debug_type = net_int2int(msg.body);
        }

        if (gotTrace) item.append("\n");
        return gotTrace;
    }

    /**
     * Reads complete item from the stream connected with server.
     * @param is connection stream with the server to read item from
     * @param doTraceOutput either read trace information or not
     * @return {@link ru.ispras.sedna.driver.NetOps.StringItem} which encapsulates text representation of the item
     * and provides information if there is next item.
     * @throws DriverException if failed to get item or something wrong with connection
     */
    static StringItem readStringItem(BufferedInputStream is, boolean doTraceOutput)
            throws DriverException {

        NetOps.Message     msg   = new NetOps.Message();
        NetOps.StringItem  sitem = new NetOps.StringItem();
        boolean gotTrace, gotDebug;
        sitem.item = new StringBuffer();
        StringBuffer debugInfo = new StringBuffer();

        ByteBuffer     byteBuf;
        CharBuffer     charBuf = CharBuffer.allocate(SEDNA_SOCKET_MSG_BUF_SIZE);
        CharsetDecoder csd = Charset.forName("utf8").newDecoder();

        NetOps.readMsg(msg, is);

        gotDebug = NetOps.readDebugInfo(msg, is, debugInfo);
        gotTrace = doTraceOutput && NetOps.readTrace(msg, is, sitem.item);

        if (msg.instruction == NetOps.se_ItemEnd) {
            /* If we got se_ItemEnd before se_ItemPart/se_ItemStart
             * it means that query returned empty xs:string. */
            sitem.hasNextItem = true;
            return sitem;
        }
        if (msg.instruction == NetOps.se_ResultEnd) {
            if (!gotTrace) sitem.item = null;
            sitem.hasNextItem = false;
            return sitem;
        }

        while ((msg.instruction != NetOps.se_ItemEnd) && (msg.instruction != NetOps.se_ResultEnd)) {

            if (msg.instruction == NetOps.se_ErrorResponse) {
                DriverException ex = new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
                if (gotDebug) ex.setDebugInfo(debugInfo);
                throw ex;
            }
            if (msg.instruction == NetOps.se_ItemPart)
            {
                byteBuf = ByteBuffer.wrap(msg.body, 5, msg.length - 5);
                csd.decode(byteBuf, charBuf, false);

                sitem.item.ensureCapacity(charBuf.length());

                try {
                    sitem.item.append(charBuf.flip());
                } catch (OutOfMemoryError ignore) {}

                charBuf.clear();
            }

            NetOps.readMsg(msg, is);
            if (doTraceOutput) NetOps.readTrace(msg, is, sitem.item);
        }

        if (msg.instruction == NetOps.se_ResultEnd) {
            sitem.hasNextItem = false;
        }

        if (msg.instruction == NetOps.se_ItemEnd) {
            sitem.hasNextItem = true;
        }
        return sitem;
    }

    private static void writeInt(int i, BufferedOutputStream bufOutputStream)
            throws IOException {
        bufOutputStream.write(0xff & (i >> 24));
        bufOutputStream.write(0xff & (i >> 16));
        bufOutputStream.write(0xff & (i >> 8));
        bufOutputStream.write(0xff & i);
    }

    static void writeInt(int i, byte[] byte_array, int pos) {
        byte_array[pos]     = (new Integer(0xff & (i >> 24))).byteValue();
        byte_array[pos + 1] = (new Integer(0xff & (i >> 16))).byteValue();
        byte_array[pos + 2] = (new Integer(0xff & (i >> 8))).byteValue();
        byte_array[pos + 3] = (new Integer(0xff & i)).byteValue();
    }

    static void writeMsg(Message msg, OutputStream outputStream)
            throws DriverException {
        if (msg.length > SEDNA_SOCKET_MSG_BUF_SIZE) {
            throw new DriverException(ErrorCodes.SE3012, "");
        }

        BufferedOutputStream bufOutputStream =
                new BufferedOutputStream(outputStream);

        try {
            NetOps.writeInt(msg.instruction, bufOutputStream);
            NetOps.writeInt(msg.length, bufOutputStream);

            if (msg.length != 0) {
                bufOutputStream.write(msg.body, 0, msg.length);
            }

            bufOutputStream.flush();
        } catch (IOException ioe) {
            throw new DriverException(ErrorCodes.SE3006, "");
        }
    }

    /**
     * Gets error message body and makes a string that is error info.
     * @param body byte array which contains text to retrieve message from
     * @param length shift of error code in the array 
     * @return error message retrieved from the message text representation
     */
    static String getErrorInfo(byte[] body, int length) {
        return new String(body, 9, length - 9);
    }

    /**
     * @param body byte array which contains text to retrieve error code from
     * @return error code retrieved from the text representation
     */
    static int getErrorCode(byte[] body) {
        return net_int2int(body);
    }

    private static int net_int2int(byte[] body) {
        return (((body[0] & 0xff) << 24)
                | ((body[1] & 0xff) << 16)
                | ((body[2] & 0xff) << 8)
                | (body[3] & 0xff));
    }

    /**
     * Class encapsulates one message which is passed between the server and a client.
     */
    static class Message {
        byte body[];
        int  instruction;
        int  length;

        /**
         * Initializes a new empty client-server message instance. 
         */
        Message() {
            this.body = new byte[SEDNA_SOCKET_MSG_BUF_SIZE];
        }
    }

    /**
     * Class encapsulates one XQuery item returned by server. 
     */
    static class StringItem {
        boolean      hasNextItem;
        StringBuffer item;
    }
}
