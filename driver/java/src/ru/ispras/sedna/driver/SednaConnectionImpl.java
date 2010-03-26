/*
 * File:  SednaConnectionImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

package ru.ispras.sedna.driver;

import java.io.*;
import java.net.Socket;

class SednaConnectionImpl implements SednaConnection {
    private boolean       isClose       = false;
    private boolean       doTraceOutput = true;
    private Socket        socket;
    BufferedInputStream   bufInputStream;
    OutputStream          outputStream;

    public void begin() throws DriverException {
        if (this.isClose) {
            throw new DriverException(ErrorCodes.SE3028, "");
        }

        NetOps.Message msg = new NetOps.Message();

        msg.instruction = NetOps.se_BeginTransaction;
        msg.length      = 0;
        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);

        if ((msg.instruction == NetOps.se_BeginTransactionFailed)
                || (msg.instruction == NetOps.se_ErrorResponse)) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction != NetOps.se_BeginTransactionOk) {
            throw new DriverException(ErrorCodes.SE3008, "");    // Unknown message from server
        }
    }

    public void close() throws DriverException {
        if (this.isClose) {
            throw new DriverException(ErrorCodes.SE3028, "");
        }

        NetOps.Message msg = new NetOps.Message();

        msg.instruction = NetOps.se_CloseConnection;
        msg.length      = 0;
        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);

        if (msg.instruction == NetOps.se_TransactionRollbackBeforeClose) {
            try {
                this.socket.close();
                this.outputStream.close();
                this.bufInputStream.close();
                this.isClose = true;
            } catch (IOException ioe) {

            }
        } else if (msg.instruction == NetOps.se_ErrorResponse) {
            this.isClose = true;

            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction != NetOps.se_CloseConnectionOk) {
            this.isClose = true;

            throw new DriverException(ErrorCodes.SE3008, "");    // Unknown message from server
        }

        this.isClose = true;
    }

    public void commit() throws DriverException {
        if (this.isClose) {
            throw new DriverException(ErrorCodes.SE3028, "");
        }

        NetOps.Message msg = new NetOps.Message();

        msg.instruction = NetOps.se_CommitTransaction;
        msg.length      = 0;
        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);

        if (msg.instruction == NetOps.se_CommitTransactionFailed)    // CommitFailed
        {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction == NetOps.se_ErrorResponse) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction != NetOps.se_CommitTransactionOk) {
            throw new DriverException(ErrorCodes.SE3008, "");
        }
    }

    public SednaStatement createStatement() throws DriverException {
        if (isClose()) {
            throw new DriverException(ErrorCodes.SE5500, "");
        }

        SednaStatement st = new SednaStatementImpl(this.outputStream,
                this.bufInputStream,
                this.doTraceOutput);

        return st;
    }

    public void rollback() throws DriverException {
        if (this.isClose) {
            throw new DriverException(ErrorCodes.SE3028, "");
        }

        NetOps.Message msg = new NetOps.Message();

        msg.instruction = NetOps.se_RollbackTransaction;
        msg.length      = 0;
        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);

        if (msg.instruction == NetOps.se_RollbackTransactionFailed)    // RollbackFailed
        {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction == NetOps.se_ErrorResponse) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction != NetOps.se_RollbackTransactionOk) {
            throw new DriverException(ErrorCodes.SE3008, "");
        }
    }

    public void setTraceOutput(boolean trace) throws DriverException {
        this.doTraceOutput = trace;
    }

    public void setDebugMode(boolean debug) throws DriverException {
        if (this.isClose) {
            throw new DriverException(ErrorCodes.SE3028, "");
        }
        NetOps.Message msg = new NetOps.Message();

        msg.instruction = NetOps.se_SetSessionOptions;
        msg.length      = 9;

        if(debug){
            NetOps.writeInt(NetOps.se_Session_Debug_On, msg.body, 0); // option type
        } else {
            NetOps.writeInt(NetOps.se_Session_Debug_Off, msg.body, 0); //option type
        }
        msg.body[4] = 0; //option value string type
        NetOps.writeInt(0, msg.body, 5); // option value length

        NetOps.writeMsg(msg, outputStream);
        NetOps.readMsg(msg, bufInputStream);


        if (msg.instruction == NetOps.se_ErrorResponse) {
            throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
        } else if (msg.instruction != NetOps.se_SetSessionOptionsOk) {
            throw new DriverException(ErrorCodes.SE3008, "");
        }

    }

    public boolean isClose() {
        return this.isClose;
    }

    void setBIS(BufferedInputStream bufInputStream) {
        this.bufInputStream = bufInputStream;
    }

    void setOS(OutputStream outputStream) {
        this.outputStream = outputStream;
    }

    void setSocket(Socket s) {
        this.socket = s;
    }
}
