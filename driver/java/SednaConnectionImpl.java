
/*
 * File:  SednaConnectionImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

import java.net.Socket;

import java.util.*;

//~--- classes ----------------------------------------------------------------

class SednaConnectionImpl implements SednaConnection {
    private boolean       isClose       = false;
    SednaSerializedResult currentResult = null;    // all statements that were created in this connection
    BufferedInputStream bufInputStream;
    private Integer     id;
    OutputStream        outputStream;
    private Socket      socket;

    //~--- methods ------------------------------------------------------------

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

    // closes connection (exits connection process on server)  
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
                                this.bufInputStream, this.currentResult);

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

    //~--- get methods --------------------------------------------------------

    // gets session id (for driver-internal use)  
    Integer getId() {
        return this.id;
    }

    // gets socket (for driver-internal use)  
    Socket getSocket() {
        return this.socket;
    }

    public boolean isClose() {
        return this.isClose;
    }

    //~--- set methods --------------------------------------------------------

    // sets DataInputStream of the connections socket (for driver-internal use)  
    void setBIS(BufferedInputStream bufInputStream) {
        this.bufInputStream = bufInputStream;
    }

    // sets session id (for driver-internal use)  
    void setId(Integer id) {
        this.id = id;
    }

    // sets OutputStream of the connections socket (for driver-internal use)  
    void setOS(OutputStream outputStream) {
        this.outputStream = outputStream;
    }

    // sets socket (for driver-internal use)  
    void setSocket(Socket s) {
        this.socket = s;
    }
}
