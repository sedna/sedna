/*
 * File:  SednaConnectionImpl.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;
import java.net.Socket;

/**
 * Implementation of the {@link ru.ispras.sedna.driver.SednaConnection} for the
 * client/server protocol 3.0
 */
class SednaConnectionImpl implements SednaConnection {

    private boolean       isClose       = false;
    private boolean       doTraceOutput = true;
    private Socket        socket;
    BufferedInputStream   bufInputStream;
    OutputStream          outputStream;

    /* Note, don't use enum to save compatibility with old Java */
    private static final class SessionOption {
        static public SessionOption SEDNA_DEBUG_MODE    = new SessionOption(NetOps.se_Session_Debug_On, NetOps.se_Session_Debug_Off);
        static public SessionOption SEDNA_READONLY_MODE = new SessionOption(NetOps.se_Session_Readonly_On, NetOps.se_Session_Readonly_Off);

        private final int onCode;
        private final int offCode;

        private SessionOption(int on, int off) {
            this.onCode = on;
            this.offCode = off;
        }

        public int getOnCode() {
            return onCode;
        }
        public int getOffCode() {
            return offCode;
        }
    }

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
            } catch (IOException ignore) {

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

        return new SednaStatementImpl(this.outputStream,
                this.bufInputStream,
                this.doTraceOutput);
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

    /**
     * Internal helper to set all session options within one place
     * @param option session option to set, must be one of the statically defined in
     * {@link ru.ispras.sedna.driver.SednaConnectionImpl.SessionOption} class
     * @param mode either <code>true</code> to turn option on or <code>false</code>
     * to turn it off
     * @throws DriverException in case of unknown option or some other error
     */
    private void setSessionOption(SessionOption option, boolean mode) throws DriverException {
        if(option == SessionOption.SEDNA_DEBUG_MODE || option == SessionOption.SEDNA_READONLY_MODE) {
            if (this.isClose)
                throw new DriverException(ErrorCodes.SE3028, "");

            NetOps.Message msg = new NetOps.Message();

            msg.instruction = NetOps.se_SetSessionOptions;
            /* total set-option message length */
            msg.length      = 9;

            if(mode) NetOps.writeInt(option.getOnCode(),  msg.body, 0);
            else     NetOps.writeInt(option.getOffCode(), msg.body, 0);

            /* option value string type */
            msg.body[4] = 0;
            /* option value length */
            NetOps.writeInt(0, msg.body, 5);

            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);

            if (msg.instruction == NetOps.se_ErrorResponse) {
                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
            } else if (msg.instruction != NetOps.se_SetSessionOptionsOk) {
                throw new DriverException(ErrorCodes.SE3008, "code returned: " + msg.instruction);
            }
        } else {
            throw new DriverException(ErrorCodes.SE4619, "");
        }
    }



    public void setDebugMode(boolean debug) throws DriverException {
        setSessionOption(SessionOption.SEDNA_DEBUG_MODE, debug);
    }

    public void setReadonlyMode(boolean debug) throws DriverException {
        setSessionOption(SessionOption.SEDNA_READONLY_MODE, debug);
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
