/*
 * File:  DriverException.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

/**
 * Signals that driver exception of some sort has been occurred.
 * This class is the general class of exceptions produced by failed
 * operation with Sedna server.
 */
public class DriverException extends Exception {

    /* One of the error code defined in ./kernel/common/errdbg/errror.code */
    private int errorCode;
    /* Debug information returned by server */
    private String debugInfo;

    /**
     * Constructs a new DriverException with the specified error_code
     * and defined main error message.
     * @param message    basic error message.
     * @param errorCode error code.
     */
    public DriverException(String message, int errorCode) {

        super(message);
        this.errorCode = errorCode;
        this.debugInfo = null;
    }

    /**
     * Constructs a new DriverException with the specified details message
     * and defined error code.
     * @param errorCode   one of the error codes.
     * @param details     exception details.
     */
    public DriverException(int errorCode, String details) {
        super(buildDescription(errorCode, details));
        this.errorCode = errorCode;
    }
    /**
     *  @return error code
     */
    public int getErrorCode() {
        return this.errorCode;
    }

    /**
     * @return the detail message string of the error occurred.
     */
    public String getErrorMessage() {
        return this.toString();
    }

    /**
     * When session debug mode is on (see Sedna Programmer's Guide for details on Sedna's debug facilities)
     * provides debug information when query fails.
     *
     * @return  debug information as a string.
     * @see     SednaConnection#setDebugMode
     */
    public String getDebugInfo() {
        return this.debugInfo;
    }

    /**
     * Used for the internal needs.
     * @param debugInfo text of the debug information
     */
    protected void setDebugInfo(StringBuffer debugInfo) {
        this.debugInfo = debugInfo.toString();
    }

    /**
     * Constructs a full description for exception.
     * @param errorCode    one of the error codes
     * @param details      error details
     * @return             full description of the error which
     *                     then is provided to the client
     */
    protected static String buildDescription(int errorCode, String details) {
        StringBuffer message = new StringBuffer(200);
        message.append("SEDNA Message: ERROR ")
                .append(ErrorCodes.user_error_code_entry[errorCode][0])
                .append("\n")
                .append(ErrorCodes.user_error_code_entry[errorCode][1]);

        if (details != null && details.length() != 0)
            message.append(" (" + details + ")");
        return message.toString();
    }
}
