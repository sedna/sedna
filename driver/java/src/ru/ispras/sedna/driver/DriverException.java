/*
 * File:  DriverException.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

package ru.ispras.sedna.driver;

/**
 * Instance of this exception is thrown when some
 * error occured while working with Sedna.
 */
public class DriverException extends Exception {

    /* One of the error code defined in ./kernel/common/errdbg/errror.code */
    private int error_code;

    /* Debug information returned by server */
    private String debugInfo;

    /**
     * Constructs a new DriverException with the specified error_code
     * and defined main error message.
     *
     * @param error_msg    basic error message.
     * @param error_code   error code.
     */
    public DriverException(String error_msg, int error_code) {

        super(error_msg);
        this.error_code = error_code;
        this.debugInfo = null;
    }

    /**
     * Constructs a new DriverException with the specified details message
     * and defined error code.
     *
     * @param error_code  one of the error codes.
     * @param details     exception details.
     */
    public DriverException(int error_code, String details) {
        super(buildDescription(error_code, details));
        this.error_code = error_code;
    }
    /**
     *  Returns the error code
     */
    public int getErrorCode() {
        return this.error_code;
    }

    /**
     * Returns the error message.
     */
    public String getErrorMessage() {
        return this.toString();
    }

    /**
     * When session debug mode is on (see Sedna Programmer's Guide for details on Sedna's debug facilities)
     * {@link #DriverException} provides debug information when query fails.
     *
     * @return  debug inforamation as a string.
     * @see     SednaConnection#setDebugMode
     */
    public String getDebugInfo() {
        return this.debugInfo;
    }

    /**
     * Used for the internal needs.
     */
    protected void setDebugInfo(StringBuffer debugInfo) {
        this.debugInfo = debugInfo.toString();
    }

    /**
     * Constructs a full description for exception.
     *
     * @param error_code   one of the error codes defined in Sedna
     * @param details      error details
     * @return             full description of the error which
     *                     then is provided to the client
     */
    protected static String buildDescription(int error_code, String details) {
        StringBuilder message = new StringBuilder(200);
        message.append("SEDNA Message: ERROR ")
                .append(ErrorCodes.user_error_code_entry[error_code][0])
                .append("\n")
                .append(ErrorCodes.user_error_code_entry[error_code][1]);

        if (details != null && !details.isEmpty())
            message.append(" (" + details + ")");
        return message.toString();
    }
}
