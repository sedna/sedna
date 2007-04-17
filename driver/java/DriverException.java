
/*
 * File:  DriverException.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

/**
 * An exception thrown when some error occured while working with Sedna.
 */
public class DriverException extends Exception {
	
	private int error_code;
	private String debugInfo;
    //~--- constructors -------------------------------------------------------

    public DriverException(String error_msg, int error_code) {
       
        super(error_msg);
        this.error_code = error_code;
        this.debugInfo = null;
    }

    public DriverException(int error_code, String details) {
    	
      	super(new String("SEDNA Message: ERROR " + 
    	                  ru.ispras.sedna.driver.ErrorCodes.user_error_code_entry[error_code][0] + 
    	                  "\n" +
    	                  ErrorCodes.user_error_code_entry[error_code][1] +
    	                  details));
		this.error_code = error_code;    	                  
    }
/**
 *  Returns the error code.
 */
    public int getErrorCode()
    {
    	return this.error_code;
    }

/**
 * Returns the error message.
 */
    public String getErrorMessage()
    {
    	return this.toString();
    }

/**
 * When session debug mode is on (see Sedna Programmer’s Guide for details on Sedna's debug facilities) 
 * <code>DriverException</code> provides debug information. 
 * Method <code>getDebugInfo</code> returns debug inforamation.
 */
    public String getDebugInfo()
    {
    	return this.debugInfo;
    }
    
/**
 * Used for the internal needs.
 */
    protected void setDebugInfo(StringBuffer debugInfo)
    {
    	this.debugInfo = debugInfo.toString();
    }
}
