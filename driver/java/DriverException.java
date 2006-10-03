
/*
 * File:  DriverException.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

/**
 * Exception class for internal goals
 */
public class DriverException extends Exception {
	
	private int error_code;
    //~--- constructors -------------------------------------------------------

    public DriverException(String error_msg, int error_code) {
       
        super(error_msg);
        this.error_code = error_code ;
    }

    public DriverException(int error_code, String details) {
    	
      	super(new String("SEDNA Message: ERROR " + 
    	                  ru.ispras.sedna.driver.ErrorCodes.user_error_code_entry[error_code][0] + 
    	                  "\n" +
    	                  ErrorCodes.user_error_code_entry[error_code][1] +
    	                  details));
		this.error_code = error_code;    	                  
    }
    
    public int getErrorCode()
    {
    	return this.error_code;
    }
    public String getErrorMessage()
    {
    	return this.toString();
    }
}
