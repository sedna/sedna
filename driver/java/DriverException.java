
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
 * <code>DriverException</code> provides debug information when query fails. 
 * For example:
 *
 * <code>
 *    SednaConnection con = DatabaseManager.getConnection("localhost", "x", "SYSTEM", "MANAGER");
 *      con.begin();
 *      SednaStatement st1 = con.createStatement();
 *      
 *      // execute XQuery query.
 *      boolean call_res = st1.execute("doc(\"region\")/regions/*");
 *           
 *      // if call_res is true the statement was not an update
 *      // and we can use SednaSerializedResult object
 *      if (call_res)    
 *      {
 *          System.out.println("Result:");
 *          SednaSerializedResult pr1 = st1.getSerializedResult();
 *
 *          item  = pr1.next();
 *
 *          while (item != null) {
 *              System.out.println(item);
 *              System.out.println("=====================================\n");
 *              item = pr1.next();
 *          }
 *      }
 *    
 *      con.close();
 * </code>
 *
 * @return debug inforamation as a string.
 * @see SednaConnection#setDebugMode
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
