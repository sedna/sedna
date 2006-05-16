/*
 * File:  DriverException.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

/**
 * Exception class for internal goals
 */
 
public class DriverException extends Exception {

final static String SE3003 = "SEDNA Message: ERROR SE3003\n Failed to connect the host specified.";
final static String SE3008 = "SEDNA Message: ERROR SE3008\n Unknown message from server.";
final static String SE3012 = "SEDNA Message: ERROR SE3012\n Message length exceeds available size.";
final static String SE3015 = "SEDNA Message: ERROR SE3015\n Too long login/password/db_name.";
final static String SE3053 = "SEDNA Message: ERROR SE3053\n Authentication failed.";
final static String SE5500 = "SEDNA Message: ERROR SE5500\n An attempt to create a statement on a closed connection.";
final static String SE5501 = "SEDNA Message: ERROR SE5501\n Result item cannot be returned as a String because it is too large.";
final static String SE3007 = "SEDNA Message: ERROR SE3007\n Failed to recieve a message.";
final static String SE3006 = "SEDNA Message: ERROR SE3006\n Failed to send a message.";
final static String SE5502 = "SEDNA Message: ERROR SE5502\n Unsupported encoding.";
final static String SE4042 = "SEDNA Message: ERROR SE4042\n Can't open file.";
final static String SE3028 = "SEDNA Message: ERROR SE3028\n Connection with server is closed or have not been established yet.";

  public DriverException(String msg)
  {
		super(msg);
  	  	
  }
  public DriverException(String msg, String details)
  {
		super(msg + "\nDetails: " + details);
  }	

}
    
