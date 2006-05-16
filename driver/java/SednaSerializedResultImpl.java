/*
 * File:  SednaSerializedResultImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.io.*;

class SednaSerializedResultImpl implements SednaSerializedResult
{
 StringBuffer stringItem;
 boolean hasNextItem;
 BufferedInputStream bufInputStream;
 OutputStream outputStream;
 
 SednaSerializedResultImpl(StringBuffer stringItem, BufferedInputStream is, OutputStream os)
 {
 	this.bufInputStream = is;
 	this.outputStream = os;
 	this.stringItem = stringItem;
 	this.hasNextItem = true;
 }	

// returns string item if success
// null if the end of the sequence
// throws exception if errors
public String next() throws DriverException
 {
	 String tmpItem;	
  	 NetOps.Message msg = new NetOps.Message();
	 if (this.hasNextItem == true)
	 {
	 	try
	  	{
	  		tmpItem = this.stringItem.toString();
	  		msg.instruction = NetOps.se_GetNextItem;
	  		msg.length = 0;
	  
  	  		NetOps.writeMsg(msg, outputStream);
            NetOps.String_item sitem = NetOps.readStringItem(bufInputStream);

      		if (sitem.item == null)
      		{
         		hasNextItem = false;
         		this.stringItem = null;
       	 		return tmpItem;
      		} 
			else 
      		{
         		this.stringItem = sitem.item;
         		this.hasNextItem = sitem.hasNextItem;
         		return tmpItem;
      		}
     	}
     	catch(OutOfMemoryError e)
     	{
        	throw new DriverException("Can't return item as a string because item is too large.");
     	}
     	catch(DriverException e)
     	{
        	NetOps.driverErrOut(e.toString()+"\n");
        	throw e;
     	}
    }
    else 
	    return null;
 }

//  returns 0 - if success,
//  -1 - if end of the sequence 
//  throws exception if errors
public int next(Writer writer) throws DriverException
{
  	 NetOps.Message msg = new NetOps.Message();

  	 if (this.hasNextItem == true)
	 {
	 	try
	  	{	 
   	  		writer.write(this.stringItem.toString());
	  	 	msg.instruction = NetOps.se_GetNextItem;
	  		msg.length = 0;
	  
  	  		NetOps.writeMsg(msg, outputStream);
		    NetOps.String_item sitem = NetOps.readStringItem(bufInputStream);
		    if (sitem.item == null) 
      		{
       			hasNextItem = false;
        		this.stringItem = null;
        		return 0;
      		}
      		else
      		{
        		this.stringItem = sitem.item;
       			this.hasNextItem = sitem.hasNextItem;
        		return 0;
      		}
     	}
     	catch(IOException e)
     	{
        	throw new DriverException("Input/Output error while getting next item");
     	}
     	catch(DriverException e)
     	{
        	NetOps.driverErrOut(e.toString()+"\n");
        	throw e;
     	}
    	catch(OutOfMemoryError e)
   	    {
   	 	    throw new DriverException(DriverException.SE5501);
   	    }
     }
     else return (-1);
 }
}