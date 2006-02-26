/*
 * File:  SednaStatementImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.net.Socket;
import java.io.*;
import java.lang.System;

class SednaStatementImpl implements SednaStatement
{
  private OutputStream outputStream;
  private BufferedInputStream bufInputStream;
  private SednaSerializedResultImpl serializedResult = null;
  private SednaSerializedResult currentResult = null;
  
  SednaStatementImpl(OutputStream outputStream, BufferedInputStream bufInputStream, SednaSerializedResult currentResult)
  {
    this.outputStream = outputStream;
    this.bufInputStream = bufInputStream;
    this.currentResult = currentResult;
  }
  
// @param - query as a String  
// returns true if PlaneResult of this Statement is a result of a query
// false if it was Update
// throws Exception if there were errors
  
  public boolean execute(String queryText) throws DriverException
  {
 	
 	NetOps.Message msg = new NetOps.Message();

  try
  {
  		
    NetOps.driverPrintOut("Executing query ...");
    byte query_bytes[] = queryText.getBytes("utf8");
    
  
	if(query_bytes.length > NetOps.SOCKET_MSG_BUF_SIZE - 6)
	{
	  int bytes_sent = 0;
	  while(bytes_sent < query_bytes.length)
	  {
		  msg.instruction = 301; //ExecuteLong
		  msg.length = NetOps.SOCKET_MSG_BUF_SIZE; // body containes: result format (sxml=1 or xml=0) - 1 byte)
		  // string format - 1 byte;
		  // string length - 4 bytes
		  // string
    
		  // writing queryText	
		  msg.body[0] = 0; // result format code
		  msg.body[1] = 0; // string format
		  NetOps.writeInt(NetOps.SOCKET_MSG_BUF_SIZE-6, msg.body, 2);
 
		  if((query_bytes.length - bytes_sent) > (NetOps.SOCKET_MSG_BUF_SIZE-6))
		  {
			  System.arraycopy((Object)query_bytes, bytes_sent, (Object)msg.body, 6, NetOps.SOCKET_MSG_BUF_SIZE-6);          	
			  bytes_sent += NetOps.SOCKET_MSG_BUF_SIZE-6;
		  }
		  else
		  {
			  System.arraycopy((Object)query_bytes, bytes_sent, (Object)msg.body, 6, (query_bytes.length - bytes_sent));          	
    		  msg.length = (query_bytes.length - bytes_sent)+6; // body containes: result format (sxml=1 or xml=0) - 1 byte)
			  bytes_sent += (query_bytes.length - bytes_sent);
		  }

		  NetOps.writeMsg(msg, outputStream);
	  }
  	  msg.instruction = 302; //LongQueryEnd
	  msg.length = 0; 
   	  NetOps.writeMsg(msg, outputStream);
	}
	else
	{
	   msg.instruction = 300; //Execute
	   msg.length = query_bytes.length + 6; // body containes: result format (sxml=1 or xml=0) - 1 byte)
	  // string format - 1 byte;
	  // string length - 4 bytes
	  // string
    
	  // writing queryText	
	  msg.body[0] = 0; // result format code
	  msg.body[1] = 0; // string format
	  NetOps.writeInt(query_bytes.length, msg.body, 2);
 
 	  System.arraycopy((Object)query_bytes, 0, (Object)msg.body, 6, query_bytes.length);          	

	  NetOps.writeMsg(msg, outputStream);
	}

	NetOps.readMsg(msg, bufInputStream);
    
	if (msg.instruction == 320) //QuerySucceeded
    {
       NetOps.driverPrintOut("Query succeeded\n");
       
       NetOps.String_item sitem = NetOps.readStringItem(bufInputStream);
       this.serializedResult = new SednaSerializedResultImpl(sitem.item, this.bufInputStream, this.outputStream);

  //     blockPrevResult();
       
       return true;
    }

    else if (msg.instruction == 330) //QueryFailed
    {
       NetOps.driverPrintOut("Query failed\n");
       throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
    }
    
    else if (msg.instruction == 340) //UpdateSucceeded
    {
       NetOps.driverPrintOut("Update Succeeded\n");
   //    blockPrevResult();
       
   	   return false;
    }
    
    else if (msg.instruction == 350) //UpdateFailed
    {
       NetOps.driverPrintOut("Update Failed\n");
 //      blockPrevResult();
       
       throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
    }
    
    else if (msg.instruction == 430) //BulkLoadFileName
    {
    	NetOps.driverPrintOut("Bulk Load\n");
    	String file_name = new String(msg.body, 5, msg.length-5);
        NetOps.driverPrintOut("Data from file "+file_name+" \n");
 
        FileInputStream fis = new FileInputStream(file_name);
         
        boolean res = NetOps.bulkLoad(fis, this.bufInputStream, this.outputStream);
 //      blockPrevResult();

        return res;            //bulk loading is update...	
    }

    else if (msg.instruction == 431) //BulkLoadFromStream
    {
        NetOps.driverPrintOut("Bulk Load from STDIN\n"); 
        boolean res = NetOps.bulkLoad(System.in, this.bufInputStream, this.outputStream);
 //      blockPrevResult();
        return res; //bulk loading is update...	
    }
    
    else if (msg.instruction == 100) //ErrorResponse
    {
    	NetOps.driverPrintOut("Error\n");
    	throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
    }
    throw new DriverException("Unknown error while execute"); 
    
    }
    catch(Exception e)
    {
    throw new DriverException(e.toString());	
    }

  }

// @param - Reader to take the query from  
// returns true if PlaneResult of this Statement is a result of a query
// false if it was Update
// throws Exception if there were errors
  
  public boolean execute(InputStream in) throws DriverException, IOException
  {
  	// constructs the query string
  	StringBuffer strBuf = new StringBuffer();
  	NetOps.Message msg = new NetOps.Message();
  	
    try
    {
  	  NetOps.driverPrintOut("Executing query ...");
      int call_res = 1;
  	  while(call_res > 0) // 0 - array is full; -1 - end of stream or error
  	  {
  	      call_res = in.read(msg.body, 6, NetOps.SOCKET_MSG_BUF_SIZE-6);
		  if(call_res > 0)
		  {
			  msg.instruction = 301; //Execute
			  msg.length = call_res + 6; // body containes: result format (sxml=1 or xml=0) - 1 byte)
			  // string format - 1 byte;
			  // string length - 4 bytes
			  // string
    
			  // writing queryText	
			  msg.body[0] = 0; // result format code
			  msg.body[1] = 0; // string format

			  NetOps.writeInt(call_res, msg.body, 2);
			  NetOps.writeMsg(msg, outputStream);
		  }
      }
      msg.instruction = 302; //Execute
	  msg.length = 0; // body containes: result format (sxml=1 or xml=0) - 1 byte)
 	  NetOps.writeMsg(msg, outputStream);

      NetOps.readMsg(msg, bufInputStream);
  	
      if (msg.instruction == 320) //QuerySucceeded
      {
         NetOps.driverPrintOut(" Query succeeded\n");
       
         NetOps.String_item sitem = NetOps.readStringItem(bufInputStream);
         this.serializedResult = new SednaSerializedResultImpl(sitem.item, this.bufInputStream, this.outputStream);
  //     blockPrevResult();
         return true;
      }
      else if (msg.instruction == 330) //QueryFailed
      {
         NetOps.driverPrintOut("Query failed\n");
  //     blockPrevResult();
         throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
      }
      else if (msg.instruction == 340) //UpdateSucceeded
      {
         NetOps.driverPrintOut("Update Succeeded\n");
  //     blockPrevResult();
   	     return false;
      }
      else if (msg.instruction == 350) //UpdateFailed
      {
         NetOps.driverPrintOut("Update Failed\n");
 //      blockPrevResult();
         throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
      }
      else if (msg.instruction == 430) //BulkLoadFileName
      {
      	 NetOps.driverPrintOut("Bulk Load\n");
    	 String file_name = new String(msg.body, 5, msg.length-5);
         NetOps.driverPrintOut("Data from file "+file_name+" \n");
        
         FileInputStream fis = new FileInputStream(file_name);
         
         boolean res = NetOps.bulkLoad(fis, this.bufInputStream, this.outputStream);
  //      blockPrevResult();

        return res; //bulk loading is update...	
      }
      else if (msg.instruction == 431) //BulkLoadFromStream
      {
         NetOps.driverPrintOut("Bulk Load from STDIN\n"); 
         boolean res = NetOps.bulkLoad(System.in, this.bufInputStream, this.outputStream);
  //      blockPrevResult();

         return res; //bulk loading is update...	
      }
     else if (msg.instruction == 100) //ErrorResponse
     {
      	 NetOps.driverPrintOut("Error\n");
  //      blockPrevResult();
    	
    	 throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
     }
     throw new DriverException("Unknown error while execute"); 
    
    }
    catch(Exception e)
    {
    throw new DriverException(e.toString());	
    }
  }

  public void loadDocument(InputStream in, String doc_name) throws DriverException, IOException
  {
  	String queryText = "LOAD STDIN \""+doc_name+"\"";
  
    NetOps.Message msg = new NetOps.Message();

  		
//  		System.out.print("Executing Lite ...");
      NetOps.driverPrintOut("Executing bulk load ...");
    
      msg.instruction = 300; //Execute
      msg.length = queryText.length() + 6; // body containes: result format (sxml=1 or xml=0) - 1 byte)
                                         // string format - 1 byte;
                                         // string length - 4 bytes
                                         // string
    
     // writing queryText	
      msg.body[0] = 0; // result format code
      msg.body[1] = 0; // string format
      NetOps.writeInt(queryText.length(), msg.body, 2);
            	
      byte query_bytes[] = queryText.getBytes();
            	
      for(int i = 0; i < queryText.length(); i++)
      {
       msg.body[6+i] = query_bytes[i];	
      }

  	  NetOps.writeMsg(msg, outputStream);

  	  NetOps.readMsg(msg, bufInputStream);

      if (msg.instruction == 431) //BulkLoadFromStream
      {
        NetOps.driverPrintOut(" Bulk Load from stream\n");
        boolean res = NetOps.bulkLoad(in, this.bufInputStream, this.outputStream);
        //blockPrevResult();
        if (res) throw new DriverException("Bulk Load failed");
      }
      else if (msg.instruction == 100) //ErrorResponse
      {
         throw new DriverException("Bulk Load failed. "+NetOps.getErrorInfo(msg.body, msg.length));
      }
      else if (msg.instruction == 330) //QueryFailed
      {
         throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
      }
      
      else
      {
      	throw new DriverException("Unknown error");
      }
  }  
  
  public void loadDocument(InputStream in, String doc_name, String col_name) throws DriverException, IOException
  {
  	loadDocument(in, doc_name + "\" \"" + col_name);
  }

  public SednaSerializedResult getSerializedResult()
  {
  	return this.serializedResult;
  }


/*  void blockPrevResult()
  {
  	if (NetOps.currentStatement!=null){System.out.println("Driver: in blockPrevResult(): set to false current statement\n");  
  		 if(((SednaStatementImpl)NetOps.currentStatement).serializedResult!=null)
  		 ((SednaSerializedResultImpl)((SednaStatementImpl)NetOps.currentStatement).serializedResult).hasNextItem = false;
  }
if(this.serializedResult.hasNextItem) System.out.println("Driver: in blockPrevResult(): this.hasNextItem = true\n");  
  	NetOps.currentStatement = this;
if(this.serializedResult.hasNextItem) System.out.println("Driver: in blockPrevResult(): after setting new result this.hasNextItem = true\n");  

  }*/
  
}