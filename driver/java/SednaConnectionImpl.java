/*
 * File:  SednaConnectionImpl.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.net.Socket;
import java.io.*;
import java.util.*;

class SednaConnectionImpl implements SednaConnection
{
  private Integer id;
  private Socket socket;
  OutputStream outputStream;
  BufferedInputStream bufInputStream;
   
  private boolean isClose = false; 
 
  SednaSerializedResult currentResult = null; // all statements that were created in this connection

//sets session id (for driver-internal use)  
  void setId(Integer id)
  {
  	this.id = id;
  }
//gets session id (for driver-internal use)  
  Integer getId()
  {
  	return this.id;
  }
//sets socket (for driver-internal use)  
  void setSocket(Socket s)
  {
  	this.socket = s;
  }
//gets socket (for driver-internal use)  
  Socket getSocket()
  {
  	return this.socket;
  }
  
//sets OutputStream of the connections socket (for driver-internal use)  
  void setOS(OutputStream outputStream)
  {
  	this.outputStream = outputStream;
  }
  
//sets DataInputStream of the connections socket (for driver-internal use)  
  void setBIS(BufferedInputStream bufInputStream)
  {
  	this.bufInputStream = bufInputStream;
  }

// begins transaction; 1 - success, 0 - error
  public void begin() throws DriverException
  {
  	 NetOps.Message msg = new NetOps.Message();
  	 try
  	 {
      NetOps.driverPrintOut("\nBeginning transaction...");
      
      msg.instruction = 210;// BeginTransaction
      msg.length = 0;
  	  NetOps.writeMsg(msg, outputStream);
  	  
      NetOps.readMsg(msg, bufInputStream);
      
      if (msg.instruction == 230)
      {
          NetOps.driverPrintOut(" OK\n");
      }
      else if (msg.instruction == 240)
      {
    //    NetOps.driverPrintOut(" Failed\n"+(NetOps.getErrorInfo(msg.body)));
          throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));      
      }
      else if (msg.instruction == 100)
      {
    //    NetOps.driverPrintOut(" Failed\n"+(NetOps.getErrorInfo(msg.body)));
          throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));      
      }
      else throw new DriverException("Unknown message from server");      
     }
     catch(IOException e)
     {
      NetOps.driverErrOut("Error while IO to server: "+e.toString());
       throw new DriverException("Error while IO to server: "+e.toString());
     }
  } 
// closes connection (exits connection process on server)  
  public void close() throws DriverException
  {
  	 NetOps.Message msg = new NetOps.Message();
 	
  	try
  	{
      NetOps.driverPrintOut("\nClosing session...");
      msg.instruction = 500; //CloseConnection
      msg.length = 0;
 	  NetOps.writeMsg(msg, outputStream);
 	  
 	  
      NetOps.readMsg(msg, bufInputStream);
      
      if (msg.instruction == 520) //TransactionRollBack
      {
      	throw new DriverException("Transaction rollback. Session is closed.");
      }
      else if (msg.instruction == 510) //CloseSessionOk
      {
      	NetOps.driverPrintOut( "OK\n");
      } 
      else if (msg.instruction == 100) //ErrorResponse
      {
        throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));	
      }
      else throw new DriverException("Unknown message from server");      
    }
    
    catch(IOException e)
    {
      NetOps.driverErrOut("Error while IO to server: "+e.toString());
      this.isClose = true;
      throw new DriverException("Error while IO to server: "+e.toString());	
    }
   this.isClose = true;
   
  }
  public SednaStatement createStatement() throws DriverException
  {
    if (isClose()) throw new DriverException ("Creating a statement on a closed connection");
    NetOps.driverPrintOut("Creating  statement...");
  	SednaStatement st = new SednaStatementImpl(this.outputStream, this.bufInputStream, this.currentResult);
  	NetOps.driverErrOut("OK\n");
  	return st;
  }
  
  public void commit() throws DriverException
  {
  	 NetOps.Message msg = new NetOps.Message();
  	
    
    NetOps.driverPrintOut("Commiting transaction...");  	
  	try
  	{
  	msg.instruction = 220;
  	msg.length = 0;	
    NetOps.writeMsg(msg, outputStream);

    NetOps.readMsg(msg, bufInputStream);
    
    if (msg.instruction == 260) // CommitFailed
    {
      throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));	
    }
    
    else if (msg.instruction == 250) //CommitSucceeded
    {
      NetOps.driverPrintOut(" OK\n");	
    }
 
    else if (msg.instruction == 100)
    {
      NetOps.driverPrintOut(" Error\n");	
      throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));	    
    }
    
    else throw new DriverException("Unknown instruction from server");
    
    }
    catch(IOException e)
    {
     NetOps.driverErrOut(e.toString()); 
     throw new DriverException(e.toString());
    }
  }

  public void rollback() throws DriverException
  {
  	 NetOps.Message msg = new NetOps.Message();
  	
    
    NetOps.driverPrintOut("Rollback transaction...");  	
  	try
  	{
  	msg.instruction = 225;
  	msg.length = 0;	
    NetOps.writeMsg(msg, outputStream);

    NetOps.readMsg(msg, bufInputStream);
    
    if (msg.instruction == 265) // RollbackFailed
    {
      throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));	
    }
    
    else if (msg.instruction == 255) //RollbackSucceeded
    {
      NetOps.driverPrintOut(" OK\n");	
    }
 
    else if (msg.instruction == 100)
    {
      NetOps.driverPrintOut(" Error\n");	
      throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));	    
    }
    
    else throw new DriverException("Unknown instruction from server");
    
    }
    catch(IOException e)
    {
     NetOps.driverErrOut(e.toString()); 
     throw new DriverException(e.toString());
    }
  }

  
  public boolean isClose()
  {
  	return this.isClose;
  }
  
 
}