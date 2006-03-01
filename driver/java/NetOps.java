/*
 * File:  NetOps.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

/*
 * Copyright (C) ISPRAS MODIS 2003-2004. All rights reserved.
 * $Id: NetOps.java,v 1.19 2006/01/31 17:27:52 masha Exp $
 * $Author: masha $
 */


package ru.ispras.sedna.driver;

import java.net.Socket;
import java.io.*;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;
import java.nio.*;
import java.lang.*;
import java.nio.charset.*;

/**
 * Static functions to organize message exchange between 
 * client application and kernel
 */
class NetOps {
	
static class Message {
		
		int instruction;
		int length;
		byte body[];
		Message()
		{
        this.body = new byte[SOCKET_MSG_BUF_SIZE];
        }
}

static class String_item{
		
		String item;
		boolean hasNextItem;
}

static Object currentStatement = null;
    /**
     * Size of bulk load portion
     */
    final static int BULK_LOAD_PORTION = 5120;
    final static int SOCKET_MSG_BUF_SIZE = 10240;
    
    static byte int_array[] = new byte[4];

    static void readMsg(Message msg, BufferedInputStream bufInputStream) throws DriverException, IOException
    {
    	int call_res;
	
    	    msg.instruction = NetOps.readInt(bufInputStream);
       	    msg.length = NetOps.readInt(bufInputStream);

            if(msg.length > SOCKET_MSG_BUF_SIZE) throw new DriverException("Message length exceeds available size");
            
            if (msg.length!=0)
            {
             int count = 0;
             int pos = 0;
             while(pos < msg.length) 
             {
              count = bufInputStream.read(msg.body, pos, msg.length-pos);
              
              if (count != -1) 
              pos += count;
             }
            }
       
    }
    
    static void writeMsg(Message msg, OutputStream outputStream) throws DriverException, IOException
    {
    	if (msg.length > SOCKET_MSG_BUF_SIZE) throw new DriverException("Message length exceeds available size");
    	
    	BufferedOutputStream bufOutputStream = new BufferedOutputStream(outputStream);
        BufferedWriter bufWriter = new BufferedWriter( new OutputStreamWriter(outputStream));
    	
    	NetOps.writeInt(msg.instruction, bufOutputStream);
    	NetOps.writeInt(msg.length, bufOutputStream);
        if (msg.length != 0)
        {
    	bufOutputStream.write(msg.body, 0, msg.length);
        }
    	bufOutputStream.flush();
    }


    /**
     * Loads data from stream to Server via sockets
     * false - if bulk Load (it is update) succeeded
     * true - if failed
     */ 
    static boolean bulkLoad(InputStream in, BufferedInputStream bufInputStream, OutputStream outputStream) throws DriverException
    {
   	  NetOps.Message msg = new NetOps.Message();
      int bytes_read;

      bytes_read = 0; 
        
        try
        {
         while(bytes_read != -1)
	      {
	       bytes_read = in.read(msg.body, 5, BULK_LOAD_PORTION);	  
//           if(bytes_read == (-1)) throw new DriverException("Error while reading from file");
	       if (bytes_read != -1)   
	       { 	
           msg.instruction = 410; //BulkLoadPortion
           msg.length = bytes_read+5;
           msg.body[0] = 0;
           NetOps.writeInt(bytes_read, msg.body, 1);
   	       NetOps.writeMsg(msg, outputStream);
           }
           
          } 

     	  NetOps.Message msg2 = new NetOps.Message();
          
          msg2.instruction = 420; //BulkLoadEnd
          msg2.length = 0;
   	      NetOps.writeMsg(msg2, outputStream);
   	       
       	  //result of bulk loading
       	  NetOps.readMsg(msg, bufInputStream);
       	  if ((msg.instruction == 440)||(msg.instruction == 340))
       	  {
            return false;
       	  }
       	  else if ((msg.instruction == 450)||(msg.instruction == 350)||(msg.instruction == 100))
       	  {
   	        throw new DriverException(new String(msg.body, 9, msg.length-9));
       	  }
   	      return false;
         }
         catch(IOException e)
         {
      	   NetOps.driverErrOut("IOException\n");
           msg.instruction = 400; //BulkLoadError
      	   msg.length = 0;
      	   try{
      	   NetOps.writeMsg(msg, outputStream);
      	
    	   NetOps.readMsg(msg, bufInputStream);
   	          }
   	       catch(IOException ex)
   	       { throw new DriverException(" Input/Output error");}
   	       
   	       throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
         }

   }

/** Reads a whole item from the socket
 *  
 */
   static String_item readStringItem(BufferedInputStream is) throws IOException, DriverException
   {
  	 try{
  	 NetOps.Message msg = new NetOps.Message();
  	 NetOps.String_item sitem = new NetOps.String_item();
     ByteBuffer byteBuf;
     CharBuffer charBuf = CharBuffer.allocate(SOCKET_MSG_BUF_SIZE);
     CharsetDecoder csd = Charset.forName("utf8").newDecoder();
     StringBuffer strBuf = new StringBuffer();
     
   	 NetOps.readMsg(msg, is);
   	 if ((msg.instruction == 370)||(msg.instruction == 375)) //ItemEnd or ResultEnd
   	 {
   	 	sitem.hasNextItem = false;
   	 	sitem.item = null;
   	 	return sitem;
   	 }
   	 while((msg.instruction != 370)&&(msg.instruction != 375)) //ItemEnd or ResultEnd
   	 {  
   	    if (msg.instruction == 100) //ErrorResponse
   	 	throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
   	 	
   	 	if (msg.instruction == 360) //ItemPart
   	 	{ 
    	 	byteBuf = ByteBuffer.wrap(msg.body, 5, msg.length-5);
    	 	csd.decode(byteBuf, charBuf, false);
    	   	strBuf.append(charBuf.flip());
    	   	charBuf.clear();
    	   	
  	    }
   	    NetOps.readMsg(msg, is);
   	    
   	 }
   	 if (msg.instruction == 375) sitem.hasNextItem = false;
   	 if (msg.instruction == 370) sitem.hasNextItem = true;

//   	 csd.flush(charBuf);
//     strBuf.append(charBuf);
   	 
   	 sitem.item = strBuf.toString();
   	 return sitem;
     }
   	 catch(OutOfMemoryError e)
   	 {
   	 	throw new DriverException("Item is too large. Can't get enough memory");
   	 }
   }


/** 
 *  Gets error message body and
 *  Makes a string that is error info (usually for DriverException)
 */

    static String getErrorInfo(byte [] body, int length)
    {
    	
     	return new String(body, 9, length-9);
    }
    
    static void writeInt(int i, BufferedOutputStream bufOutputStream) throws IOException
    {
      	bufOutputStream.write(0xff & (i >> 24));
    	bufOutputStream.write(0xff & (i >> 16));
    	bufOutputStream.write(0xff & (i >>  8));
    	bufOutputStream.write(0xff &  i );
    }
    
    static void writeInt(int i, byte[] byte_array, int pos)
    {
    	byte_array[pos] = (new Integer(0xff & (i >> 24))).byteValue();
    	byte_array[pos+1] = (new Integer(0xff & (i >> 16))).byteValue();
    	byte_array[pos+2] = (new Integer(0xff & (i >> 8))).byteValue();
    	byte_array[pos+3] = (new Integer(0xff & i)).byteValue();
    	
    }
    
    static int readInt(BufferedInputStream bufInputStream) throws IOException, DriverException
    {
      int call_res;
   
    	for(int i=0; i<4; i++)
    	{
        call_res = int_array[i] = (byte)bufInputStream.read();
//  	    if (call_res == -1) throw new DriverException("Error while read from socket");
     	}

      int  integer = (((int_array[0] & 0xff) << 24) | ((int_array[1] & 0xff) << 16) |
  ((int_array[2] & 0xff) << 8) | (int_array[3] & 0xff));

      return integer;
    }
    
    static void driverPrintOut(String str)
    {
    	if (Debug.DEBUG)
    	{
    		System.err.print(str);
    	}
    }
    
    static void driverErrOut(String str)
    {
    	if (Debug.DEBUG)
    	{
    		System.err.println(str);
    	}
    }   
}