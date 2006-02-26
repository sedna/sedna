/*
 * File:  DatabaseManager.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.io.*;
import java.net.*;
import java.util.*;

public class DatabaseManager{

public static SednaConnection getConnection(String url_string, String db_name, String login, String password) throws DriverException
{
      InputStream inputStream;
      BufferedInputStream bufInputStream;    
      OutputStream outputStream;

      SednaConnectionImpl con = new SednaConnectionImpl();
      
      Socket socket = null;
      String url = "";
      int socket_port;
      
      try {
            NetOps.driverPrintOut("Trying to establish connection ...");
        
            if(url_string.indexOf(":")!=(-1))
            { 
            	url = url_string.substring(0,url_string.indexOf(":"));
            	socket_port = Integer.parseInt(url_string.substring(url_string.indexOf(":")+1, url_string.length()));
            }
            else 
            {
            	url = url_string;
            	socket_port = 5050;
            } 
         
//            if(url.equals("localhost")) url = InetAddress.getLocalHost().getHostName();
            if(url.equals("localhost")) url = "127.0.0.1"; //local ip address
            
        // open a socket connection
            socket = new Socket(/*InetAddress.getLocalHost()*/url, socket_port);
            if (socket!=null) 
            NetOps.driverPrintOut("socket connected to server\n\n");
            
            con.setSocket(socket);
       
          try {
           	outputStream = socket.getOutputStream();
            inputStream = socket.getInputStream();
            bufInputStream = new BufferedInputStream(inputStream, 1024);

            con.setOS(outputStream);
            con.setBIS(bufInputStream);

            // a message for listener        
            NetOps.Message msg = new NetOps.Message();
            msg.instruction = 110; //Start-Up
            msg.length = 0;
 //           String msg = "connection";
            NetOps.writeMsg(msg, outputStream);	
            
            NetOps.readMsg(msg, bufInputStream);
            if (msg.instruction == 140) //SendSessionParameters
            {
                NetOps.driverPrintOut("Openning session with session parameters: "+login+" "+db_name+"... ");
            	msg.instruction = 120;
            	//body contains:
                //major protocol version
                //minor protocol version
                // login string
                //dbname string            	
            	msg.length = 2+5+login.length()+5+db_name.length();
            	if (msg.length > NetOps.SOCKET_MSG_BUF_SIZE) throw new DriverException("Too long user name/db name");
            	int body_position = 0;
            
            // writing protocol version 1.0
                msg.body[body_position] = 1;
                msg.body[body_position+1] = 0;
                body_position +=2;
                
            // writing login	
            	msg.body[body_position] = 0; // format code
            	NetOps.writeInt(login.length(), msg.body, body_position+1);

            	byte login_bytes[] = login.getBytes();
            	body_position += 5;
//            	System.arraycopy(msg.body, body_position, login_bytes, 0, login.length());
// System.arraycopy has a BUG!!! Do not use it
                for(int i = 0; i < login.length(); i++)
                {
                  msg.body[body_position+i] = login_bytes[i];	
                }
                
                body_position += login.length();
                
            // writing db_name	
            
            	msg.body[body_position] = 0; // format code
            	NetOps.writeInt(db_name.length(), msg.body, body_position+1);

            	byte db_name_bytes[] = db_name.getBytes();
            	body_position += 5;
            	//System.arraycopy(msg.body, body_position, db_name_bytes, 0, db_name.length());
                for(int i = 0; i < db_name.length(); i++)
                {
                  msg.body[body_position+i] = db_name_bytes[i];	
                }
                body_position += db_name.length();
            	
            	NetOps.writeMsg(msg, outputStream);
            }
            if (msg.instruction == 100) //ErrorResponse
            {
                NetOps.driverPrintOut(" Failed\n\n");	
                con = null;
                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));

            }
            
            NetOps.readMsg(msg,bufInputStream);
            if (msg.instruction == 150) //SendAuthenticationParameters
            {
                NetOps.driverPrintOut("OK\n\n");
                NetOps.driverPrintOut("Processing authentication...");
            	msg.instruction = 130;
            	msg.length = 5+password.length();
            	if (msg.length > NetOps.SOCKET_MSG_BUF_SIZE) throw new DriverException("Too long user name/db name");
            	int body_position = 0;
            	
            // writing password	
            	msg.body[0] = 0; // format code
            	NetOps.writeInt(password.length(), msg.body, 1);

            	byte passw_bytes[] = password.getBytes();
            	body_position += 5;
//            	System.arraycopy(msg.body, body_position, passw_bytes, 0, password.length());
                for(int i = 0; i < password.length(); i++)
                {
                  msg.body[body_position+i] = passw_bytes[i];	
                }

                body_position += password.length();
            	NetOps.writeMsg(msg, outputStream);
                NetOps.readMsg(msg, bufInputStream);//read the answer

            }
            
            if (msg.instruction == 160) //AuthenicationOk
            {
                NetOps.driverPrintOut("Authentication succeeded\n\n");
                	
            }
            if (msg.instruction == 170) //AuthenicationFailed
            {
                NetOps.driverPrintOut("Authentication failed\n\n");
                throw new DriverException("Authentication failed");
            }
            if (msg.instruction == 100)
            {
                NetOps.driverPrintOut("Authentication failed\n\n");	
                con = null;
                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length));
            }
          
           }
           catch (IOException e) {
            NetOps.driverErrOut("Socket error"+e.toString());  
            throw new DriverException("Socket error");
           }
            
          } catch (UnknownHostException e) {
            NetOps.driverErrOut("Don't know about host: "+url);  
            throw new DriverException("Unknown host: "+url);
          } catch (IOException e) {
            NetOps.driverErrOut("Can not connect to the Sedna DBMS on "
                                +url);
            throw new DriverException("Can not connect to the Sedna DBMS on "
                                +url);
          }
   return con;

}

}