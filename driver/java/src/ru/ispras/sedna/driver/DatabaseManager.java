
/*
 * File:  DatabaseManager.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;
import java.net.*;
import java.util.*;

//~--- classes ----------------------------------------------------------------
/**
 * To start working with Sedna application has to open a session via establishing
 * an authenticated connection with the Sedna server. <code>DatabaseManager</code> provides <code>getConnection</code>
 * method to open connections to one of the Sedna databases. 
 */
public class DatabaseManager {
/**
 * To open a session application uses static method <code>getConnection</code> of the
 * DatabaseManager class.
 *
 * @param  url_string  the name of the computer where the Sedna DBMS is running. This parameter may contain a port number. If the port number is not specified, the default port number (5050) is used
 * @param  db_name     the name of the database to connect to
 * @param  login       user name
 * @param  password    user password
 * @return             If the connection is established and authentication succeeds the method returns an object that implements the <code>SednaConnection</code> interface. Otherwise, <code>DriverException</code> is thrown
 * @see         SednaConnection
 */
	
    public static SednaConnection getConnection(String url_string,
                                                String db_name, 
                                                String login, 
                                                String password)
            throws DriverException {
        InputStream         inputStream;
        BufferedInputStream bufInputStream;
        OutputStream        outputStream;
        SednaConnectionImpl con    = new SednaConnectionImpl();
        Socket              socket = null;
        String              url    = "";
        int                 socket_port;

        try {
            if (url_string.indexOf(":") != (-1)) {
                url         = url_string.substring(0, url_string.indexOf(":"));
                socket_port = Integer.parseInt(
                    url_string.substring(
                        url_string.indexOf(":") + 1, url_string.length()));
            } else {
                url         = url_string;
                socket_port = 5050;
            }

            if (url.equals("localhost")) {
                url = "127.0.0.1";    // local ip address
            }

            // open a socket connection
            socket = new Socket(url, socket_port);
            con.setSocket(socket);
            outputStream   = socket.getOutputStream();
            inputStream    = socket.getInputStream();
            bufInputStream = new BufferedInputStream(inputStream, 1024);
            con.setOS(outputStream);
            con.setBIS(bufInputStream);

            // StartUp message
            NetOps.Message msg = new NetOps.Message();

            msg.instruction = NetOps.se_StartUp;    // Start-Up
            msg.length      = 0;
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);

            if (msg.instruction == NetOps.se_SendSessionParameters)    // SendSessionParameters
            {
                msg.instruction = NetOps.se_SessionParameters;

                // body contains:
                // major protocol version
                // minor protocol version
                // login string
                // dbname string
                msg.length = 2 + 5 + login.length() + 5 + db_name.length();

                if (msg.length > NetOps.SEDNA_SOCKET_MSG_BUF_SIZE) {
                    throw new DriverException(ErrorCodes.SE3015, "");
                }

                int body_position = 0;

                // writing protocol version 
                msg.body[body_position]     = NetOps.majorProtocolVer;
                msg.body[body_position + 1] = NetOps.minorProtocolVer;
                body_position += 2;

                // writing login
                msg.body[body_position] = 0;    // format code
                NetOps.writeInt(login.length(), msg.body, body_position + 1);

                byte login_bytes[] = login.getBytes();

                body_position += 5;

                for (int i = 0; i < login.length(); i++) {
                    msg.body[body_position + i] = login_bytes[i];
                }

                body_position += login.length();

                // writing db_name
                msg.body[body_position] = 0;                   // format code
                NetOps.writeInt(db_name.length(), msg.body, body_position + 1);

                byte db_name_bytes[] = db_name.getBytes();

                body_position += 5;

                for (int i = 0; i < db_name.length(); i++) {
                    msg.body[body_position + i] = db_name_bytes[i];
                }

                body_position += db_name.length();
                NetOps.writeMsg(msg, outputStream);
            }

            if (msg.instruction == NetOps.se_ErrorResponse)    // ErrorResponse
            {
                con = null;

                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
            }

            NetOps.readMsg(msg, bufInputStream);

            if (msg.instruction == NetOps.se_SendAuthParameters)    // SendAuthenticationParameters
            {
                msg.instruction = NetOps.se_AuthenticationParameters;
                msg.length      = 5 + password.length();

                if (msg.length > NetOps.SEDNA_SOCKET_MSG_BUF_SIZE) {
                    throw new DriverException(ErrorCodes.SE3015, "");
                }

                int body_position = 0;

                // writing password
                msg.body[0] = 0;    // format code
                NetOps.writeInt(password.length(), msg.body, 1);

                byte passw_bytes[] = password.getBytes();

                body_position += 5;

                for (int i = 0; i < password.length(); i++) {
                    msg.body[body_position + i] = passw_bytes[i];
                }

                body_position += password.length();
                NetOps.writeMsg(msg, outputStream);
                NetOps.readMsg(msg, bufInputStream);    // read the answer
            }

            if (msg.instruction == NetOps.se_AuthenticationFailed)    // AuthenicationFailed
            {
                throw new DriverException(ErrorCodes.SE3053, "");
            }

            if (msg.instruction == NetOps.se_ErrorResponse) {
                con = null;

                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
            } else if (msg.instruction != NetOps.se_AuthenticationOK) {
                con = null;

                throw new DriverException(ErrorCodes.SE3008, "");
            }
        } catch (UnknownHostException e) {
            throw new DriverException(ErrorCodes.SE3003, url);
        } catch (IOException e) {
            throw new DriverException(ErrorCodes.SE3003, url);
        }

        return con;
    }
    
    /**
     * @deprecated
     */
    protected DatabaseManager(){
    }
}
