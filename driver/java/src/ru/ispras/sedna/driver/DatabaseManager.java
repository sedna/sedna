/*
 * File:  DatabaseManager.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;
import java.net.*;

/**
 * To start working with Sedna application has to open a session via establishing
 * an authenticated connection with the Sedna server.
 * Use {@link DatabaseManager#getConnection(String, String, String, String)} 
 * method to open connections to one of the Sedna databases. 
 */
public class DatabaseManager {

    /**
     * Try to establish connection with the Sedna database server.
     * @param  urlString the name of the computer where the Sedna DBMS is running.
     * This parameter may contain a port number. For example <code>127.0.0.1:6060</code> specifies that Sedna
     * server is running on the localhost and listening on the TCP port <code>6060</code>. If the port number is not specified,
     * the default port number <code>5050</code> is used
     * @param  dbName the name of the database to connect to
     * @param  login user name
     * @param  password user password
     * @return If the connection has been established and authentication succeeded the method returns an object that implements the
     * {@link ru.ispras.sedna.driver.SednaConnection} interface
     * @throws DriverException if failed to establish connection
     * @see ru.ispras.sedna.driver.SednaConnection
     */
    public static SednaConnection getConnection(String urlString,
                                                String dbName,
                                                String login,
                                                String password) throws DriverException {
        if(urlString == null || urlString.length() == 0)
            throw new IllegalArgumentException("URL string is null or empty");
        if(dbName == null || dbName.length() == 0)
            throw new IllegalArgumentException("Database name string is null or empty");
        if(login == null || login.length() == 0)
            throw new IllegalArgumentException("Login string is null or empty");
        if(password == null || password.length() == 0)
            throw new IllegalArgumentException("Password string is null or empty");

        InputStream         inputStream;
        BufferedInputStream bufInputStream;
        OutputStream        outputStream;
        SednaConnectionImpl con = new SednaConnectionImpl();
        Socket              socket;
        String              url = "";
        int                 socketPort;

        try {
            if (urlString.indexOf(":") != (-1)) {
                url = urlString.substring(0, urlString.indexOf(":"));
                try {
                    socketPort = Integer.parseInt(urlString.substring(urlString.indexOf(":") + 1, urlString.length()));
                } catch (NumberFormatException e) {
                    throw new IllegalArgumentException("URL string contains invalid port number", e);
                }
                if(socketPort < 0) {
                    throw new IllegalArgumentException("URL string contains negative port number");
                }
            } else {
                url = urlString;
                socketPort = 5050;
            }

            if (url.equals("localhost")) {
                url = "127.0.0.1";
            }

            /* open a socket connection */
            socket = new Socket(url, socketPort);
            con.setSocket(socket);
            outputStream   = socket.getOutputStream();
            inputStream    = socket.getInputStream();
            bufInputStream = new BufferedInputStream(inputStream, 1024);
            con.setOS(outputStream);
            con.setBIS(bufInputStream);

            /* StartUp message */
            NetOps.Message msg = new NetOps.Message();

            msg.instruction = NetOps.se_StartUp;
            msg.length      = 0;
            NetOps.writeMsg(msg, outputStream);
            NetOps.readMsg(msg, bufInputStream);

            if (msg.instruction == NetOps.se_SendSessionParameters)
            {
                msg.instruction = NetOps.se_SessionParameters;

                /* body contains: major protocol version, minor protocol version
                 * login string, database name string */
                msg.length = 2 + 5 + login.length() + 5 + dbName.length();

                if (msg.length > NetOps.SEDNA_SOCKET_MSG_BUF_SIZE) {
                    throw new DriverException(ErrorCodes.SE3015, "");
                }

                int body_position = 0;

                /* writing protocol version */
                msg.body[body_position]     = NetOps.majorProtocolVer;
                msg.body[body_position + 1] = NetOps.minorProtocolVer;
                body_position += 2;

                /* writing login */
                msg.body[body_position] = 0;
                NetOps.writeInt(login.length(), msg.body, body_position + 1);
                byte login_bytes[] = login.getBytes();
                body_position += 5;
                System.arraycopy(login_bytes, 0, msg.body, body_position, login.length());
                body_position += login.length();

                /* writing database name */
                msg.body[body_position] = 0;
                NetOps.writeInt(dbName.length(), msg.body, body_position + 1);
                byte db_name_bytes[] = dbName.getBytes();
                body_position += 5;
                System.arraycopy(db_name_bytes, 0, msg.body, body_position, dbName.length());
                body_position += dbName.length();

                NetOps.writeMsg(msg, outputStream);
            }

            if (msg.instruction == NetOps.se_ErrorResponse) {
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

                /* writing password */
                msg.body[0] = 0;
                NetOps.writeInt(password.length(), msg.body, 1);

                byte passw_bytes[] = password.getBytes();

                body_position += 5;

                System.arraycopy(passw_bytes, 0, msg.body, body_position, password.length());

                body_position += password.length();
                NetOps.writeMsg(msg, outputStream);
                NetOps.readMsg(msg, bufInputStream);
            }

            if (msg.instruction == NetOps.se_AuthenticationFailed) {
                throw new DriverException(ErrorCodes.SE3053, "");
            } else if (msg.instruction == NetOps.se_ErrorResponse) {
                throw new DriverException(NetOps.getErrorInfo(msg.body, msg.length), NetOps.getErrorCode(msg.body));
            } else if (msg.instruction != NetOps.se_AuthenticationOK) {
                throw new DriverException(ErrorCodes.SE3008, "");
            }
        } catch (UnknownHostException e) {
            throw new DriverException(ErrorCodes.SE3003, url);
        } catch (IOException e) {
            throw new DriverException(ErrorCodes.SE3003, url);
        }
        return con;
    }
}
