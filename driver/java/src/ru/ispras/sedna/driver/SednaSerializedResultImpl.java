/*
 * File:  SednaSerializedResultImpl.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.io.*;

class SednaSerializedResultImpl implements SednaSerializedResult {
    BufferedInputStream bufInputStream;
    boolean             hasNextItem;
    OutputStream        outputStream;
    StringBuffer        stringItem;
    boolean             doTraceOutput;

    SednaSerializedResultImpl(StringBuffer stringItem,
                              boolean hasNextItem,
                              BufferedInputStream is,
                              OutputStream os,
                              boolean doTraceOutput) {
        this.bufInputStream = is;
        this.outputStream   = os;
        this.stringItem     = stringItem;
        this.hasNextItem    = hasNextItem;
        this.doTraceOutput  = doTraceOutput;
    }

    public String next() throws DriverException {
        String         tmpItem;
        NetOps.Message msg = new NetOps.Message();

        if (this.stringItem == null)
            tmpItem = null;
        else
            tmpItem = this.stringItem.toString();

        try {
            if(this.hasNextItem) {
                msg.instruction = NetOps.se_GetNextItem;
                msg.length      = 0;
                NetOps.writeMsg(msg, outputStream);

                NetOps.StringItem sitem = NetOps.readStringItem(bufInputStream, doTraceOutput);

                this.stringItem  = sitem.item;
                this.hasNextItem = sitem.hasNextItem;
            } else {
                this.stringItem = null;
            }

        } catch (OutOfMemoryError e) {
            throw new DriverException(ErrorCodes.SE5501, "");
        } catch (DriverException e) {
            NetOps.driverErrOut(e.toString() + "\n");
            throw e;
        }

        return tmpItem;
    }

    public int next(Writer writer) throws DriverException {
        NetOps.Message msg = new NetOps.Message();

        try {
            if (this.stringItem == null)
                return -1;

            writer.write(this.stringItem.toString());

            if(this.hasNextItem) {
                msg.instruction = NetOps.se_GetNextItem;
                msg.length      = 0;
                NetOps.writeMsg(msg, outputStream);

                NetOps.StringItem sitem = NetOps.readStringItem(bufInputStream, doTraceOutput);

                this.stringItem  = sitem.item;
                this.hasNextItem = sitem.hasNextItem;
            } else {
                this.stringItem = null;
            }

        } catch (OutOfMemoryError e) {
            throw new DriverException(ErrorCodes.SE5501, "");
        } catch (IOException e) {
            throw new DriverException(ErrorCodes.SE3007, "");
        } catch (DriverException e) {
            NetOps.driverErrOut(e.toString() + "\n");
            throw e;
        }

        return 0;
    }
}
