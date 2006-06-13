
/*
 * File:  SednaSerializedResult.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

//~--- interfaces -------------------------------------------------------------

public interface SednaSerializedResult {
    public String next() throws DriverException;

    public int next(Writer writer) throws DriverException;
}
