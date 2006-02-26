/*
 * File:  SednaStatement.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.io.*;

public interface SednaStatement
{
public boolean execute(String queryText) throws DriverException;

public boolean execute(InputStream in) throws DriverException, IOException;

public void loadDocument(InputStream in, String doc_name) throws DriverException, IOException;

public void loadDocument(InputStream in, String doc_name, String col_name) throws DriverException, IOException;

public SednaSerializedResult getSerializedResult();

}
