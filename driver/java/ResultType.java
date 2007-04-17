
/*
 * File:  ResultType.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



package ru.ispras.sedna.driver;

//~--- JDK imports ------------------------------------------------------------

import java.io.*;

//~--- classes ----------------------------------------------------------------
/*
 * <code>ResultType</code> is used to set the type of the query result. 
 * There are two possible formats for XQuery query result in Sedna: XML and SXML 
 * (see Sedna Programmer’s Guide for details).
 */
public final class ResultType {
    public static final ResultType XML  = new ResultType("xml");
    public static final ResultType SXML = new ResultType("sxml");

    //~--- fields -------------------------------------------------------------

    private String code;

    //~--- constructors -------------------------------------------------------

    private ResultType(String code) {

        // assert code != null ???
        this.code = code;
    }

    //~--- get methods --------------------------------------------------------

    public String getCode() {
        return code;
    }
}
