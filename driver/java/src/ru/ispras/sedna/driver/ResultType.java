/*
 * File:  ResultType.java
 * Copyright (C) 2004-2011 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

/**
 * Used to set the type of the query result. There are two possible formats for XQuery
 * query result in Sedna: XML and SXML (see Sedna Programmer's Guide for details).
 */
public final class ResultType {
    /**
     * Predefined XML result type
     */
    public static final ResultType XML  = new ResultType("xml");
    /**
     * Predefined SXML result type
     */
    public static final ResultType SXML = new ResultType("sxml");

    private String code;

    /**
     * Creates a new instance of the <code>ResultType</code> specified by code:
     * either "sxml" or "xml".
     * @param code string representation of the result type
     */
    private ResultType(String code) {
        this.code = code;
    }

    /**
     * Returns code of the result type.
     * @return either "xml" or "sxml" result type
     */
    public String getCode() {
        return code;
    }
}
