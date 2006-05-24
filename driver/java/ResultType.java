/*
 * File:  ResultType.java
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


package ru.ispras.sedna.driver;

import java.io.*;

public final class ResultType
{
    public static final ResultType XML = new ResultType("xml");
    public static final ResultType SXML = new ResultType("sxml");

        private String code;

    private ResultType(String code)
    {
	// assert code != null ???
	this.code = code;
    }

    public String getCode()
    {
	return code;
    }
}
