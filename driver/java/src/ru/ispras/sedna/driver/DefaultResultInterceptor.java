/*
 * File: DefaultResultInterceptor.java
 * Copyright (C) 2004-2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

package ru.ispras.sedna.driver;

import java.nio.ByteBuffer;

class DefaultResultInterceptor extends ResultInterceptor {

    public ByteBuffer handle(ByteBuffer res) {
        return res;
    }
}
