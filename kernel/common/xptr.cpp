/*
 * File:  xptr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "common/xptr.h"
#include "common/errdbg/d_printf.h"

void xptr::print() const
{ 
    d_printf3("(layer, addr) = (%d, 0x%x)\n", (int)layer, (unsigned int)addr); 
}


