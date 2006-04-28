/*
 * File:  xptr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "xptr.h"
#include "d_printf.h"

xptr XNULL;

void xptr::print() const 
{ 
    d_printf3("(layer, addr) = (%d, 0x%x)\n", (int)layer, (unsigned int)addr); 
}


