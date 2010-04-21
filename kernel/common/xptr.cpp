/*
 * File:  xptr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "common/xptr.h"
#include "common/errdbg/d_printf.h"

/* xptr layer-specific values */
void  *LAYER_ADDRESS_SPACE_START_ADDR     = NULL;
void  *LAYER_ADDRESS_SPACE_BOUNDARY       = NULL;
uintptr_t LAYER_ADDRESS_SPACE_START_ADDR_INT = 0;
uintptr_t LAYER_ADDRESS_SPACE_BOUNDARY_INT   = 0;
lsize_t LAYER_ADDRESS_SPACE_SIZE           = 0;

void xptr::print() const
{ 
    d_printf3("(layer, addr) = (%d, 0x%x)\n", (unsigned int)layer, offs);
}


