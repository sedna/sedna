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

void xptr::inc_offset(unsigned n)
{
    // first, shift layer
    layer += (t_layer)(n / LAYER_ADDRESS_SPACE_SIZE);
    n = n % LAYER_ADDRESS_SPACE_SIZE;

    // then, offset
    if (LAYER_ADDRESS_SPACE_SIZE - offs > n)
    {
        offs += n;
    }
    else
    {
        layer++;
        offs = n - (LAYER_ADDRESS_SPACE_SIZE - offs);
    }
}

void xptr::dec_offset(unsigned n)
{
    // first, shift layer
    layer -= (t_layer)(n / LAYER_ADDRESS_SPACE_SIZE);
    n = n % LAYER_ADDRESS_SPACE_SIZE;

    // then, offset
    if (offs >= n)
    {
        offs -= n;
    }
    else
    {
        layer--;
        offs = LAYER_ADDRESS_SPACE_SIZE - (n - offs);
    }
}

void xptr::print() const
{ 
    d_printf3("(layer, addr) = (%u, 0x%x)\n", (unsigned int)layer, offs);
}
