/*
 * File:  base64Binary.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BASE64BIMARY_H
#define _BASE64BINARY_H

#include "sedna.h"
#include "PPBase.h"

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
///    Defines interface fot casting to or from xs:base64Binary type.
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c);

tuple_cell cast_base64Binary_to_hexBinary(const tuple_cell &c);
tuple_cell cast_hexBinary_to_base64Binary(const tuple_cell &c);

#endif
