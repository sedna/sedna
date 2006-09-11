/*
 * File:  casting_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _CASTING_OPERATIONS_H
#define _CASTING_OPERATIONS_H

#include "sedna.h"
#include "PPBase.h"


tuple_cell cast (const tuple_cell &SV, xmlscm_type TT);
bool is_castable(const tuple_cell &SV, xmlscm_type TT);

tuple_cell cast_primitive_to_xs_string       (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_QName        (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_untypedAtomic(const tuple_cell &c);
tuple_cell cast_primitive_to_xs_float        (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_double       (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_decimal      (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_integer      (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_boolean      (const tuple_cell &c);
tuple_cell cast_primitive_to_xs_dateTime     (const tuple_cell &c, xmlscm_type xtype);


#endif

