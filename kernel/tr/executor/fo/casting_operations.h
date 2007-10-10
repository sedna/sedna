/*
 * File:  casting_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _CASTING_OPERATIONS_H
#define _CASTING_OPERATIONS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


tuple_cell cast (const tuple_cell &SV, xmlscm_type TT, int __xquery_line = 0);
bool is_castable(const tuple_cell &SV, xmlscm_type TT, int __xquery_line = 0);

tuple_cell cast_primitive_to_xs_untypedAtomic(const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_string       (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_float        (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_double       (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_decimal      (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_integer      (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_dateTime     (const tuple_cell &c, xmlscm_type xtype, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_boolean      (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_base64Binary (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_hexBinary    (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_anyURI       (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_QName        (const tuple_cell &c, int __xquery_line = 0);
tuple_cell cast_primitive_to_xs_NOTATION     (const tuple_cell &c, int __xquery_line = 0);


#endif

