/*
 * File:  xs_helper.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XS_HELPER_H
#define _XS_HELPER_H

#include "sedna.h"
#include "PPBase.h"

char *_get_pointer_to_c_str(const tuple_cell &c);

float   c_str2xs_float  (const char *t);
double  c_str2xs_double (const char *t);
__int64 c_str2xs_integer(const char *t);
bool    c_str2xs_boolean(const char *t);



inline tuple_cell cast_string_type_to_xs_float(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_float(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_double(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_double(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_decimal(const tuple_cell &c)
    { return tuple_cell::atomic(xs_decimal_t(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_integer(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_integer(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_boolean(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_boolean(_get_pointer_to_c_str(c))); }

tuple_cell cast_string_type_to_xs_dateTime(const tuple_cell &c, xmlscm_type xtype);








char *get_xs_double_lexical_representation (char *s, double d);
char *get_xs_integer_lexical_representation(char *s, __int64 v);
char *get_xs_boolean_lexical_representation(char *s, bool b);
char *get_xs_dateTime_lexical_representation(char *s, const XMLDateTime &d, xmlscm_type xtype);




inline tuple_cell cast_xs_float_to_string_type(const tuple_cell &c)
{
    get_xs_double_lexical_representation(tr_globals::mem_str_buf, (double)c.get_xs_float());
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_double_to_string_type(const tuple_cell &c)
{
    get_xs_double_lexical_representation(tr_globals::mem_str_buf, c.get_xs_double());
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_decimal_to_string_type(const tuple_cell &c)
{
    c.get_xs_decimal().get_c_str(tr_globals::mem_str_buf);
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_integer_to_string_type(const tuple_cell &c)
{
    get_xs_integer_lexical_representation(tr_globals::mem_str_buf, c.get_xs_integer());
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_boolean_to_string_type(const tuple_cell &c)
{
    get_xs_boolean_lexical_representation(tr_globals::mem_str_buf, c.get_xs_boolean());
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_dateTime_to_string_type(const tuple_cell &c, xmlscm_type xtype)
{
    get_xs_dateTime_lexical_representation(tr_globals::mem_str_buf, c.get_xs_dateTime(), xtype);
    return tuple_cell::atomic_deep(xs_string, tr_globals::mem_str_buf);
}

#endif

