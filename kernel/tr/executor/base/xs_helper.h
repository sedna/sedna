/*
 * File:  xs_helper.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XS_HELPER_H
#define _XS_HELPER_H

#include "sedna.h"
#include "PPBase.h"
#include "crmutils.h"

char *_get_pointer_to_c_str(const tuple_cell &c);

float   c_str2xs_float  (const char *t);
double  c_str2xs_double (const char *t);
__int64 c_str2xs_integer(const char *t);
bool    c_str2xs_boolean(const char *t);



char *get_xs_double_lexical_representation (char *s, double d);
char *get_xs_integer_lexical_representation(char *s, __int64 v);
char *get_xs_boolean_lexical_representation(char *s, bool b);
char *get_xs_dateTime_lexical_representation(char *s, const XMLDateTime &d, xmlscm_type xtype);


char *get_lexical_representation_for_fixed_size_atomic(char *s, const tuple_cell &c, t_print ptype);




#endif

