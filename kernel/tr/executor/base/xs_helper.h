/*
 * File:  xs_helper.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XS_HELPER_H
#define _XS_HELPER_H

#include "sedna.h"
#include "PPBase.h"
#include "crmutils.h"
#include "xs_decimal_t.h"

#define IS_WHITESPACE(byte) \
    (byte == ' ' || byte == '\t' || byte == '\n' || byte == '\r')

/////////////////////////////////////////////////////////////////////////
/// String to XML Schema datatypes conversion routines.
/////////////////////////////////////////////////////////////////////////

char *_get_pointer_to_c_str(const tuple_cell &c);

float   c_str2xs_float  (const char *t);
double  c_str2xs_double (const char *t);
__int64 c_str2xs_integer(const char *t);
bool    c_str2xs_boolean(const char *t);


/////////////////////////////////////////////////////////////////////////
/// XML Schema fixed datatypes to string conversion routines.
/////////////////////////////////////////////////////////////////////////

char *get_xs_double_lexical_representation  (char *s, double d);
char *get_xs_float_lexical_representation   (char *s, float f);
char *get_xs_integer_lexical_representation (char *s, __int64 v);
char *get_xs_boolean_lexical_representation (char *s, bool b);
char *get_xs_dateTime_lexical_representation(char *s, const XMLDateTime &d);

char *get_lexical_representation_for_fixed_size_atomic(char *s, const tuple_cell &c, t_print ptype);


/////////////////////////////////////////////////////////////////////////
/// XML Schema fixed datatypes to fixed datatypes conversion routines.
/////////////////////////////////////////////////////////////////////////
__int64 xs_float2xs_integer(float v);
__int64 xs_double2xs_integer(double v);
inline __int64 xs_boolean2xs_integer(bool v)
{
    return (v ? (__int64)1 : (__int64)0);
}

/////////////////////////////////////////////////////////////////////////
/// Numeric operations.
/////////////////////////////////////////////////////////////////////////
#define double_NaN          (*(double*)(&_double_NaN))
#define double_Neg_INF      (*(double*)(&_double_Neg_INF))
#define double_Pos_INF      (*(double*)(&_double_Pos_INF))

#define float_NaN           (*(float*)(&_float_NaN))
#define float_Neg_INF       (*(float*)(&_float_Neg_INF))
#define float_Pos_INF       (*(float*)(&_float_Pos_INF))

extern __int64 _double_NaN;
extern __int64 _double_Neg_INF;
extern __int64 _double_Pos_INF;
extern __int32 _float_NaN;
extern __int32 _float_Neg_INF;
extern __int32 _float_Pos_INF;

double       xs_divide(double       x, double       y);
float        xs_divide(float        x, float        y);
xs_decimal_t xs_divide(__int64      x, __int64      y);
xs_decimal_t xs_divide(xs_decimal_t x, xs_decimal_t y);

__int64 xs_integer_divide(double       x, double       y);
__int64 xs_integer_divide(float        x, float        y);
__int64 xs_integer_divide(__int64      x, __int64      y);
__int64 xs_integer_divide(xs_decimal_t x, xs_decimal_t y);

double       xs_mod(double       x, double       y);
float        xs_mod(float        x, float        y);
__int64      xs_mod(__int64      x, __int64      y);
xs_decimal_t xs_mod(xs_decimal_t x, xs_decimal_t y);



/////////////////////////////////////////////////////////////////////////
/// XML Schema Datatypes normalization routines.
/////////////////////////////////////////////////////////////////////////

/// Brief description:

/// "replace"  - All occurrences of #x9 (tab), #xA (line feed) and #xD 
///              (carriage return) are replaced with #x20 (space);
/// "collapse" - after the processing implied by replace, contiguous sequences 
///              of #x20's are collapsed to a single #x20, and leading and 
///              trailing #x20's are removed;
/// "remove"   - all leading and trailing (after first occurence of whitespace) 
///              #x9 (tab), #xA (line feed) and #xD (carriage return) are 
///              removed. This normalization is not described in XML Schema 
///              Datatypes spec. It is used for datatypes that are not allowed to
///              contain whitespace except leading and trailing (e.g. xs:anyURI).

void replace_string_normalization (const tuple_cell *tc, stmt_str_buf& out_buf);
void collapse_string_normalization(const tuple_cell *tc, stmt_str_buf& out_buf);
void remove_string_normalization  (const tuple_cell *tc, stmt_str_buf& out_buf);

/// Normalization on char*
/// -- char* must point to zero ended C-string.

void replace_string_normalization (const char *s, stmt_str_buf& out_buf);
void collapse_string_normalization(const char *s, stmt_str_buf& out_buf);
void remove_string_normalization  (const char *s, stmt_str_buf& out_buf);



#endif

