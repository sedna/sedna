/*
 * File:  xs_helper.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XS_HELPER_H
#define _XS_HELPER_H

#include "common/sedna.h"
#include "tr/crmutils/crmbase.h"
#include "tr/executor/base/xs_decimal_t.h"
#include "tr/executor/base/tuple.h"
#include "tr/strings/strings.h"

#define IS_WHITESPACE(byte) \
    ((byte) == ' ' || (byte) == '\t' || (byte) == '\n' || (byte) == '\r')

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

__int64 double2__int64_bits(double d);
__int32 float2__int32_bits (float  f);


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

double round_half_to_even_double(double d, __int64 precision);
float round_half_to_even_float(float d, __int64 precision);
__int64 round_half_to_even_integer(__int64 d, __int64 precision);





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
///              Datatypes spec. It is used for datatypes that aren't allowed to
///              contain whitespace except leading and trailing (e.g. anyURI).

void replace_string_normalization (const tuple_cell *tc, stmt_str_buf& out_buf);
void collapse_string_normalization(const tuple_cell *tc, stmt_str_buf& out_buf);
void remove_string_normalization  (const tuple_cell *tc, stmt_str_buf& out_buf);

/// Normalization on char*
/// -- char* must point to zero ended C-string.

void replace_string_normalization (const char *s, stmt_str_buf& out_buf);
void collapse_string_normalization(const char *s, stmt_str_buf& out_buf);
void remove_string_normalization  (const char *s, stmt_str_buf& out_buf);


/////////////////////////////////////////////////////////////////////////
/// XML 1.0 and XML 1.1 Char production checking
/////////////////////////////////////////////////////////////////////////

/// Returns true if the specified character is valid XML 1.1 char. Method
/// also checks the surrogate character range from 0x10000 to 0x10FFFF.
/// Char  ::=  [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
inline bool isXML11Valid(int c) 
{
    return    (0x1     <= c && c <= 0xD7FF)
           || (0xE000  <= c && c <= 0xFFFD)
           || (0x10000 <= c && c <= 0x10FFFF);
}


/// Table for ASCII symbols optimization.
const unsigned char xml10_one_byte_valid[16] = {0x00, 0x64, 0x00, 0x00,
                                                0xFF, 0xFF, 0xFF, 0xFF, 
                                                0xFF, 0xFF, 0xFF, 0xFF, 
                                                0xFF, 0xFF, 0xFF, 0xFF};

/// Returns true if the specified character is valid XML 1.1 char. Method
/// also checks the surrogate character range from 0x10000 to 0x10FFFF.
/// Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
inline bool isXML10Valid(int c) 
{
    return (c & (~0x0 << 7)) ? 
           ( (0x80 <= c && c <= 0xD7FF) || (0xE000 <= c && c <= 0xFFFD) || (0x10000 <= c && c <= 0x10FFFF) ) :
           ((xml10_one_byte_valid[(c >> 3)] & (0x80 >> (c & 7))) != 0);
}


#endif

