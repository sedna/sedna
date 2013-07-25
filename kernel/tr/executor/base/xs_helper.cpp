/*
 * File:  xs_helper.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>

#include "common/sedna.h"
#include "common/u/uutils.h"

#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/xs_fp_converter.h"
#include "tr/executor/base/PPBase.h"
#include "tr/strings/e_string.h"


/*******************************************************************************
 * string to xs datatype conversion functions
 ******************************************************************************/

// Funtion returns pointer to C string representing value.
// Pointer is valid only for a short period of time.
char *_get_pointer_to_c_str(const tuple_cell &c)
{
    char *t = NULL;

    if (c.is_heavy_atomic())
    { // VMM is used for storing
         int64_t size = c.get_strlen_vmm();
        if (size > MAX_ATOMIC_LEX_REPR_SIZE) throw USER_EXCEPTION2(SE1003, "Buffer overflow");
        t = executor_globals::mem_str_buf;
        estr_copy_to_buffer(t, c.get_str_vmm(), size);
        t[size] = '\0';
    }
    else
    { // On-line memory is used
        t = c.get_str_mem();
    }

    return t;
}

static void _strip_c_str(const char* t, const char** start, const char** end)
{
    size_t len = strlen(t);
    *start = t;
    *end = t + len - 1;

    while (**start != '\0' &&
           (**start == ' ' || **start == '\t' || **start == '\n' || **start == '\r'))
        ++(*start);

    while (*end > *start &&
           (**end == ' ' || **end == '\t' || **end == '\n' || **end == '\r'))
        --(*end);

    ++(*end);
}

float c_str2xs_float(const char *t)
{
    float res = 0.0;
    const char *start = NULL;
    const char *end = NULL;
    _strip_c_str(t, &start, &end);

    if ((end - start == 3) && (strncmp(start, "NaN", 3) == 0))
        res = float_NaN;
    else if ((end - start == 4) && (strncmp(start, "-INF", 4) == 0))
        res = float_Neg_INF;
    else if ((end - start == 3) && (strncmp(start, "INF", 3) == 0))
        res = float_Pos_INF;
    else
    {
        char* stop = NULL;
        double d = strtod(start, &stop);
        if ((end - start == 0) || (stop != end) || u_is_nan(d) || u_is_pos_inf(d) || u_is_neg_inf(d))
            throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:float type");
		res = (float)d;
    }

    return res;
}

double c_str2xs_double(const char *t)
{
    double res = 0.0;
    const char *start = NULL;
    const char *end = NULL;
    _strip_c_str(t, &start, &end);

    if ((end - start == 3) && (strncmp(start, "NaN", 3) == 0))
        res = double_NaN;
    else if ((end - start == 4) && (strncmp(start, "-INF", 4) == 0))
        res = double_Neg_INF;
    else if ((end - start == 3) && (strncmp(start, "INF", 3) == 0))
        res = double_Pos_INF;
    else
    {
        char* stop = NULL;
        res = strtod(start, &stop);
        if ((end - start == 0) || (stop != end) || u_is_nan(res) || u_is_pos_inf(res) || u_is_neg_inf(res))
            throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:double type");
    }

    return res;
}

int64_t c_str2xs_integer(const char *t)
{
    int64_t res = 0;
    char* stop = NULL;
    const char *start = NULL;
    const char *end = NULL;
    int overflow = 0;

    _strip_c_str(t, &start, &end);

    errno = 0; // must set to zero; see strtoll man pages
    res = strto__int64(start, &stop, 10);
    if ((INT64_MAX == res || INT64_MIN == res) && errno == ERANGE) overflow = 1;

    if ((end - start == 0) || (stop != end)) throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:integer type");
    if (overflow) throw XQUERY_EXCEPTION2(FOAR0002, "Cannot convert to xs:integer type");

    return res;
}

bool c_str2xs_boolean(const char *t)
{
    bool res = false;
    const char *start = NULL;
    const char *end = NULL;
    _strip_c_str(t, &start, &end);

    if (end - start == 1)
    {
        if (strncmp(start, "1", 1) == 0) res = true;
        else if (strncmp(start, "0", 1) == 0) res = false;
        else throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:boolean type");
    }
    else if ((end - start == 4) && (strncmp(start, "true", 4) == 0)) res = true;
    else if ((end - start == 5) && (strncmp(start, "false", 5) == 0)) res = false;
    else throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:boolean type");

    return res;
}





/*******************************************************************************
 * xs datatype to string conversion functions
 ******************************************************************************/

#define udouble2_int64_bits(p)

#define udouble2_int64_bits(p)
#define ufloat2_int32_bits(p)

#define FPC_DOUBLESIGNMASK  ((int64_t)0x8000 << (int64_t)48) // 0x8000000000000000L

int64_t double2__int64_bits(double d)
{
    union {
    int64_t l;
    double  d;
    } u;

    if (u_is_nan(d))
        return _double_NaN;

    udouble2_int64_bits(&d);
    u.d = d;
    return u.l;
}

int32_t float2__int32_bits(float f)
{
    union {
    int32_t i;
    float   f;
    } u;

    if (u_is_nan((double)f))
        return _float_NaN;

    ufloat2_int32_bits(&f);
    u.f = f;
    return u.i;
}


#define double_sign(d) (double2__int64_bits(d) & FPC_DOUBLESIGNMASK)

static int _sprint__uint64(char* buffer, uint64_t x, bool m)
{
    uint64_t quot = x / (uint64_t)1000;
    int chars_written = 0;
    if (quot != 0)
    {
        chars_written = _sprint__uint64(buffer, quot, m);
        chars_written += sprintf(buffer + chars_written, "%03u", (uint32_t)(x % (uint64_t)1000));
    }
    else
    {
        if (m)
            chars_written += sprintf(buffer, "-");
        chars_written += sprintf(buffer + chars_written, "%u", (uint32_t)(x % (uint64_t)1000));
    }
    return chars_written;
}

inline int _sprint__int64(char* buffer, int64_t x)
{
    if (x < (int64_t)0)
        return _sprint__uint64(buffer, (uint64_t)-x, true);
    else
        return _sprint__uint64(buffer, (uint64_t)x, false);
}

char *get_xs_double_lexical_representation(char *s, double d)
{
    return get_xs_double_lexical_representation_Saxon(s, d);
}

char *get_xs_float_lexical_representation(char *s, float f)
{
    return get_xs_float_lexical_representation_Saxon(s, f);
}

char *get_xs_integer_lexical_representation(char *s, int64_t v)
{
    _sprint__int64(s, v);
    return s;
}

char *get_xs_boolean_lexical_representation(char *s, bool b)
{
    return b ? strcpy(s, "true") : strcpy(s, "false");
}

char *get_xs_dateTime_lexical_representation(char *s, const XMLDateTime &d)
{
    d.get_string_value(s);
    return s;
}

char *get_lexical_representation_for_fixed_size_atomic(char *s, const tuple_cell &c)
{
    U_ASSERT(c.is_light_atomic());

    switch (c.get_atomic_type())
    {
        case xs_gYearMonth        :
        case xs_gYear             :
        case xs_gMonthDay         :
        case xs_gDay              :
        case xs_gMonth            :
        case xs_dateTime          :
        case xs_time              :
        case xs_date              : return get_xs_dateTime_lexical_representation(s, XMLDateTime(c.get_xs_dateTime(), c.get_atomic_type()));
        case xs_duration          :
        case xs_yearMonthDuration :
        case xs_dayTimeDuration   : return get_xs_dateTime_lexical_representation(s, XMLDateTime(c.get_xs_duration(), c.get_atomic_type()));
        case xs_boolean           : return get_xs_boolean_lexical_representation(s, c.get_xs_boolean());
        case xs_float             : return get_xs_float_lexical_representation(s, c.get_xs_float());
        case xs_double            : return get_xs_double_lexical_representation(s, c.get_xs_double());
        case xs_decimal           : return c.get_xs_decimal().get_c_str(s);
        case xs_nonPositiveInteger:
        case xs_negativeInteger   :
        case xs_long              :
        case xs_int               :
        case xs_short             :
        case xs_byte              :
        case xs_nonNegativeInteger:
        case xs_unsignedLong      :
        case xs_unsignedInt       :
        case xs_unsignedShort     :
        case xs_unsignedByte      :
        case xs_positiveInteger   :
        case xs_integer           : return get_xs_integer_lexical_representation(s, c.get_xs_integer());
        default                   : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to get_lexical_representation_for_fixed_size_atomic");
    }
}


/////////////////////////////////////////////////////////////////////////
/// XML Schema fixed datatypes to fixed datatypes conversion routines.
/////////////////////////////////////////////////////////////////////////
int64_t xs_float2xs_integer(float v)
{
    if (u_is_neg_inf((double)v) || u_is_pos_inf((double)v) || u_is_nan((double)v))
        throw XQUERY_EXCEPTION2(FOCA0002, "Error casting xs:float value to xs:integer");

    double i = 0.0;
    modf((double)v, &i);
    return (int64_t)i;
}

int64_t xs_double2xs_integer(double v)
{
    if (u_is_neg_inf(v) || u_is_pos_inf(v) || u_is_nan(v))
        throw XQUERY_EXCEPTION2(FOCA0002, "Error casting xs:double value to xs:integer");

    if(v > INT64_MAX || v < INT64_MIN)
        throw XQUERY_EXCEPTION2(FOCA0003, "Error casting xs:double value to xs:integer (too long value given)");

    double i = 0.0;
    modf(v, &i);
    return (int64_t)i;
}


/////////////////////////////////////////////////////////////////////////
/// ANumeric operations.
/////////////////////////////////////////////////////////////////////////

int64_t _double_NaN = ((int64_t)0x7FF00000 << (int64_t)32) | 0x1;
int64_t _double_Neg_INF = ((int64_t)0xFFF00000 << (int64_t)32);
int64_t _double_Pos_INF = ((int64_t)0x7FF00000 << (int64_t)32);
int32_t _float_NaN = 0x7F800001;
int32_t _float_Neg_INF = 0xFF800000;
int32_t _float_Pos_INF = 0x7F800000;


double xs_divide(double x, double y)
{
    if (y == 0.0)
    {
        if (x == 0.0) return double_NaN;
        int s = (double_sign(x) ? -1 : 1) * (double_sign(y) ? -1 : 1);
        return s * double_Pos_INF;
    }

    if ((u_is_neg_inf(x) || u_is_pos_inf(x)) && (u_is_neg_inf(y) || u_is_pos_inf(y)))
        return double_NaN;

    return x / y;
}

float xs_divide(float x, float y)
{
    if ((double)y == 0.0)
    {
        if (x == 0.0) return float_NaN;
        int s = (double_sign((double)x) ? -1 : 1) * (double_sign((double)y) ? -1 : 1);
        return s * float_Pos_INF;
    }

    if ((u_is_neg_inf((double)x) || u_is_pos_inf((double)x)) && (u_is_neg_inf((double)y) || u_is_pos_inf((double)y)))
        return float_NaN;

    return x / y;
}

xs_decimal_t xs_divide(int64_t x, int64_t y)
{
    if (y == 0) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-divide");
    return xs_decimal_t(x) / xs_decimal_t(y);
}

xs_decimal_t xs_divide(xs_decimal_t x, xs_decimal_t y)
{
    if (y.is_zero()) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-divide");
    return x / y;
}

int64_t xs_integer_divide(double x, double y)
{
    if (y == 0.0)
        throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-integer-divide");

    if (u_is_nan(x) || u_is_nan(y) || u_is_neg_inf(x) || u_is_pos_inf(x))
        throw XQUERY_EXCEPTION(FOAR0002);

    return xs_double2xs_integer(x / y);
}

int64_t xs_integer_divide(float x, float y)
{
    if ((double)y == 0.0)
        throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-integer-divide");

    if (u_is_nan((double)x) || u_is_nan((double)y) || u_is_neg_inf((double)x) || u_is_pos_inf((double)x))
        throw XQUERY_EXCEPTION(FOAR0002);

    return xs_float2xs_integer(x / y);
}

int64_t xs_integer_divide(int64_t x, int64_t y)
{
    if (y == 0) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-integer-divide");
    return x / y;
}

int64_t xs_integer_divide(xs_decimal_t x, xs_decimal_t y)
{
    if (y.is_zero()) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-integer-divide");
    return (x / y).get_int();
}

double xs_mod(double x, double y)
{
    if (u_is_nan(x) || u_is_nan(y) || u_is_neg_inf(x) || u_is_pos_inf(x) || y == 0.0)
        return double_NaN;

    if (x == 0.0 || u_is_neg_inf(y) || u_is_pos_inf(y))
        return x;

    return fmod(x, y);
}

float xs_mod(float x, float y)
{
    if (u_is_nan((double)x) || u_is_nan((double)y) || u_is_neg_inf((double)x) || u_is_pos_inf((double)x) || (double)y == 0.0)
        return float_NaN;

    if ((double)x == 0.0 || u_is_neg_inf((double)y) || u_is_pos_inf((double)y))
        return x;

    return fmodf(x, y);
}

int64_t xs_mod(int64_t x, int64_t y)
{
    if (y == 0) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-mod");
    return x % y;
}

xs_decimal_t xs_mod(xs_decimal_t x, xs_decimal_t y)
{
    if (y.is_zero()) throw XQUERY_EXCEPTION2(FOAR0001, "Division by zero in op:numeric-mod");
    return x % y;
}


double round_half_to_even_double(double d, int64_t precision)
{
    double m_i = 0, m_f = 0;
    int64_t y = 1;

    int64_t p = precision < 0 ? -precision : precision;
    for (int64_t j = 0; j < p; j++) y *= 10;

    if (precision < 0)
    {
        m_f = modf(d / y, &m_i);
        return m_i * y;
    }
    else
    {
        double i = 0;
        int s = 0;
        d = d < 0 ? (s = -1, -d) : (s = 1, d);
        double f = modf(d, &i);

        if (y == 1) {
            m_i = i;
            i = 0;
            m_f = f;
        } else {
            m_f = modf(f * y, &m_i);
        }

        if (m_f == 0.5)
        {
            if (((int64_t)m_i % 2) == 1)
            {
                m_i += 1;
            }
        }
        else if (m_f > 0.5)
        {
            m_i += 1;
        }

        return s * (i + m_i / y);
    }
}

float round_half_to_even_float(float d, int64_t precision)
{
    return (float)round_half_to_even_double((float)d, precision);
}

int64_t round_half_to_even_integer(int64_t d, int64_t precision)
{
    if (precision < 0)
    {
        int64_t y = 1;
        for (int64_t j = 0; j < -precision; j++) y *= 10;

        return (d / y) * y;
    }
    else
    {
        return d;
    }
}



/*******************************************************************************
 * XML Schema datatypes normalization routines
 ******************************************************************************/

template <class Iterator>
static inline void _replace_normalization(Iterator &start, const Iterator &end, stmt_str_buf& out_buf)
{
    unsigned char value;
    int64_t spaces_counter = 0;

    while(start < end && IS_WHITESPACE(*start)) { start++; }

    while(start < end)
    {
        value = *start++;
        if (IS_WHITESPACE(value)) spaces_counter++;
        else
        {
            while(spaces_counter) { out_buf << ' '; spaces_counter--; }
            out_buf << value;
        }
    }
}

template <class Iterator>
static inline void _collapse_normalization(Iterator &start, const Iterator &end, stmt_str_buf& out_buf)
{
    unsigned char value;
    bool is_space = false;

    while(start < end && IS_WHITESPACE(*start)) { start++; }

    while(start < end)
    {
        value = *start++;
        if (IS_WHITESPACE(value)) is_space = true;
        else
        {
            if(is_space) { out_buf << ' '; is_space = false; }
            out_buf << value;
        }
    }
}

template <class Iterator>
static inline void _remove_normalization(Iterator &start, const Iterator &end, stmt_str_buf& out_buf)
{
    while(start < end &&  IS_WHITESPACE(*start)) { start++; }
    while(start < end && !IS_WHITESPACE(*start)) { out_buf << (*start++); }
}


void replace_string_normalization (const tuple_cell *tc, stmt_str_buf& out_buf)
{
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(_replace_normalization, tc, out_buf);
}

void collapse_string_normalization(const tuple_cell *tc, stmt_str_buf& out_buf)
{
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(_collapse_normalization, tc, out_buf);
}

void remove_string_normalization  (const tuple_cell *tc, stmt_str_buf& out_buf)
{
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(_remove_normalization, tc, out_buf);
}



void replace_string_normalization (const char *s, stmt_str_buf& out_buf)
{
    _replace_normalization<const char*> (s, s + strlen(s), out_buf);
}

void collapse_string_normalization(const char *s, stmt_str_buf& out_buf)
{
    _collapse_normalization<const char*> (s, s + strlen(s), out_buf);
}

void remove_string_normalization  (const char *s, stmt_str_buf& out_buf)
{
    _remove_normalization<const char*> (s, s + strlen(s), out_buf);
}

