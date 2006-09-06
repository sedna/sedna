/*
 * File:  xs_helper.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "xs_helper.h"
#include "e_string.h"
#include "utils.h"

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
        int size = c.get_strlen_vmm();
        if (size > MAX_MEM_STR_SIZE) throw USER_EXCEPTION2(SE1003, "Buffer overflow");
        t = tr_globals::mem_str_buf;
        e_str_copy_to_buffer(t, c.get_str_vmm(), size);
        t[size] = '\0';
    }
    else 
    { // On-line memory is used
        t = c.get_str_mem();
    }

    return t;
}

float c_str2xs_float(const char *t)
{
    float res = 0.0;

    if (strcmp(t, "NaN") == 0)
    {
        __int32  x = 0x7F800001;
        res = *(float*)&x;
    }
    else if (strcmp(t, "-INF") == 0)
    {
        __int32 x = 0xFF800000;
        res = *(float*)&x;
    }
    else if (strcmp(t, "INF") == 0)
    {
        __int32 x = 0x7F800000;
        res = *(float*)&x;
    }
    else
    {
        char* stop;
        res = (float)strtod(t, &stop);
        if (*stop != '\0') throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:float type");
    }

    return res;
}

double c_str2xs_double(const char *t)
{
    double res = 0.0;

    if (strcmp(t, "NaN") == 0)
    {
        __int64  x = ((__int64)0x7FF00000 << (__int64)32) | 0x1;
        res = *(double*)&x;
    }
    else if (strcmp(t, "-INF") == 0)
    {
        __int64 x = ((__int64)0xFFF00000 << (__int64)32);
        res = *(double*)&x;
    }
    else if (strcmp(t, "INF") == 0)
    {
        __int64 x = ((__int64)0x7FF00000 << (__int64)32);
        res = *(double*)&x;
    }
    else
    {
        char* stop;
        res = strtod(t, &stop);
        if (*stop != '\0') throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:double type");
    }

    return res;
}

__int64 c_str2xs_integer(const char *t)
{
    char* stop;
    __int64 res = strto__int64(t, &stop, 10);
    if (*stop != '\0') throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:integer type");

    return res;
}

bool c_str2xs_boolean(const char *t)
{
    bool res;
    if (_stricmp(t, "true") == 0 || _stricmp(t, "1") == 0) res = true;
    else if (_stricmp(t, "false") == 0 || _stricmp(t, "0") == 0) res = false;
    else throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:boolean type");

    return res;
}


tuple_cell cast_string_type_to_xs_dateTime(const tuple_cell &c, xmlscm_type xtype)
{
    char *t = _get_pointer_to_c_str(c);
    XMLDateTime res;

    switch(xtype)
    {
        case xs_gYearMonth        : res.parseYearMonth(t); break;
        case xs_gYear             : res.parseYear(t); break;
        case xs_gMonthDay         : res.parseMonthDay(t); break;
        case xs_gDay              : res.parseDay(t); break;
        case xs_gMonth            : res.parseMonth(t); break;
        case xs_dateTime          : res.parseDateTime(t); break;
        case xs_time              : res.parseTime(t); break;
        case xs_date              : res.parseDate(t); break;
        case xs_duration          : res.parseDuration(t); break;
        case xs_yearMonthDuration : res.parseYearMonthDuration(t); break;
        case xs_dayTimeDuration   : res.parseDayTimeDuration(t); break;
        default                   : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_dateTime");
    }
		
    return tuple_cell::atomic(xtype, res.getRawData());
}



/*******************************************************************************
 * xs datatype to string conversion functions
 ******************************************************************************/

#define udouble2_int64_bits(p)
#define FPC_DOUBLESIGNMASK  ((__int64)0x8000 << (__int64)48) // 0x8000000000000000L

static __int64 double2__int64_bits(double d)
{
    union {
    __int64 l;
    double  d;
    } u;

    if (u_is_nan(d)) 
    {
        u.l = __int64(0x7FF80000);
        return u.l << 32;
    }
    udouble2_int64_bits(&d);
    u.d = d;
    return u.l;
}

static int _sprint__uint64(char* buffer, __uint64 x, bool m) 
{
    __uint64 quot = x / (__uint64)1000;
    int chars_written = 0;
    if (quot != 0) 
    {
        chars_written = _sprint__uint64(buffer, quot, m);
        chars_written += sprintf(buffer + chars_written, "%03u", (__uint32)(x % (__uint64)1000));
    }
    else 
    {
        if (m)
            chars_written += sprintf(buffer, "-");
        chars_written += sprintf(buffer + chars_written, "%u", (__uint32)(x % (__uint64)1000));
    }
    return chars_written;
}

inline int _sprint__int64(char* buffer, __int64 x)
{
    if (x < (__int64)0) 
        return _sprint__uint64(buffer, (__uint64)-x, true);
    else 
        return _sprint__uint64(buffer, (__uint64)x, false);
}


char *get_xs_double_lexical_representation(char *s, double d)
{
    if (u_is_neg_inf(d))
        strcpy(s, "-INF");
    else if (u_is_pos_inf(d))
        strcpy(s, "INF");
    else if (u_is_nan(d))
        strcpy(s, "NaN");
    else if (d == 0.0) 
    {
        if ((double2__int64_bits(d) & FPC_DOUBLESIGNMASK) != 0) 
            strcpy(s, "-0");
        else
            strcpy(s, "0");
    } 
    else 
    {
        double d_abs = d < 0 ? -d : d;
        if ((d_abs >= 1000000 || d_abs < 0.000001))
        { // exponential
            sprintf(s, "%#.12E", d);
            int len = strlen(s);
            int i = 0;
            int E_pos = 0;
            int shift = 0;
    
            while (E_pos < len && s[E_pos] != 'E') ++E_pos;
            if (E_pos == len) // Hm, it seems to be impossible case. Quit and left it as printf done...
                return s;
    
            for (i = E_pos - 1; i > 0; i--)
                if (s[i] == '0' && s[i - 1] != '.') s[i] = 'X';
                else break;
    
            for (i = E_pos + 1; i < len; i++)
                if (s[i] == '+' || s[i] == '0') s[i] = 'X';
                else if (s[i] == '-') ;
                else break;
            if (i == len && (E_pos + 1) < len) s[E_pos + 1] = '0';
    
            for (i = 0; i < len; i++)
                if (s[i] == 'X') shift++;
                else s[i - shift] = s[i];
            s[len - shift] = '\0';
            
        }
        else
        { // decimal
            sprintf(s, "%#.12f", d);
            // remove trailing zeros
            int len = strlen(s);
            int i = len - 1;
            while (i >= 0 && s[i] == '0') s[i--] = '\0';
            if (i >= 0 && s[i] == '.') s[i] = '\0';
        }
    }

    return s;
}

char *get_xs_integer_lexical_representation(char *s, __int64 v)
{
    _sprint__int64(s, v);
    return s;
}

char *get_xs_boolean_lexical_representation(char *s, bool b)
{
    return b ? strcpy(s, "true") : strcpy(s, "false");
}

char *get_xs_dateTime_lexical_representation(char *s, const XMLDateTime &d, xmlscm_type xtype)
{
    d.get_string_value(s);
    return s;
}
