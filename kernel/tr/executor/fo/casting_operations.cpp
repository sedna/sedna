/*
 * File:  casting_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "casting_operations.h"
#include "e_string.h"
#include "utils.h"

/*******************************************************************************
 * INTERNAL FUNCTIONS
 ******************************************************************************/

// Funtion returns pointer to C string representing value.
// Pointer is valid only for a short period of time.
static char *_get_pointer_to_c_str(const tuple_cell &c)
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

static tuple_cell cast_string_type_to_xs_float(const tuple_cell &c)
{
    char *t = _get_pointer_to_c_str(c);
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

    return tuple_cell::atomic(res);
}

static tuple_cell cast_string_type_to_xs_double(const tuple_cell &c)
{
    char *t = _get_pointer_to_c_str(c);
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

    return tuple_cell::atomic(res);
}

static tuple_cell cast_string_type_to_xs_decimal(const tuple_cell &c)
{
    char *t = _get_pointer_to_c_str(c);
    return tuple_cell::atomic(decimal(t));
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!! __int64 should be used here
static tuple_cell cast_string_type_to_xs_integer(const tuple_cell &c)
{
    char *t = _get_pointer_to_c_str(c);
    char* stop;
    int res = strtol(t, &stop, 10);
    if (*stop != '\0') throw USER_EXCEPTION2(FOCA0002, "Cannot convert to xs:integer type"); // !!! this error code is not correct...

    return tuple_cell::atomic(res);
}

static tuple_cell cast_string_type_to_xs_boolean(const tuple_cell &c)
{
    char *t = _get_pointer_to_c_str(c);
    bool res;
    if (_stricmp(t, "true") == 0 || _stricmp(t, "1") == 0) res = true;
    else if (_stricmp(t, "false") == 0 || _stricmp(t, "0") == 0) res = false;
    else throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:boolean type");

    return tuple_cell::atomic(res);
}

static tuple_cell cast_string_type_to_xs_dateTime(const tuple_cell &c, xmlscm_type xtype)
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

static char *get_xs_double_lexical_representation(char *s, double d)
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

// the function returns pointer to the static buffer (so the function is not thread safe)
inline char *get_xs_double_lexical_representation(double d)
{
    return get_xs_double_lexical_representation(tr_globals::mem_str_buf, d);
}



/*******************************************************************************
 * XML QUERY CAST FUNCTIONS
 ******************************************************************************/
tuple_cell cast_to_xs_untypedAtomic(const tuple_cell &c)
{
    tuple_cell tc(cast_to_xs_string(c));
    tc.set_xtype(xs_untypedAtomic);
    return tc;
}

tuple_cell cast_to_xs_QName(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: {
                                      // !!! FIXME: check lexical representation
                                      tuple_cell res(c);
                                      res.set_xtype(xs_QName);
                                      return res;
                                  }
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: throw USER_EXCEPTION(FORG0001);
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: throw USER_EXCEPTION(FORG0001);
        case xs_double			: throw USER_EXCEPTION(FORG0001);
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: return c;
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: {
                                      // !!! FIXME: check lexical representation
                                      tuple_cell res(c);
                                      res.set_xtype(xs_QName);
                                      return res;
                                  }
        case xs_decimal			: throw USER_EXCEPTION(FORG0001);
        case xs_integer 		: throw USER_EXCEPTION(FORG0001);
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_QName");
    }
}

tuple_cell cast_to_xs_string(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: {
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        case xs_gYearMonth		: 
        case xs_gYear			:
        case xs_gMonthDay		:
        case xs_gDay			:
        case xs_gMonth			:
        case xs_dateTime		:
        case xs_time			:
        case xs_date			:
        case xs_duration		:
        case xs_yearMonthDuration	:
        case xs_dayTimeDuration	: {
                                      char tmp[MAX_MEM_STR_SIZE + 1];
                                      c.get_xs_dateTime().get_string_value(tmp);
                                      return tuple_cell::atomic_deep(xs_string, tmp);
                                  }
        case xs_boolean			: {
                                      if (c.get_xs_boolean())
                                          return tuple_cell::atomic_deep(xs_string, "true");
                                      else 
                                          return tuple_cell::atomic_deep(xs_string, "false");
                                  }
        case xs_base64Binary	: throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented");
        case xs_hexBinary		: throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented");
        case xs_float			: {
                                      char *res =get_xs_double_lexical_representation((float)c.get_xs_float());
                                      return tuple_cell::atomic_deep(xs_string, res);
                                  }
        case xs_double			: {
                                      char *res =get_xs_double_lexical_representation(c.get_xs_double());
                                      return tuple_cell::atomic_deep(xs_string, res);
                                  }
        case xs_anyURI			: {
                                      // !!! FIXME:  some conversion needed
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION2(SE1002, "Cast for some typed is not implemented");
        case xs_string			: return c;
        case xs_decimal			: {
                                      char tmp[MAX_MEM_STR_SIZE + 1];
                                      int size = c.get_xs_decimal().get_string_value(tmp);
                                      return tuple_cell::atomic_deep(xs_string, tmp);
                                  }
        case xs_integer 		: {
                                      char tmp[MAX_MEM_STR_SIZE + 1];
                                      int size = sprintf(tmp, "%d", c.get_xs_integer());
                                      return tuple_cell::atomic_deep(xs_string, tmp);
                                  }
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_string");
    }
}

tuple_cell cast_to_xs_float(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_float(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: if (c.get_xs_boolean()) return tuple_cell::atomic((float)1);
                                  else return tuple_cell::atomic((float)0);
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: return c;
        case xs_double			: return tuple_cell::atomic((float)(c.get_xs_double()));
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_float(c);
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().to_float());
        case xs_integer 		: return tuple_cell::atomic((float)(c.get_xs_integer()));
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_float");
    }
}

tuple_cell cast_to_xs_double(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_double(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration		: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: if (c.get_xs_boolean()) return tuple_cell::atomic((double)1);
                                  else return tuple_cell::atomic((double)0);
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: return tuple_cell::atomic((double)(c.get_xs_float()));
        case xs_double			: return c;
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_double(c);
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().to_double());
        case xs_integer 		: return tuple_cell::atomic((double)(c.get_xs_integer()));
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_double");
    }
}

tuple_cell cast_to_xs_decimal(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_decimal(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: if (c.get_xs_boolean()) return tuple_cell::atomic(decimal(1.0));
                                  else return tuple_cell::atomic(decimal(0.0));
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: return tuple_cell::atomic(decimal(c.get_xs_float()));
        case xs_double			: return tuple_cell::atomic(decimal(c.get_xs_double()));
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_decimal(c);
        case xs_decimal			: return c;
        case xs_integer 		: return tuple_cell::atomic(decimal(c.get_xs_integer()));
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_decimal");
    }
}

tuple_cell cast_to_xs_integer(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_integer(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: if (c.get_xs_boolean()) return tuple_cell::atomic(1);
                                  else return tuple_cell::atomic(0);
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: return tuple_cell::atomic((int)(floor(c.get_xs_float())));
        case xs_double			: return tuple_cell::atomic((int)(floor(c.get_xs_double())));
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_integer(c);
        case xs_decimal			: return tuple_cell::atomic((int)(floor(c.get_xs_decimal().to_double())));
        case xs_integer 		: return c;
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_integer");
    }
}

tuple_cell cast_to_xs_boolean(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_boolean(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xs_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
        case xs_boolean			: return c;
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: return c.get_xs_float() == (float)0 ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_double			: return c.get_xs_double() == (double)0 ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_boolean(c);
        case xs_decimal			: return c.get_xs_decimal().is_zero() ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_integer 		: return c.get_xs_integer() == 0 ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_boolean");
    }
}

tuple_cell cast_to_xs_dateTime(const tuple_cell &c, xmlscm_type type)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return cast_string_type_to_xs_dateTime(c,type);
        case xs_gYearMonth		: {
					if(type != xs_gYearMonth) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_gYear			: {
					if (type != xs_gYear) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_gMonthDay		: {
					if (type != xs_gMonthDay) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_gDay			: {
					if (type != xs_gDay) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_gMonth			: {
					if (type != xs_gMonth) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_dateTime		: {
					switch(type)
					  {
						case xs_gYear: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gYearMonth: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gMonth: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gDay: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_dateTime: return c;
						case xs_time: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_date: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						default:  throw USER_EXCEPTION(FORG0001);
					 }
					}
        case xs_time			: {
					if (type != xs_time) throw USER_EXCEPTION(FORG0001);
					else return c;
					}
        case xs_date			: {
					switch(type)
					  {
						case xs_gYear: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gYearMonth: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gMonth: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_gDay: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_dateTime: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_date: return c;
						default: throw USER_EXCEPTION(FORG0001);
					 }
					}
        case xs_duration		: {
					switch (type)
					  {
						case xs_duration: return c; 
						case xs_yearMonthDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_dayTimeDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						default: throw USER_EXCEPTION(FORG0001);
					  }
					}
	case xs_yearMonthDuration	: {
					switch (type)
					  {
						case xs_duration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_yearMonthDuration: return c;
						case xs_dayTimeDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						default: throw USER_EXCEPTION(FORG0001);
					  }
					}
	case xs_dayTimeDuration	: {
					switch (type)
					  {
						case xs_duration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xs_yearMonthDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData()); 
						case xs_dayTimeDuration: return c;
						default: throw USER_EXCEPTION(FORG0001);
					  }
					}
        case xs_boolean			: throw USER_EXCEPTION(FORG0001);
        case xs_base64Binary	: throw USER_EXCEPTION(FORG0001);
        case xs_hexBinary		: throw USER_EXCEPTION(FORG0001);
        case xs_float			: throw USER_EXCEPTION(FORG0001);
        case xs_double			: throw USER_EXCEPTION(FORG0001);
        case xs_anyURI			: throw USER_EXCEPTION(FORG0001);
        case xs_QName			: throw USER_EXCEPTION(FORG0001);
        case xs_NOTATION		: throw USER_EXCEPTION(FORG0001);
        case xs_string			: return cast_string_type_to_xs_dateTime(c,type);
        case xs_decimal			: throw USER_EXCEPTION(FORG0001);
        case xs_integer 		: throw USER_EXCEPTION(FORG0001);
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_date");
    }
}

tuple_cell cast(const tuple_cell &c, xmlscm_type xtype)
{
    switch (xtype)
    {
        case xs_untypedAtomic	: return cast_to_xs_untypedAtomic(c);
        case xs_gYearMonth		: return cast_to_xs_dateTime(c,xtype);
        case xs_gYear			: return cast_to_xs_dateTime(c,xtype);
        case xs_gMonthDay		: return cast_to_xs_dateTime(c,xtype);
        case xs_gDay			: return cast_to_xs_dateTime(c,xtype);
        case xs_gMonth			: return cast_to_xs_dateTime(c,xtype);
        case xs_dateTime		: return cast_to_xs_dateTime(c,xtype);
        case xs_time			: return cast_to_xs_dateTime(c,xtype);
        case xs_date			: return cast_to_xs_dateTime(c,xtype);
        case xs_duration		: return cast_to_xs_dateTime(c,xtype);
        case xs_yearMonthDuration	: return cast_to_xs_dateTime(c,xtype);
        case xs_dayTimeDuration	: return cast_to_xs_dateTime(c,xtype);
        case xs_boolean			: return cast_to_xs_boolean(c);
        case xs_base64Binary	: throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented");
        case xs_hexBinary		: throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented");
        case xs_float			: return cast_to_xs_float(c);
        case xs_double			: return cast_to_xs_double(c);
        case xs_anyURI			: throw USER_EXCEPTION2(SE1002, "Cast for some typed is not implemented");
        case xs_QName			: throw USER_EXCEPTION2(SE1002, "Cast for some typed is not implemented");
        case xs_NOTATION		: throw USER_EXCEPTION2(SE1002, "Cast for some typed is not implemented");
        case xs_string			: return cast_to_xs_string(c);
        case xs_decimal			: return cast_to_xs_decimal(c);
        case xs_integer 		: return cast_to_xs_integer(c);
        default					: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast");
    }
}

bool is_castable(const tuple_cell &c, xmlscm_type xtype)
{
	try
	{
		cast(c, xtype);	
		return true;
	}
	catch(SednaUserException &e)
	{
		return false;
	}
}
