/*
 * File:  casting_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "casting_operations.h"
#include "e_string.h"
#include "utils.h"

tuple_cell cast_string_type_to_xs_float(const tuple_cell &c)
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

    char* stop;
    float res = (float)strtod(t, &stop);
    if (*stop != '\0') throw USER_EXCEPTION2(FOCA0002, "Cannot convert to xs:float type"); // !!! this error code is not correct...

    return tuple_cell::atomic(res);
}

tuple_cell cast_string_type_to_xs_double(const tuple_cell &c)
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

    char* stop;
    double res = strtod(t, &stop);
    if (*stop != '\0') throw USER_EXCEPTION2(FOCA0002, "Cannot convert to xs:double type"); // !!! this error code is not correct...

    return tuple_cell::atomic(res);
}

tuple_cell cast_string_type_to_xs_decimal(const tuple_cell &c)
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

    return tuple_cell::atomic(decimal(t));
}

tuple_cell cast_string_type_to_xs_integer(const tuple_cell &c)
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

    char* stop;
    int res = strtol(t, &stop, 10);
    if (*stop != '\0') throw USER_EXCEPTION2(FOCA0002, "Cannot convert to xs:integer type"); // !!! this error code is not correct...

    return tuple_cell::atomic(res);
}

tuple_cell cast_string_type_to_xs_boolean(const tuple_cell &c)
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

    bool res;
    if (_stricmp(t, "true") == 0 || _stricmp(t, "1") == 0) res = true;
    else if (_stricmp(t, "false") == 0 || _stricmp(t, "0") == 0) res = false;
    else throw USER_EXCEPTION2(FOCA0002, "Cannot convert to xs:boolean type"); // !!! this error code is not correct...

    return tuple_cell::atomic(res);
}

tuple_cell cast_string_type_to_xs_dateTime(const tuple_cell &c, xmlscm_type xtype)
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

    XMLDateTime res;

    switch(xtype)
    {
        case xs_gYearMonth		: res.parseYearMonth(t); break;
        case xs_gYear			: res.parseYear(t); break;
        case xs_gMonthDay		: res.parseMonthDay(t); break;
        case xs_gDay			: res.parseDay(t); break;
        case xs_gMonth			: res.parseMonth(t); break;
        case xs_dateTime		: res.parseDateTime(t); break;
        case xs_time			: res.parseTime(t); break;
	case xs_date			: res.parseDate(t); break;
        case xs_duration		: res.parseDuration(t); break;
        case xdt_yearMonthDuration	: res.parseYearMonthDuration(t); break;
        case xdt_dayTimeDuration	: res.parseDayTimeDuration(t); break;
        default				: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_to_xs_dateTime");
    }
		
    return tuple_cell::atomic(xtype, res.getRawData());
}

tuple_cell cast_to_xdt_untypedAtomic(const tuple_cell &c)
{
    tuple_cell tc(cast_to_xs_string(c));
    tc.set_xtype(xdt_untypedAtomic);
    return tc;
}

tuple_cell cast_to_xs_QName(const tuple_cell &c)
{
    if (!c.is_atomic()) throw USER_EXCEPTION2(SE1003, "Cannot apply cast to none-atomic value");

    switch (c.get_atomic_type())
    {
        case xdt_untypedAtomic	: {
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
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: {
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
        case xdt_yearMonthDuration	:
        case xdt_dayTimeDuration	: {
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
                                      char tmp[MAX_MEM_STR_SIZE + 1];
                                      int size = sprintf(tmp, "%f", c.get_xs_float());
                                      return tuple_cell::atomic_deep(xs_string, tmp);
                                  }
        case xs_double			: {
                                      char tmp[MAX_MEM_STR_SIZE + 1];
                                      int size = sprintf(tmp, "%e", c.get_xs_double());
                                      return tuple_cell::atomic_deep(xs_string, tmp);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_float(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_double(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration		: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_decimal(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_integer(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_boolean(c);
        case xs_gYearMonth		: throw USER_EXCEPTION(FORG0001);
        case xs_gYear			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonthDay		: throw USER_EXCEPTION(FORG0001);
        case xs_gDay			: throw USER_EXCEPTION(FORG0001);
        case xs_gMonth			: throw USER_EXCEPTION(FORG0001);
        case xs_dateTime		: throw USER_EXCEPTION(FORG0001);
        case xs_time			: throw USER_EXCEPTION(FORG0001);
        case xs_date			: throw USER_EXCEPTION(FORG0001);
        case xs_duration		: throw USER_EXCEPTION(FORG0001);
        case xdt_yearMonthDuration	: throw USER_EXCEPTION(FORG0001);
        case xdt_dayTimeDuration	: throw USER_EXCEPTION(FORG0001);
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
        case xdt_untypedAtomic	: return cast_string_type_to_xs_dateTime(c,type);
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
						case xdt_yearMonthDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xdt_dayTimeDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						default: throw USER_EXCEPTION(FORG0001);
					  }
					}
	case xdt_yearMonthDuration	: {
					switch (type)
					  {
						case xs_duration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xdt_yearMonthDuration: return c;
						case xdt_dayTimeDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						default: throw USER_EXCEPTION(FORG0001);
					  }
					}
	case xdt_dayTimeDuration	: {
					switch (type)
					  {
						case xs_duration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData());
						case xdt_yearMonthDuration: return tuple_cell::atomic(type, c.get_xs_dateTime().convertTo(type).getRawData()); 
						case xdt_dayTimeDuration: return c;
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
        case xdt_untypedAtomic	: return cast_to_xdt_untypedAtomic(c);
        case xs_gYearMonth		: return cast_to_xs_dateTime(c,xtype);
        case xs_gYear			: return cast_to_xs_dateTime(c,xtype);
        case xs_gMonthDay		: return cast_to_xs_dateTime(c,xtype);
        case xs_gDay			: return cast_to_xs_dateTime(c,xtype);
        case xs_gMonth			: return cast_to_xs_dateTime(c,xtype);
        case xs_dateTime		: return cast_to_xs_dateTime(c,xtype);
        case xs_time			: return cast_to_xs_dateTime(c,xtype);
        case xs_date			: return cast_to_xs_dateTime(c,xtype);
        case xs_duration		: return cast_to_xs_dateTime(c,xtype);
        case xdt_yearMonthDuration	: return cast_to_xs_dateTime(c,xtype);
        case xdt_dayTimeDuration	: return cast_to_xs_dateTime(c,xtype);
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
