/*
 * File:  casting_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>

#include "common/sedna.h"

#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/xs_binary.h"
#include "tr/executor/base/xs_names.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/xsd.h"


/******************************************************************************/

inline tuple_cell cast_string_type_to_xs_float(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_float(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_double(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_double(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_decimal(const tuple_cell &c)
    { return tuple_cell::atomic(xs_decimal_t(_get_pointer_to_c_str(c))); }

inline tuple_cell cast_string_type_to_xs_integer(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_integer(_get_pointer_to_c_str(c))); }

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

    if (xtype == xs_duration || xtype == xs_yearMonthDuration || xtype == xs_dayTimeDuration)
    	return tuple_cell::atomic(res.getPackedDuration(), xtype);
    else
   {
	res = XMLDateTime( res.getPackedDateTime(), xtype );
    	return tuple_cell::atomic(res.getPackedDateTime(), xtype);
   }
}

inline tuple_cell cast_string_type_to_xs_boolean(const tuple_cell &c)
    { return tuple_cell::atomic(c_str2xs_boolean(_get_pointer_to_c_str(c))); }


inline tuple_cell cast_string_type_to_xs_anyURI(const tuple_cell &c)
{ 
    bool valid = false;
    tuple_cell res;
    Uri::Information nfo;
    Uri::check_constraints(&c, &valid, &nfo);
    
    if(!valid) throw XQUERY_EXCEPTION2(FORG0001, "The value does not conform to the lexical constraints defined for the xs:anyURI type.");

    if(!nfo.normalized)
    {
        stmt_str_buf result;
        collapse_string_normalization(&c, result);
        res = result.get_tuple_cell();
    }
    else 
        res = c;

    res.set_xtype(xs_anyURI);
    return res;
}

inline tuple_cell cast_string_type_to_xs_QName(const tuple_cell &c)
{
    // we evaluate 'string literal'->xs:QName constructor at static analysis
    // all other string->xs:QName should be considered XQuery error (AK)
    //throw XQUERY_EXCEPTION2(SE1003, "Casting to xs:QName should be performed at compile time");
    throw XQUERY_EXCEPTION2(XPTY0004, "cannot cast xs:string to xs:QName dynamically");
/*
    // !!! FIXME: check lexical representation
    tuple_cell tmp = tuple_cell::make_sure_light_atomic(c);
    char *qname = xs_QName_create(tmp.get_str_mem(), tuple_char_alloc);
    return tuple_cell::atomic(xs_QName, qname);
*/
}


/******************************************************************************/

inline tuple_cell cast_xs_float_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_float_lexical_representation(executor_globals::mem_str_buf, c.get_xs_float());
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_double_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_double_lexical_representation(executor_globals::mem_str_buf, c.get_xs_double());
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_decimal_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    c.get_xs_decimal().get_c_str(executor_globals::mem_str_buf);
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_integer_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_integer_lexical_representation(executor_globals::mem_str_buf, c.get_xs_integer());
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_dateTime_to_string_type(const tuple_cell &c, xmlscm_type xtype, xmlscm_type res_type)
{
    if (xtype == xs_duration || xtype == xs_yearMonthDuration || xtype == xs_dayTimeDuration )
        get_xs_dateTime_lexical_representation(executor_globals::mem_str_buf, XMLDateTime(c.get_xs_duration(), xtype));
    else
        get_xs_dateTime_lexical_representation(executor_globals::mem_str_buf, XMLDateTime(c.get_xs_dateTime(), xtype));
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_boolean_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_boolean_lexical_representation(executor_globals::mem_str_buf, c.get_xs_boolean());
    return tuple_cell::atomic_deep(res_type, executor_globals::mem_str_buf);
}

inline tuple_cell cast_xs_anyURI_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{   
    tuple_cell res(c);
    res.set_xtype(res_type);
    return res;
}

inline tuple_cell cast_xs_QName_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    U_ASSERT(c.is_light_atomic()); // xs:QName value is always ligth atomic

    xsd::QName qname = xsd::QName::deserialize(c.get_str_mem());
    const char *prefix = qname.getPrefix();
    const char *local  = qname.getLocalName();
    char *res = NULL;

    if (prefix && strcmp(prefix, "") != 0)
    {
        res = se_new char[strlen(prefix) + strlen(local) + 2];
        strcpy(res, prefix);
        strcat(res, ":");
        strcat(res, local);
    }
    else
    {
        res = se_new char[strlen(local) + 1];
        strcpy(res, local);
    }
    return tuple_cell::atomic(res_type, res);
}



/******************************************************************************/

inline tuple_cell cast_xs_hexBinary_to_xs_base64Binary(const tuple_cell &c)
{ 
    //implementation in base64Binary.cpp
    return cast_hexBinary_to_base64Binary(c);
}

inline tuple_cell cast_xs_base64Binary_to_xs_hexBinary(const tuple_cell &c)
{ 
    //implementation in base64Binary.cpp
    return cast_base64Binary_to_hexBinary(c);
}



/******************************************************************************/

static tuple_cell _cast_is_not_supported(xmlscm_type from, xmlscm_type to)
{
    char buf[128];
    strcpy(buf, "Cast from ");
    strcat(buf, xmlscm_type2c_str(from));
    strcat(buf, " to ");
    strcat(buf, xmlscm_type2c_str(to));
    strcat(buf, " is not supported");
    throw XQUERY_EXCEPTION2(XPTY0004, buf);
}


/*******************************************************************************
 * CASTS FOR PRIMITIVE DATATYPES (CASTING TABLE IMPLEMENTATION)
 ******************************************************************************/
tuple_cell cast_primitive_to_xs_untypedAtomic(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: return c;
        case xs_string			: {
                                      tuple_cell res(c);
                                      res.set_xtype(xs_untypedAtomic);
                                      return res;
                                  }
        case xs_float			: return cast_xs_float_to_string_type(c, xs_untypedAtomic);
        case xs_double			: return cast_xs_double_to_string_type(c, xs_untypedAtomic);
        case xs_decimal			: return cast_xs_decimal_to_string_type(c, xs_untypedAtomic);
        case xs_integer 		: return cast_xs_integer_to_string_type(c, xs_untypedAtomic);
        case xs_duration		: return cast_xs_dateTime_to_string_type(c, xs_duration, xs_untypedAtomic);
        case xs_yearMonthDuration:return cast_xs_dateTime_to_string_type(c, xs_yearMonthDuration, xs_untypedAtomic);
        case xs_dayTimeDuration	: return cast_xs_dateTime_to_string_type(c, xs_dayTimeDuration, xs_untypedAtomic);
        case xs_dateTime		: return cast_xs_dateTime_to_string_type(c, xs_dateTime, xs_untypedAtomic);
        case xs_time			: return cast_xs_dateTime_to_string_type(c, xs_time, xs_untypedAtomic);
        case xs_date			: return cast_xs_dateTime_to_string_type(c, xs_date, xs_untypedAtomic);
        case xs_gYearMonth		: return cast_xs_dateTime_to_string_type(c, xs_gYearMonth, xs_untypedAtomic);
        case xs_gYear			: return cast_xs_dateTime_to_string_type(c, xs_gYear, xs_untypedAtomic);
        case xs_gMonthDay		: return cast_xs_dateTime_to_string_type(c, xs_gMonthDay, xs_untypedAtomic);
        case xs_gDay			: return cast_xs_dateTime_to_string_type(c, xs_gDay, xs_untypedAtomic);
        case xs_gMonth			: return cast_xs_dateTime_to_string_type(c, xs_gMonth, xs_untypedAtomic);
        case xs_boolean			: return cast_xs_boolean_to_string_type(c, xs_untypedAtomic);
        case xs_base64Binary	: 
        case xs_hexBinary		: {
                                      tuple_cell res(c);
                                      res.set_xtype(xs_untypedAtomic);
                                      return res;
                                  }
        case xs_anyURI			: return cast_xs_anyURI_to_string_type(c, xs_untypedAtomic);
        case xs_QName			: return cast_xs_QName_to_string_type(c, xs_untypedAtomic);
        default                 : return _cast_is_not_supported(c.get_atomic_type(), xs_untypedAtomic);
    }
}

tuple_cell cast_primitive_to_xs_string(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: {
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        case xs_string			: return c;
        case xs_float			: return cast_xs_float_to_string_type(c, xs_string);
        case xs_double			: return cast_xs_double_to_string_type(c, xs_string);
        case xs_decimal			: return cast_xs_decimal_to_string_type(c, xs_string);
        case xs_integer 		: return cast_xs_integer_to_string_type(c, xs_string);
        case xs_duration		: return cast_xs_dateTime_to_string_type(c, xs_duration, xs_string);
        case xs_yearMonthDuration:return cast_xs_dateTime_to_string_type(c, xs_yearMonthDuration, xs_string);
        case xs_dayTimeDuration	: return cast_xs_dateTime_to_string_type(c, xs_dayTimeDuration, xs_string);
        case xs_dateTime		: return cast_xs_dateTime_to_string_type(c, xs_dateTime, xs_string);
        case xs_time			: return cast_xs_dateTime_to_string_type(c, xs_time, xs_string);
        case xs_date			: return cast_xs_dateTime_to_string_type(c, xs_date, xs_string);
        case xs_gYearMonth		: return cast_xs_dateTime_to_string_type(c, xs_gYearMonth, xs_string);
        case xs_gYear			: return cast_xs_dateTime_to_string_type(c, xs_gYear, xs_string);
        case xs_gMonthDay		: return cast_xs_dateTime_to_string_type(c, xs_gMonthDay, xs_string);
        case xs_gDay			: return cast_xs_dateTime_to_string_type(c, xs_gDay, xs_string);
        case xs_gMonth			: return cast_xs_dateTime_to_string_type(c, xs_gMonth, xs_string);
        case xs_boolean			: return cast_xs_boolean_to_string_type(c, xs_string);
        case xs_base64Binary	: 
        case xs_hexBinary		: {
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        case xs_anyURI			: return cast_xs_anyURI_to_string_type(c, xs_string);
        case xs_QName			: return cast_xs_QName_to_string_type(c, xs_string);
        case xs_NOTATION		: { // !!! FIX ME: don't know what to do
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_string);
    }
}

tuple_cell cast_primitive_to_xs_float(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_float(c);
        case xs_float			: return c;
        case xs_double			: return tuple_cell::atomic((float)(c.get_xs_double()));
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().get_float());
        case xs_integer 		: return tuple_cell::atomic((float)(c.get_xs_integer()));
        case xs_boolean			: return c.get_xs_boolean() ? tuple_cell::atomic((float)1.0e0)
                                                            : tuple_cell::atomic((float)0.0e0);
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_float);
    }
}

tuple_cell cast_primitive_to_xs_double(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_double(c);
        case xs_float			: return tuple_cell::atomic((double)(c.get_xs_float()));
        case xs_double			: return c;
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().get_double());
        case xs_integer 		: return tuple_cell::atomic((double)(c.get_xs_integer()));
        case xs_boolean			: return c.get_xs_boolean() ? tuple_cell::atomic((double)1.0e0)
                                                            : tuple_cell::atomic((double)0.0e0);
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_double);
    }
}

tuple_cell cast_primitive_to_xs_decimal(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_decimal(c);
        case xs_float			: return tuple_cell::atomic(xs_decimal_t(c.get_xs_float()));
        case xs_double			: return tuple_cell::atomic(xs_decimal_t(c.get_xs_double()));
        case xs_decimal			: return c;
        case xs_integer 		: return tuple_cell::atomic(xs_decimal_t(c.get_xs_integer()));
        case xs_boolean			: return tuple_cell::atomic(xs_decimal_t(c.get_xs_boolean()));
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_decimal);
    }
}

tuple_cell cast_primitive_to_xs_integer(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_integer(c);
        case xs_float			: return tuple_cell::atomic(xs_float2xs_integer(c.get_xs_float()));
        case xs_double			: return tuple_cell::atomic(xs_double2xs_integer(c.get_xs_double()));
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().get_int());
        case xs_integer 		: return c;
        case xs_boolean			: return tuple_cell::atomic(xs_boolean2xs_integer(c.get_xs_boolean()));
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_integer);
    }
}

tuple_cell cast_xs_dateTime_to_xs_dateTime(const tuple_cell &c, xmlscm_type type)
{
	XMLDateTime dt1(c.get_xs_dateTime(), c.get_atomic_type());
	return tuple_cell::atomic(dt1.convertTo(type).getPackedDateTime(), type); 
}

tuple_cell cast_xs_duration_to_xs_duration(const tuple_cell &c, xmlscm_type type)
{
	XMLDateTime dt1(c.get_xs_duration(), c.get_atomic_type());
	return tuple_cell::atomic(dt1.convertTo(type).getPackedDuration(), type); 
}

tuple_cell cast_primitive_to_xs_dateTime(const tuple_cell &c, xmlscm_type type)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic    : 
        case xs_string           : return cast_string_type_to_xs_dateTime(c, type);
        case xs_duration         : 
        case xs_yearMonthDuration:
        case xs_dayTimeDuration  : 
            switch(type)
            {
                case xs_duration         :
                case xs_yearMonthDuration:
                case xs_dayTimeDuration  : return cast_xs_duration_to_xs_duration(c, type);
                default                  : return _cast_is_not_supported(c.get_atomic_type(), type);
            }
	
        case xs_dateTime         :
            switch(type)
            {
                case xs_dateTime         : return c;
                case xs_time             : 
                case xs_date             : 
                case xs_gYearMonth       : 
                case xs_gYear            : 
                case xs_gMonthDay        : 
                case xs_gDay             : 
                case xs_gMonth           : return cast_xs_dateTime_to_xs_dateTime(c, type);
                default                  : return _cast_is_not_supported(c.get_atomic_type(), type);
            }


        case xs_time             : if (type == c.get_atomic_type()) return c;
                                   else return _cast_is_not_supported(c.get_atomic_type(), type);
        case xs_date             :
            switch(type)
            {
                case xs_date             : return c;
                case xs_dateTime         : 
                case xs_gYearMonth       : 
                case xs_gYear            : 
                case xs_gMonthDay        : 
                case xs_gDay             : 
                case xs_gMonth           : return  cast_xs_dateTime_to_xs_dateTime(c, type);
                default                  : return _cast_is_not_supported(c.get_atomic_type(), type);
            }



        case xs_gYearMonth       : 
        case xs_gYear            :
        case xs_gMonthDay        : 
        case xs_gDay             : 
        case xs_gMonth           : if (type == c.get_atomic_type()) return c; 
                                   else return _cast_is_not_supported(c.get_atomic_type(), type);
        default					 : return _cast_is_not_supported(c.get_atomic_type(), type);
    }
}

tuple_cell cast_primitive_to_xs_boolean(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_boolean(c);
        case xs_float			: 
        {
            float value = c.get_xs_float();
            return (value == (float)0.0e0 || u_is_nan((double)value)) ? 
                   tuple_cell::atomic(false) : 
                   tuple_cell::atomic(true);
        }
        case xs_double			: 
        {
            double value = c.get_xs_double();
            return (value == (double)0.0e0 || u_is_nan(value)) ? 
                   tuple_cell::atomic(false) : 
                   tuple_cell::atomic(true);
        }
        case xs_decimal			: return c.get_xs_decimal().is_zero()        ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_integer 		: return c.get_xs_integer() == (int64_t)0    ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_boolean			: return c;
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_boolean);
    }
}

tuple_cell cast_primitive_to_xs_base64Binary(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_base64Binary(c);
        case xs_base64Binary	: return c;
        case xs_hexBinary		: return cast_xs_hexBinary_to_xs_base64Binary(c);
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_base64Binary);
    }
}

tuple_cell cast_primitive_to_xs_hexBinary(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_hexBinary(c);
        case xs_base64Binary	: return cast_xs_base64Binary_to_xs_hexBinary(c);
        case xs_hexBinary		: return c;
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_hexBinary);
    }
}

tuple_cell cast_primitive_to_xs_anyURI(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_anyURI(c);
        case xs_anyURI          : return c;
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_anyURI);
    }
}

tuple_cell cast_primitive_to_xs_QName(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_string			: return cast_string_type_to_xs_QName(c);
        case xs_QName			: return c;
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_QName);
    }
}

tuple_cell cast_primitive_to_xs_NOTATION(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    // Since we do not provide support for types derived from xs:NOTATION,
    // any casting to xs:NOTATION is prohibited

    return _cast_is_not_supported(c.get_atomic_type(), xs_NOTATION);
}

tuple_cell cast_primitive(const tuple_cell &c, xmlscm_type xtype)
{
    switch (xtype)
    {
        case xs_untypedAtomic    : return cast_primitive_to_xs_untypedAtomic(c);
        case xs_string           : return cast_primitive_to_xs_string(c);
        case xs_float            : return cast_primitive_to_xs_float(c);
        case xs_double           : return cast_primitive_to_xs_double(c);
        case xs_decimal          : return cast_primitive_to_xs_decimal(c);
        case xs_integer          : return cast_primitive_to_xs_integer(c);
        case xs_duration         : 
        case xs_yearMonthDuration: 
        case xs_dayTimeDuration  : 
        case xs_dateTime         : 
        case xs_time             : 
        case xs_date             : 
        case xs_gYearMonth       : 
        case xs_gYear            : 
        case xs_gMonthDay        : 
        case xs_gDay             : 
        case xs_gMonth           : return cast_primitive_to_xs_dateTime(c, xtype);
        case xs_boolean          : return cast_primitive_to_xs_boolean(c);
        case xs_base64Binary     : return cast_primitive_to_xs_base64Binary(c);
        case xs_hexBinary        : return cast_primitive_to_xs_hexBinary(c);
        case xs_anyURI           : return cast_primitive_to_xs_anyURI(c);
        case xs_QName            : return cast_primitive_to_xs_QName(c);
        case xs_NOTATION         : return cast_primitive_to_xs_NOTATION(c);
        default                  : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast");
    }
}

/*******************************************************************************
 * CAST FOR DERIVED DATATYPES: Casting within a branch of the type hierarchy
 ******************************************************************************/
static bool check_constraints_for_xs_nonPositiveInteger(const int64_t& value)
{
    return value <= 0;
}

static bool check_constraints_for_xs_negativeInteger(const int64_t& value)
{
    return value < 0;
}

static bool check_constraints_for_xs_long(const int64_t& value)
{
    // because int64_t fully conforms to xs:long...
    return true;
}

static bool check_constraints_for_xs_int(const int64_t& value)
{
    return value >= (int64_t)(-2147483647 - 1) && value <= (int64_t)2147483647;
}

static bool check_constraints_for_xs_short(const int64_t& value)
{
    return value >= (int64_t)-32768 && value <= (int64_t)32767;
}

static bool check_constraints_for_xs_byte(const int64_t& value)
{
    return value >= (int64_t)-128 && value <= (int64_t)127;
}

static bool check_constraints_for_xs_nonNegativeInteger(const int64_t& value)
{
    return value >= 0;
}

static bool check_constraints_for_xs_unsignedLong(const int64_t& value)
{
    return value >= 0;
}

static bool check_constraints_for_xs_unsignedInt(const int64_t& value)
{
    return value >= 0 && value <= (int64_t)4294967295UL;
}

static bool check_constraints_for_xs_unsignedShort(const int64_t& value)
{
    return value >= 0 && value <= (int64_t)65535;
}

static bool check_constraints_for_xs_unsignedByte(const int64_t& value)
{
    return value >= 0 && value <= (int64_t)255;
}

static bool check_constraints_for_xs_positiveInteger(const int64_t& value)
{
    return value > 0;
}

static inline bool check_constraints_for_xs_language(const tuple_cell *value)
{
	char const* regex = "^[\\n\\r\\t ]*[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*[\\n\\r\\t ]*$";
	return charset_handler->matches(value, regex);
}

static tuple_cell cast_within_a_branch(const tuple_cell &SV, xmlscm_type TT, xmlscm_type base_type)
{
    if (TT == base_type) return SV;

    // only xs:integer and xs:string have a branch
    U_ASSERT(base_type == xs_integer || base_type == xs_string);

    bool sat = false;
    bool need_string_normalization = false;
    
    if (base_type == xs_integer)
    {
        int64_t value = SV.get_xs_integer();

        switch (TT)
        {
            case xs_integer           : sat = true;                                               break;
            case xs_nonPositiveInteger: sat = check_constraints_for_xs_nonPositiveInteger(value); break;
            case xs_negativeInteger   : sat = check_constraints_for_xs_negativeInteger(value);    break;
            case xs_long              : sat = check_constraints_for_xs_long(value);               break;
            case xs_int               : sat = check_constraints_for_xs_int(value);                break;
            case xs_short             : sat = check_constraints_for_xs_short(value);              break;
            case xs_byte              : sat = check_constraints_for_xs_byte(value);               break;
            case xs_nonNegativeInteger: sat = check_constraints_for_xs_nonNegativeInteger(value); break;
            case xs_unsignedLong      : sat = check_constraints_for_xs_unsignedLong(value);       break;
            case xs_unsignedInt       : sat = check_constraints_for_xs_unsignedInt(value);        break;
            case xs_unsignedShort     : sat = check_constraints_for_xs_unsignedShort(value);      break;
            case xs_unsignedByte      : sat = check_constraints_for_xs_unsignedByte(value);       break;
            case xs_positiveInteger   : sat = check_constraints_for_xs_positiveInteger(value);    break;
            default                   : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_within_a_branch");
        }
    }
    else if (base_type == xs_string)
    {
        xmlscm_type ST = SV.get_atomic_type();
        need_string_normalization = (ST <= xs_normalizedString && TT > ST);
        
        switch (TT)
        {
            case xs_string            : 
            case xs_normalizedString  : 
       	    case xs_token             : sat = true; break;
            case xs_language          : sat = check_constraints_for_xs_language(&SV); break;
       	    case xs_NMTOKEN           : sat = check_constraints_for_xs_NMTOKEN(&SV);  break;
       	    case xs_Name              : sat = check_constraints_for_xs_Name(&SV);     break;
       	    case xs_NCName            : ///
       	    case xs_ID                : /// NCName, ID, IDREF, ENTITY have just the same lexical values space.
       	    case xs_IDREF             : ///
       	    case xs_ENTITY            : sat = check_constraints_for_xs_NCName(&SV);   break;
            default                   : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_within_a_branch");
	    }
    }

    if (!sat) throw XQUERY_EXCEPTION2(FORG0001, "The value does not conform to the facets defined for the target type");

    tuple_cell TV;
    
    if(need_string_normalization) 
    {
        stmt_str_buf res;
        if(TT == xs_token) { collapse_string_normalization(&SV, res); }
        else               { replace_string_normalization(&SV, res);  }
        TV = res.get_tuple_cell();
    }
    else
        TV = SV;

    TV.set_xtype(TT);
    return TV;
}

/*******************************************************************************
 * CAST FOR DATATYPES
 ******************************************************************************/
tuple_cell cast(const tuple_cell &SV, xmlscm_type TT)
{
    xmlscm_type ST = SV.get_atomic_type();

    // We must chech primitiveness of the ST and TT before checking their derivation
    // because specification stands: "Note that xs:untypedAtomic, xs:integer and 
    // the two derived types of xs:duration:xs:yearMonthDuration and xs:dayTimeDuration 
    // are treated as primitive types"
    bool ST_primitive = is_primitive(ST);
    bool TT_primitive = is_primitive(TT);

    // If condition is true, then casting is described in section 
    // "17.1 Casting from primitive types to primitive types"
    if (ST_primitive && TT_primitive)
        return cast_primitive(SV, TT);

    // Otherwise we have to deal with derived types as section
    // "17.2 Casting to derived types" stands

    if (ST == TT) 
        return SV; // The only one exception is xs:NOTATION, 
                   // but we do not consider datatypes derived from xs:NOTATION


    // If condition is true, then casting is described in section
    // "17.3 Casting from derived types to parent types"
    if (is_derived(ST, TT))
    {
        // If target type is xs_decimal we can't simply use set_xtype()!
		if(TT == xs_decimal)
		{
			if(ST == xs_decimal) return SV;
			tuple_cell TV(SV);
			TV.set_xtype(xs_integer);
			return cast_primitive(TV, TT);
		}
		else
		{
		    tuple_cell TV(SV);
            TV.set_xtype(TT);
		    return TV;
		}
    }

    xmlscm_type ST_base = primitive_base_type(ST);
    xmlscm_type TT_base = primitive_base_type(TT);

    if (ST_base == TT_base)
    {
        // Casting is described in "17.4 Casting within a branch of the type hierarchy"
        return cast_within_a_branch(SV, TT, TT_base);
    }
    else
    {
        // Casting is described in "17.5 Casting across the type hierarchy"

        // 1. Cast the SV, up the hierarchy, to the primitive type of the source, 
        //    as described in "17.3 Casting from derived types to parent types".

        //    a. If SV is an instance of xs:string or xs:untypedAtomic, check its 
        //       value against the pattern facet of TT, and raise an error [err:FORG0001] 
        //       if the check fails. 
        //    (AF: This is not clear and ignored...)
        tuple_cell res(SV);
        res.set_xtype(ST_base);

        // 2. Cast the value to the primitive type of TT, as described in 
        //    "17.1 Casting from primitive types to primitive types".
        res = cast_primitive(res, TT_base);

        // 3. Cast the value down to the TT, as described in "17.4 Casting 
        //    within a branch of the type hierarchy"
        return cast_within_a_branch(res, TT, TT_base);
    }
}

bool is_castable(const tuple_cell &SV, xmlscm_type TT)
{
	try
	{
		cast(SV, TT);	
		return true;
	}
	catch(SednaUserException)
	{
		return false;
	}
}
