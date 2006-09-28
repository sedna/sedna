/*
 * File:  casting_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include <math.h>
#include "casting_operations.h"
#include "xs_helper.h"
#include "dm_accessors.h"
#include "base64Binary.h"


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



//this function checks constraints on xs:hexBinary lexical representation
//if given value doesn't conform to constraints valid will be 'false'
//else valid will be true and res_it will contain canonical representation of the 
//given value.
template <class Iterator>
static inline void check_constraints_for_xs_hexBinary(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it, bool *valid)
{
    *valid = false;
    unsigned char value;             
    unsigned char delta = 'A'-'a';   //used to create canonical representation with upper case symbols;
    int counter = 0;                 //number of symbols in hexBinary must be even;
    while (start < end)
    {
        value = *start;
        if( !(('0' <= value && value <= '9') || 
              ('a' <= value && value <= 'f') || 
              ('A' <= value && value <= 'F')) ) return;
        if('a' <= value && value <= 'f') value += delta;
        *res_it = value;
        ++counter;
        ++res_it;
        ++start;
    }
    if(!(counter & 1)) *valid = true;   //chech evenness at last;
}

inline tuple_cell cast_string_type_to_xs_hexBinary(const tuple_cell &c)
{ 
    if (e_string_last_blk==XNULL) 
    {
        vmm_alloc_tmp_block(&e_string_last_blk);
        e_str_blk_hdr::init(XADDR(e_string_last_blk));
        e_string_first_blk = e_string_last_blk;
    }
    
    e_string_o_iterator<unsigned char> res_it;
    xptr start_pos = res_it.pos;
    bool valid;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(check_constraints_for_xs_hexBinary, &c, res_it, &valid);
    
    if(!valid) throw USER_EXCEPTION2(FORG0001, "The value does not conform to the lexical constraints defined for the xs:hexBinary type.");
    int reslen = get_length_of_last_str(start_pos);  //FIXME!!! Possibly it must be __int64???
    return tuple_cell::atomic_estr(xs_hexBinary, reslen, start_pos);
}




inline tuple_cell cast_string_type_to_xs_anyURI(const tuple_cell &c)
    // !!! FIX ME
    { throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented"); }

inline tuple_cell cast_string_type_to_xs_QName(const tuple_cell &c)
{
    // !!! FIXME: check lexical representation
    tuple_cell res(c);
    res.set_xtype(xs_QName);
    return res;
}


/******************************************************************************/

inline tuple_cell cast_xs_float_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_double_lexical_representation(tr_globals::mem_str_buf, (double)c.get_xs_float());
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_double_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_double_lexical_representation(tr_globals::mem_str_buf, c.get_xs_double());
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_decimal_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    c.get_xs_decimal().get_c_str(tr_globals::mem_str_buf);
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_integer_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_integer_lexical_representation(tr_globals::mem_str_buf, c.get_xs_integer());
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_dateTime_to_string_type(const tuple_cell &c, xmlscm_type xtype, xmlscm_type res_type)
{
    if (xtype == xs_duration || xtype == xs_yearMonthDuration || xtype == xs_dayTimeDuration )
    	get_xs_dateTime_lexical_representation(tr_globals::mem_str_buf, XMLDateTime(c.get_xs_duration(), xtype));
    else
	get_xs_dateTime_lexical_representation(tr_globals::mem_str_buf, XMLDateTime(c.get_xs_dateTime(), xtype));
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_boolean_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{
    get_xs_boolean_lexical_representation(tr_globals::mem_str_buf, c.get_xs_boolean());
    return tuple_cell::atomic_deep(res_type, tr_globals::mem_str_buf);
}

inline tuple_cell cast_xs_anyURI_to_string_type(const tuple_cell &c, xmlscm_type res_type)
{ // !!! FIX ME: some conversion needed
    tuple_cell res(c);
    res.set_xtype(res_type);
    return res;
}



/******************************************************************************/

inline tuple_cell cast_xs_hexBinary_to_xs_base64Binary(const tuple_cell &c)
    // !!! FIX ME
    { throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented"); }

inline tuple_cell cast_xs_base64Binary_to_xs_hexBinary(const tuple_cell &c)
    // !!! FIX ME
    { throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented"); }



/******************************************************************************/

static tuple_cell _cast_is_not_supported(xmlscm_type from, xmlscm_type to)
{
    char buf[128];
    strcpy(buf, "Cast from ");
    strcat(buf, xmlscm_type2c_str(from));
    strcat(buf, " to ");
    strcat(buf, xmlscm_type2c_str(to));
    strcat(buf, " is not supported");
    throw USER_EXCEPTION2(XPTY0004, buf);
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
        case xs_QName			: { // !!! FIX ME: don't know what to do
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        case xs_NOTATION		: { // !!! FIX ME: don't know what to do
                                      tuple_cell res(c);
                                      res.set_xtype(xs_string);
                                      return res;
                                  }
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_untypedAtomic);
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
        case xs_float			: return tuple_cell::atomic((__int64)(floor(c.get_xs_float())));
        case xs_double			: return tuple_cell::atomic((__int64)(floor(c.get_xs_double())));
        case xs_decimal			: return tuple_cell::atomic(c.get_xs_decimal().get_int());
        case xs_integer 		: return c;
        case xs_boolean			: return c.get_xs_boolean() ? tuple_cell::atomic((__int64)1)
                                                            : tuple_cell::atomic((__int64)0);
        default					: return _cast_is_not_supported(c.get_atomic_type(), xs_decimal);
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
        case xs_untypedAtomic	 : 
        case xs_string			 : return cast_string_type_to_xs_dateTime(c, type);
        case xs_duration		 :
        case xs_yearMonthDuration:
        case xs_dayTimeDuration  :
            switch (type)
            {
                case xs_duration         : 
                case xs_yearMonthDuration: 
                case xs_dayTimeDuration  : return (c.get_atomic_type() == type) ? 
                                                   c : 
						   cast_xs_duration_to_xs_duration(c, type);
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
        default					: return _cast_is_not_supported(c.get_atomic_type(), type);
    }
}

tuple_cell cast_primitive_to_xs_boolean(const tuple_cell &c)
{
    U_ASSERT(c.is_atomic());

    switch (c.get_atomic_type())
    {
        case xs_untypedAtomic	: 
        case xs_string			: return cast_string_type_to_xs_boolean(c);
        case xs_float			: return c.get_xs_float()   == (float) 0.0e0 ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_double			: return c.get_xs_double()  == (double)0.0e0 ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_decimal			: return c.get_xs_decimal().is_zero()        ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
        case xs_integer 		: return c.get_xs_integer() == (__int64)0    ? tuple_cell::atomic(false) : tuple_cell::atomic(true);
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
static bool check_constraints_for_xs_nonPositiveInteger(const __int64& value)
{
    return value <= 0;
}

static bool check_constraints_for_xs_negativeInteger(const __int64& value)
{
    return value < 0;
}

static bool check_constraints_for_xs_long(const __int64& value)
{
    // because __int64 fully conforms to xs:long...
    return true;
}

static bool check_constraints_for_xs_int(const __int64& value)
{
    return value >= (__int64)(-2147483647 - 1) && value <= (__int64)2147483647;
}

static bool check_constraints_for_xs_short(const __int64& value)
{
    return value >= (__int64)-32768 && value <= (__int64)32767;
}

static bool check_constraints_for_xs_byte(const __int64& value)
{
    return value >= (__int64)-128 && value <= (__int64)127;
}

static bool check_constraints_for_xs_nonNegativeInteger(const __int64& value)
{
    return value >= 0;
}

static bool check_constraints_for_xs_unsignedLong(const __int64& value)
{
    return value >= 0;
}

static bool check_constraints_for_xs_unsignedInt(const __int64& value)
{
    return value >= 0 && value <= (__int64)4294967295;
}

static bool check_constraints_for_xs_unsignedShort(const __int64& value)
{
    return value >= 0 && value <= (__int64)65535;
}

static bool check_constraints_for_xs_unsignedByte(const __int64& value)
{
    return value >= 0 && value <= (__int64)255;
}

static bool check_constraints_for_xs_positiveInteger(const __int64& value)
{
    return value > 0;
}


template <class Iterator>
static inline void check_constraints_for_xs_normalizedString(Iterator &start, const Iterator &end, bool *res)
{
	(*res) = false;

	while (start < end)
	{
		if (*start == '\n' || *start == '\r' || *start == '\t') return;
		start++;
	}

	(*res) = true;
}

template <class Iterator>
static inline void check_constraints_for_xs_token(Iterator &start, const Iterator &end, bool *res)
{
	(*res) = start == end;
	
	if(*res) return;
	
	unsigned char previous = *start;

	while (start < end)
	{
		if (*start == '\n' || *start == '\r' || *start == '\t' || (*start == ' ' && previous == ' ')) return;
        previous = *start;
		start++;
	}

	if(previous == ' ') return;

	(*res) = true;
}

static inline bool check_constraints_for_xs_language(tuple_cell *value)
{
	char const* regex = "^([a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*)$";
	return collation_handler->matches(value, regex);
}




static tuple_cell cast_within_a_branch(const tuple_cell &SV, xmlscm_type TT, xmlscm_type base_type)
{
    if (TT == base_type) return SV;

    // only xs:integer and xs:string have a branch
    U_ASSERT(base_type == xs_integer || base_type == xs_string);

    bool sat = false;
    if (base_type == xs_integer)
    {
        __int64 value = SV.get_xs_integer();

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
        switch (TT)
        {
            case xs_string            : sat = true; break;
            case xs_normalizedString  : STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(check_constraints_for_xs_normalizedString, &SV, &sat); break;
       	    case xs_token             : STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(check_constraints_for_xs_token, &SV, &sat); break;
            case xs_language          : sat = check_constraints_for_xs_language(&SV); break;
       	    case xs_NMTOKEN           : 
       	    case xs_Name              : 
       	    case xs_NCName            : 
       	    case xs_ID                : 
       	    case xs_IDREF             :
       	    case xs_ENTITY            : sat = true; break;
            default                   : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type passed to cast_within_a_branch");
	    }
    }

    if (!sat) throw USER_EXCEPTION2(FORG0001, "The value does not conform to the facets defined for the target type");

    tuple_cell TV(SV);
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
        tuple_cell TV(SV);
        TV.set_xtype(TT);
        return TV;
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
	catch(SednaUserException &e)
	{
		return false;
	}
}
