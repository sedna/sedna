/*
 * File:  string_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "vmm.h"
#include "string_operations.h"
#include "e_string.h"
#include "pstr_long.h"
#include "PPBase.h"


/*******************************************************************************
 * EQUALITY AND COMPARISON OF STRINGS: BEGIN
 ******************************************************************************/

inline int fn_compare_pstr_long_vs_pstr_long(xptr str1, int str1len, xptr str2, int str2len, CollationHandler* handler)
{
    pstr_long_cursor cur1(str1);
    pstr_long_cursor cur2(str2);

    return handler->compare(&cur1, &cur2);
}

inline int fn_compare_pstr_long_vs_estr_pstr_short(xptr str1, int str1len, xptr str2, int str2len, CollationHandler* handler)
{
    pstr_long_cursor cur1(str1);
    estr_cursor cur2(str2, str2len);

    return handler->compare(&cur1, &cur2);
}

inline int fn_compare_pstr_long_vs_mstr(xptr str1, int str1len, const char* str2, CollationHandler* handler)
{
    pstr_long_cursor cur(str1);

    return handler->compare(&cur, str2);
}

inline int fn_compare_estr_pstr_short_vs_estr_pstr_short(xptr str1, int str1len, xptr str2, int str2len, CollationHandler* handler)
{
    estr_cursor cur1(str1, str1len);
    estr_cursor cur2(str2, str2len);

    return handler->compare(&cur1, &cur2);
}

inline int fn_compare_mstr_vs_estr_pstr_short(const char* str1, xptr str2, int str2len, CollationHandler* handler)
{
    estr_cursor cur2(str2, str2len);
    
    return -(handler->compare(&cur2, str1));
}

inline int fn_compare_mstr_vs_mstr(const char* str1, const char* str2, CollationHandler* handler)
{
    return handler->compare(str1, str2);
}

int fn_compare(const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler)
{
    U_ASSERT(a1.is_atomic() && !is_fixed_size_type(a1.get_atomic_type()));
    U_ASSERT(a2.is_atomic() && !is_fixed_size_type(a2.get_atomic_type()));
    U_ASSERT(handler);

    if (a1.get_atomic_type() == xs_base64Binary || a1.get_atomic_type() == xs_hexBinary)
        handler = charset_handler->get_unicode_codepoint_collation();


	switch (a1.get_type()) 
	{
	case tc_light_atomic_var_size: 
		switch (a2.get_type()) 
		{
		case tc_light_atomic_var_size: 
            return fn_compare_mstr_vs_mstr(a1.get_str_mem(), a2.get_str_mem(), handler);

		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return fn_compare_mstr_vs_estr_pstr_short(a1.get_str_mem(), a2.get_str_vmm(), a2.get_strlen_vmm(), handler);

		case tc_heavy_atomic_pstr_long:
            return -(fn_compare_pstr_long_vs_mstr(a2.get_str_vmm(), a2.get_strlen_vmm(), a1.get_str_mem(), handler));
		}
	case tc_heavy_atomic_estr:
	case tc_heavy_atomic_pstr_short:
		switch (a2.get_type()) 
		{
		case tc_light_atomic_var_size: 
            return -(fn_compare_mstr_vs_estr_pstr_short(a2.get_str_mem(), a1.get_str_vmm(), a1.get_strlen_vmm(), handler));

		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return fn_compare_estr_pstr_short_vs_estr_pstr_short(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm(), handler);

		case tc_heavy_atomic_pstr_long:
            return -(fn_compare_pstr_long_vs_estr_pstr_short(a2.get_str_vmm(), a2.get_strlen_vmm(), a1.get_str_vmm(), a1.get_strlen_vmm(), handler));
		}
	case tc_heavy_atomic_pstr_long:
		switch (a2.get_type()) 
		{
		case tc_light_atomic_var_size: 
            return fn_compare_pstr_long_vs_mstr(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_mem(), handler);
		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return fn_compare_pstr_long_vs_estr_pstr_short(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm(), handler);

		case tc_heavy_atomic_pstr_long:
            return fn_compare_pstr_long_vs_pstr_long(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm(), handler);
		}
	}

    throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
}


/*******************************************************************************
 * EQUALITY AND COMPARISON OF STRINGS: END
 ******************************************************************************/

