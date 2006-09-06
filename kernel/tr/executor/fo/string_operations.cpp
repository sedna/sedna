/*
 * File:  string_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "vmm.h"
#include "string_operations.h"
#include "e_string.h"
#include "pstr_long.h"


/*******************************************************************************
 * FUNCTIONS TO ASSEMBLE AND DISASSEMBLE STRINGS: BEGIN
 ******************************************************************************/

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    NOT READY
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/*******************************************************************************
 * FUNCTIONS TO ASSEMBLE AND DISASSEMBLE STRINGS: END
 ******************************************************************************/


/*******************************************************************************
 * EQUALITY AND COMPARISON OF STRINGS: BEGIN
 ******************************************************************************/

static __int64 sign(int i)
{
    if (i > 0) return (__int64)1;
    if (i < 0) return (__int64)-1;
    return (__int64)i;
}

int fn_compare_pstr_long_vs_pstr_long(xptr str1, int str1len, xptr str2, int str2len)
{
    pstr_long_cursor cur1(str1);
    pstr_long_cursor cur2(str2);

    char *str1_ptr = NULL, *str2_ptr = NULL;
    int real_count = 0;
    int cmp_res = 0;

    str2_ptr = tr_globals::e_string_buf;
    int str2_part_len = cur2.copy_blk(str2_ptr);
    int str1_part_len = cur1.get_blk(&str1_ptr);
	xptr str1xptr = ADDR2XPTR(str1_ptr);

    while (true)
    {
        real_count = s_min(str1_part_len, str2_part_len);
        cmp_res = memcmp(str1_ptr, str2_ptr, real_count);

        if (cmp_res != 0) return cmp_res;
        if (real_count == str1len && real_count == str2len) return 0;
        if (real_count == str1len) return -1;
        if (real_count == str2len) return 1;

        str1_ptr += real_count;
        str1len -= real_count;
        str1_part_len -= real_count;
        str2_ptr += real_count;
        str2len -= real_count;
        str2_part_len -= real_count;

		if (str1_part_len == 0 && str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2.copy_blk(str2_ptr);
			str1_part_len = cur1.get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str1_part_len == 0)
		{
			str1_part_len = cur1.get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2.copy_blk(str2_ptr);
			CHECKP(str1xptr);
			continue;
		}

		throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
    }
}

int fn_compare_pstr_long_vs_estr_pstr_short(xptr str1, int str1len, xptr str2, int str2len)
{
    pstr_long_cursor cur1(str1);
    e_str_cursor cur2(str2, str2len);

    char *str1_ptr = NULL, *str2_ptr = NULL;
    int real_count = 0;
    int cmp_res = 0;

    str2_ptr = tr_globals::e_string_buf;
    int str2_part_len = cur2.copy_blk(str2_ptr);
    int str1_part_len = cur1.get_blk(&str1_ptr);
	xptr str1xptr = ADDR2XPTR(str1_ptr);

    while (true)
    {
        real_count = s_min(str1_part_len, str2_part_len);
        cmp_res = memcmp(str1_ptr, str2_ptr, real_count);

        if (cmp_res != 0) return cmp_res;
        if (real_count == str1len && real_count == str2len) return 0;
        if (real_count == str1len) return -1;
        if (real_count == str2len) return 1;

        str1_ptr += real_count;
        str1len -= real_count;
        str1_part_len -= real_count;
        str2_ptr += real_count;
        str2len -= real_count;
        str2_part_len -= real_count;

		if (str1_part_len == 0 && str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2.copy_blk(str2_ptr);
			str1_part_len = cur1.get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str1_part_len == 0)
		{
			str1_part_len = cur1.get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2.copy_blk(str2_ptr);
			CHECKP(str1xptr);
			continue;
		}

		throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
    }
}

int fn_compare_pstr_long_vs_mstr(xptr str1, int str1len, const char* str2)
{
    pstr_long_cursor cur(str1);
    char *str1_ptr = NULL;
    int str1_part_len = 0;
    int str2len = strlen(str2);
    int real_count = 0;
    int cmp_res = 0;

    while (true)
    {
        str1_part_len = cur.get_blk(&str1_ptr);
        real_count = s_min(str2len, str1_part_len);
        cmp_res = memcmp(str1_ptr, str2, real_count);

        if (cmp_res != 0) return cmp_res;

        if (real_count == str1len && real_count == str2len) return 0;
        if (real_count == str1len) return -1;
        if (real_count == str2len) return 1;

        if (real_count == str1_part_len)
        {
            str1len -= real_count;
            str2len -= real_count;
            str2 += real_count;
        }
        else
            throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
    }
}

int fn_compare_estr_pstr_short_vs_estr_pstr_short(xptr str1, int str1len, xptr str2, int str2len)
{
    CHECKP(str1);
    int str1_spc_blk = BLK_BEGIN_INT(XADDR(str1)) + PAGE_SIZE - (int)(XADDR(str1));

    CHECKP(str2);
    int str2_spc_blk = BLK_BEGIN_INT(XADDR(str2)) + PAGE_SIZE - (int)(XADDR(str2));

    int real_count = s_min(s_min(str1_spc_blk, str1len), s_min(str2_spc_blk, str2len));

    memcpy(tr_globals::e_string_buf, XADDR(str2), real_count);

    CHECKP(str1);
    int cmp_res = memcmp(XADDR(str1), tr_globals::e_string_buf, real_count);

    if (cmp_res != 0) return cmp_res;

    if (real_count == str1len && real_count == str2len) return 0;

    if (real_count == str1len) return -1;

    if (real_count == str2len) return 1;

    if (real_count == str1_spc_blk && real_count == str2_spc_blk)
    {
        xptr new_str1 = E_STR_PROLONGATION(str1);
        CHECKP(str2);
        xptr new_str2 = E_STR_PROLONGATION(str2);
        return fn_compare_estr_pstr_short_vs_estr_pstr_short(new_str1, str1len - real_count, 
                                                             new_str2, str2len - real_count);
    }

    if (real_count == str1_spc_blk)
    {
        xptr new_str1 = E_STR_PROLONGATION(str1);
        return fn_compare_estr_pstr_short_vs_estr_pstr_short(new_str1,          str1len - real_count, 
                                                             str2 + real_count, str2len - real_count);
    }

    if (real_count == str2_spc_blk)
    {
        CHECKP(str2);
        xptr new_str2 = E_STR_PROLONGATION(str2);
        return fn_compare_estr_pstr_short_vs_estr_pstr_short(str1 + real_count, str1len - real_count, 
                                                             new_str2,          str2len - real_count);
    }

    throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
}

int fn_compare_mstr_vs_estr_pstr_short(const char* str1, xptr str2, int str2len)
{
    CHECKP(str2);

    int str1len = strlen(str1);
    int str2_spc_blk = BLK_BEGIN_INT(XADDR(str2)) + PAGE_SIZE - (int)(XADDR(str2));
    int real_count = s_min(str1len, s_min(str2_spc_blk, str2len));

    int cmp_res = memcmp(str1, XADDR(str2), real_count);

    if (cmp_res != 0) return cmp_res;

    if (real_count == str1len && real_count == str2len) return 0;

    if (real_count == str1len) return -1;

    if (real_count == str2len) return 1;

    if (real_count == str2_spc_blk)
        return fn_compare_mstr_vs_estr_pstr_short(str1 + real_count, E_STR_PROLONGATION(str2), str2len - real_count);

    throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
}

inline int fn_compare_mstr_vs_mstr(const char* str1, const char* str2)
{
    return strcmp(str1, str2);
}

tuple_cell fn_compare(const tuple_cell &a1, const tuple_cell &a2, bool treat_xs_untypedAtomic_as_xs_string)
{
    if (   a1.is_atomic() && (a1.get_atomic_type() == xs_string || (treat_xs_untypedAtomic_as_xs_string && a1.get_atomic_type() == xs_untypedAtomic))
        && a2.is_atomic() && (a2.get_atomic_type() == xs_string || (treat_xs_untypedAtomic_as_xs_string && a2.get_atomic_type() == xs_untypedAtomic)))
        ;
    else
        throw USER_EXCEPTION2(XPTY0004, "Calling fn:compare on non-string values");

/*
    ASSERT(a1.get_type() == tc_light_atomic || 
           a1.get_type() == tc_heavy_atomic_estr || 
           a1.get_type() == tc_heavy_atomic_pstr_short || 
           a1.get_type() == tc_heavy_atomic_pstr_long);

    ASSERT(a2.get_type() == tc_light_atomic || 
           a2.get_type() == tc_heavy_atomic_estr || 
           a2.get_type() == tc_heavy_atomic_pstr_short || 
           a2.get_type() == tc_heavy_atomic_pstr_long);
*/

	switch (a1.get_type()) 
	{
	case tc_light_atomic: 
		switch (a2.get_type()) 
		{
		case tc_light_atomic: 
            return tuple_cell::atomic(sign(fn_compare_mstr_vs_mstr(a1.get_str_mem(), a2.get_str_mem())));

		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return tuple_cell::atomic(sign(fn_compare_mstr_vs_estr_pstr_short(a1.get_str_mem(), a2.get_str_vmm(), a2.get_strlen_vmm())));

		case tc_heavy_atomic_pstr_long:
            return tuple_cell::atomic(-sign(fn_compare_pstr_long_vs_mstr(a2.get_str_vmm(), a2.get_strlen_vmm(), a1.get_str_mem())));
		}
	case tc_heavy_atomic_estr:
	case tc_heavy_atomic_pstr_short:
		switch (a2.get_type()) 
		{
		case tc_light_atomic: 
            return tuple_cell::atomic(-sign(fn_compare_mstr_vs_estr_pstr_short(a2.get_str_mem(), a1.get_str_vmm(), a1.get_strlen_vmm())));

		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return tuple_cell::atomic(sign(fn_compare_estr_pstr_short_vs_estr_pstr_short(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm())));

		case tc_heavy_atomic_pstr_long:
            return tuple_cell::atomic(-sign(fn_compare_pstr_long_vs_estr_pstr_short(a2.get_str_vmm(), a2.get_strlen_vmm(), a1.get_str_vmm(), a1.get_strlen_vmm())));
		}
	case tc_heavy_atomic_pstr_long:
		switch (a2.get_type()) 
		{
		case tc_light_atomic: 
            return tuple_cell::atomic(sign(fn_compare_pstr_long_vs_mstr(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_mem())));
		case tc_heavy_atomic_estr:
		case tc_heavy_atomic_pstr_short:
            return tuple_cell::atomic(sign(fn_compare_pstr_long_vs_estr_pstr_short(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm())));

		case tc_heavy_atomic_pstr_long:
            return tuple_cell::atomic(sign(fn_compare_pstr_long_vs_pstr_long(a1.get_str_vmm(), a1.get_strlen_vmm(), a2.get_str_vmm(), a2.get_strlen_vmm())));
		}
	}

    throw USER_EXCEPTION2(SE1003, "Impossible case in fn_compare");
}


/*******************************************************************************
 * EQUALITY AND COMPARISON OF STRINGS: END
 ******************************************************************************/

