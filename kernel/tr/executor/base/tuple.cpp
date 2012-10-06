/*
 * File:  tuple.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/strings/e_string.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "common/errdbg/d_printf.h"
#include "tr/executor/base/dm_accessors.h"


tuple_cell EMPTY_STRING_TC(tuple_cell::atomic_deep(xs_string, ""));


void tuple_cell::print(bool b) const
{
    if (b) d_printf1("Tuple cell: ");

    switch (get_type())
    {
        case tc_eos:
            if (b) d_printf1("type = ");
            d_printf1("eos");
            break;

        case tc_node:
            if (b) d_printf1("type = node ");
            get_node().print();
            break;

        case tc_light_atomic_fix_size:
        case tc_light_atomic_var_size:
            if (b)
            {
                d_printf1("type = light_atomic ");
                d_printf2("atomic type = %d ", get_atomic_type());
            }
            switch (get_atomic_type())
            {
                case xs_untypedAtomic    : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_dateTime         : d_printf1("[xs_dateTime]"); break;
                case xs_date             : d_printf1("[xs_date]"); break;
                case xs_time             : d_printf1("[xs_time]"); break;
                case xs_duration         : d_printf1("[xs_duration]"); break;
                case xs_yearMonthDuration: d_printf1("[xs_yearMonthDuration]"); break;
                case xs_dayTimeDuration  : d_printf1("[xs_dayTimeDuration]"); break;
                case xs_float            : d_printf2("%g", get_xs_float()); break;
                case xs_double           : d_printf2("%g", get_xs_double()); break;
                case xs_string           : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_normalizedString : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_token            : d_printf1("[xs_token]"); break;
                case xs_language         : d_printf1("[xs_language]"); break;
                case xs_NMTOKEN          : d_printf1("[xs_NMTOKEN]"); break;
                case xs_Name             : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_NCName           : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_ID               : d_printf1("[xs_ID]"); break;
                case xs_IDREF            : d_printf1("[xs_IDREF]"); break;
                case xs_ENTITY           : d_printf1("[xs_ENTITY]"); break;
                case xs_decimal          : get_xs_decimal().print(); break;
                case xs_integer          : d_printf2("%d", get_xs_integer()); break;
                case xs_gYearMonth       : d_printf1("[xs_gYearMonth]"); break;
                case xs_gYear            : d_printf1("[xs_gYear]"); break;
                case xs_gMonthDay        : d_printf1("[xs_gMonthDay]"); break;
                case xs_gDay             : d_printf1("[xs_gDay]"); break;
                case xs_gMonth           : d_printf1("[xs_gMonth]"); break;
                case xs_boolean          : d_printf2("%s", (get_xs_boolean() ? "true" : "false"));
                case xs_base64Binary     : d_printf1("[xs_base64Binary]"); break;
                case xs_hexBinary        : d_printf1("[xs_hexBinary]"); break;
                case xs_anyURI           : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_QName            : d_printf2("\"%s\"", get_str_mem()); break;
                case xs_NOTATION         : d_printf1("[xs_NOTATION]"); break;
                default	                 : d_printf1("UNKNOWN");
            }
            break;

        case tc_heavy_atomic_estr:
            if (b)
            {
                d_printf1("type = heavy_atomic_estr ");
                d_printf2("atomic type = %d\n", get_atomic_type());
            }
            else d_printf1("heavy_atomic_estr");
            break;

        case tc_heavy_atomic_pstr_short:
            if (b)
            {
                d_printf1("type = heavy_atomic_pstr ");
                d_printf2("atomic type = %d\n", get_atomic_type());
            }
            else d_printf1("heavy_atomic_pstr");
            break;

        case tc_heavy_atomic_pstr_long:
            if (b)
            {
                d_printf1("type = heavy_atomic_pstr_long ");
                d_printf2("atomic type = %d\n", get_atomic_type());
            }
            else d_printf1("heavy_atomic_pstr_long");
            break;

        default:
            d_printf2("unknown type %d\n", get_type());
    }

    if (b) d_printf1("\n");
}

std::string tuple_cell::type2string() const
{
    std::string res;

    switch (get_type())
    {
        case tc_eos:
            res = "empty sequence";
            break;

        case tc_node:
            res = node_type2string(get_node());
            break;

        case tc_light_atomic_fix_size:
        case tc_light_atomic_var_size:
        case tc_heavy_atomic_estr:
        case tc_heavy_atomic_pstr_short:
        case tc_heavy_atomic_pstr_long:
            res = xmlscm_type2c_str(get_atomic_type());
            break;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected type of the tuple_cell in type2string");
    }

    return res;
}

tuple_cell tuple_cell::make_sure_light_atomic(const tuple_cell& tc)
{
    if (tc.is_light_atomic()) return tc;

    if (tc.is_heavy_atomic())
    {
         /* Check that we will no be out of size_t bounds */ 
         if ((uint64_t)tc.get_strlen_vmm() > SIZE_MAX-1)
            throw USER_EXCEPTION2(SE1003, "Too large atomic string");
		 
		 /* Convsersion is valid since we've checked length */
         size_t sizep = (size_t)tc.get_strlen_vmm();
		 char *tmp = new char[sizep + 1];
         tc.copy_string(tmp);
		 return atomic(tc.get_atomic_type(), tmp);
    }
    throw USER_EXCEPTION2(SE1003, "None-atomic value in call to tuple_cell::make_sure_light_atomic");
}

char* tuple_cell::copy_string(char *buf) const
{
    switch (get_type())
    {
        case tc_light_atomic_var_size:  return strcpy(buf, get_str_mem());
        case tc_heavy_atomic_estr:
        case tc_heavy_atomic_pstr_short:estr_copy_to_buffer(buf, *(xptr*)(&data), get_strlen_vmm());
                                        buf[get_strlen_vmm()] = '\0';
                                        return buf;
        case tc_heavy_atomic_pstr_long: pstr_long_copy_to_buffer2(buf, *(xptr*)(&data), get_strlen_vmm());
                                        buf[get_strlen_vmm()] = '\0';
                                        return buf;
        default:                        throw USER_EXCEPTION2(SE1003, "None-atomic value in call to tuple_cell::copy_string");
    }
}

char* tuple_cell::copy_string(char *buf, size_t n) const
{
    switch (get_type())
    {
        case tc_light_atomic_var_size:  return strncpy(buf, get_str_mem(), n);

        case tc_heavy_atomic_estr:
        case tc_heavy_atomic_pstr_short:if ((uint64_t)get_strlen_vmm() < n)
                                        {
                                            estr_copy_to_buffer(buf, *(xptr*)(&data), get_strlen_vmm());
                                            buf[get_strlen_vmm()] = '\0';
                                        }
                                        else
                                        {
                                            estr_copy_to_buffer(buf, *(xptr*)(&data), n);
                                        }
                                        return buf;

        case tc_heavy_atomic_pstr_long: if ((uint64_t)get_strlen_vmm() < n)
                                        {
                                            pstr_long_copy_to_buffer2(buf, *(xptr*)(&data), get_strlen_vmm());
                                            buf[get_strlen_vmm()] = '\0';
                                        }
                                        else
                                        {
                                            pstr_long_copy_to_buffer2(buf, *(xptr*)(&data), n);
                                        }
                                        return buf;

        default:                        throw USER_EXCEPTION2(SE1003, "None-atomic value in call to tuple_cell::copy_string");
    }
}


void tuple::print() const
{
    d_printf3("Tuple: eos = %d; cells_number = %d\n", eos, cells_number);
    for (int i = 0; i < cells_number; i++)
    {
        d_printf2("tuple_cell[%d] ", i);
        cells[i].print();
    }
}

