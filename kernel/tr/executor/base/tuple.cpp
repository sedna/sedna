/*
 * File:  tuple.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "tuple.h"
#include "e_string.h"
#include "pstr.h"
#include "pstr_long.h"
#include "d_printf.h"


tuple_cell EMPTY_STRING_TC(tuple_cell::atomic_deep(xs_string, ""));


void tuple_cell::print(bool b) const
{
    if (b) d_printf1("Tuple cell: ");

    switch(type)
    {
        case tc_eos				: if (b) d_printf1("type = ");
                                  d_printf1("eos");
                                  break;
        case tc_node			: if (b) d_printf1("type = node ");
                                  get_node().print();
                                  break;
        case tc_light_atomic	: if (b) 
                                  {
                                      d_printf1("type = light_atomic ");
                                      d_printf2("xtype = %d ", xtype);
                                  }
                                  switch (xtype)
                                  {
                                      case xdt_untypedAtomic	: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_dateTime			: d_printf1("[xs_dateTime]"); break;
                                      case xs_date				: printf("'%s'",get_xs_dateTime()); break;
                                      case xs_time				: d_printf1("[xs_time]"); break;
                                      case xs_duration			: d_printf1("[xs_duration]"); break;
                                      case xdt_yearMonthDuration: d_printf1("[xdt_yearMonthDuration]"); break;
                                      case xdt_dayTimeDuration	: d_printf1("[xdt_dayTimeDuration]"); break;
                                      case xs_float				: d_printf2("%g", get_xs_float()); break;
                                      case xs_double			: d_printf2("%g", get_xs_double()); break;
                                      case xs_string			: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_normalizedString	: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_token				: d_printf1("[xs_token]"); break;
                                      case xs_language			: d_printf1("[xs_language]"); break;
                                      case xs_NMTOKEN			: d_printf1("[xs_NMTOKEN]"); break;
                                      case xs_Name				: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_NCName			: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_ID				: d_printf1("[xs_ID]"); break;
                                      case xs_IDREF				: d_printf1("[xs_IDREF]"); break;
                                      case xs_ENTITY			: d_printf1("[xs_ENTITY]"); break;
                                      case xs_decimal			: get_xs_decimal().print(); break;
                                      case xs_integer			: d_printf2("%d", get_xs_integer()); break;
                                      case xs_gYearMonth		: d_printf1("[xs_gYearMonth]"); break;
                                      case xs_gYear				: d_printf1("[xs_gYear]"); break;
                                      case xs_gMonthDay			: d_printf1("[xs_gMonthDay]"); break;
                                      case xs_gDay				: d_printf1("[xs_gDay]"); break;
                                      case xs_gMonth			: d_printf1("[xs_gMonth]"); break;
                                      case xs_boolean			: d_printf2("%s", (get_xs_boolean() ? "true" : "false"));
                                      case xs_base64Binary		: d_printf1("[xs_base64Binary]"); break;
                                      case xs_hexBinary			: d_printf1("[xs_hexBinary]"); break;
                                      case xs_anyURI			: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_QName				: d_printf2("\"%s\"", get_str_mem()); break;
                                      case xs_NOTATION			: d_printf1("[xs_NOTATION]"); break;
                                      default					: d_printf1("UNKNOWN");
                                  }
                                  break;
        case tc_heavy_atomic_estr:
                                  if (b)
                                  {
                                      d_printf1("type = heavy_atomic_estr ");
                                      d_printf2("xtype = %d\n", xtype);
                                  }
                                  else d_printf1("heavy_atomic_estr");
                                  break;
        case tc_heavy_atomic_pstr_short	: 
                                  if (b)
                                  {
                                      d_printf1("type = heavy_atomic_pstr ");
                                      d_printf2("xtype = %d\n", xtype);
                                  }
                                  else d_printf1("heavy_atomic_pstr");
                                  break;
        case tc_heavy_atomic_pstr_long	: 
                                  if (b)
                                  {
                                      d_printf1("type = heavy_atomic_pstr_long ");
                                      d_printf2("xtype = %d\n", xtype);
                                  }
                                  else d_printf1("heavy_atomic_pstr_long");
                                  break;
        default					: d_printf2("unknown type %d\n", type);
    }

    if (b) d_printf1("\n");
}

tuple_cell tuple_cell::make_sure_light_atomic(const tuple_cell& tc)
{
    if (tc.is_light_atomic()) return tc;
    if (tc.is_heavy_atomic())
    {
        /*if (tc.size > MAX_MEM_STR_SIZE)
            throw USER_EXCEPTION2(SE1003, "Heave atomic is too large in call to tuple_cell::make_sure_light_atomic");
		 */
		 int sizep = tc.get_strlen_vmm();
		 char *tmp = new char[sizep + 1];
		 copy_text(tmp, tc.get_str_vmm(), sizep);
		 tmp[sizep] = '\0';
		 return atomic(tc.get_atomic_type(), tmp);
    }
    throw USER_EXCEPTION2(SE1003, "None-atomic value in call to tuple_cell::make_sure_light_atomic");
}

void tuple_cell::copy_string(char *buf)
{
    ///!!! check that size <= MAX_MEM_STR_SIZE
    switch (type)
    {
//DATE: make sure we can copy strings with trailing 0's
        case tc_light_atomic:           strcpy(buf, str_ptr.get()); 
                                        return;
        case tc_heavy_atomic_estr:      e_str_copy_to_buffer(buf, *(xptr*)(&data), size); 
                                        buf[size] = '\0';
                                        return;
        case tc_heavy_atomic_pstr_short:e_str_copy_to_buffer(buf, *(xptr*)(&data), size);
                                        buf[size] = '\0';
                                        return;
        case tc_heavy_atomic_pstr_long: pstr_long_copy_to_buffer(buf, *(xptr*)(&data), size); 
                                        buf[size] = '\0';
                                        return;
        default:                        throw USER_EXCEPTION2(SE1003, "None-atomic value in call to tuple_cell::copy_string");
    }
}


tuple::tuple(const tuple& t)
{ 
    //d_printf1("tuple::tuple(const tuple &t) begin\n");

    eos = t.eos;
    cells_number = t.cells_number; 
    if (t.cells == NULL) cells = NULL;
    else
    {
        cells = new tuple_cell[cells_number]; 
        for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }

    //d_printf1("tuple::tuple(const tuple &t) end\n");
}

tuple& tuple::operator=(const tuple& t)
{
    //d_printf1("tuple::operator=(const tuple& t) begin\n");

    clear();

    eos = t.eos;
    cells_number = t.cells_number; 
    if (t.cells == NULL) cells = NULL;
    else
    {
        cells = new tuple_cell[cells_number]; 
        for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }

    //d_printf1("tuple::operator=(const tuple& t) end\n");

    return *this;
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

