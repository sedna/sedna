/*
 * File:  tuple.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "tuple.h"
#include "e_string.h"
#include "pstr.h"
#include "pstr_long.h"


tuple_cell EMPTY_STRING_TC(tuple_cell::atomic_deep(xs_string, ""));


void tuple_cell::print(bool b) const
{
    if (b) printf("Tuple cell: ");

    switch(type)
    {
        case tc_eos				: if (b) printf("type = ");
                                  printf("eos");
                                  break;
        case tc_node			: if (b) printf("type = node ");
                                  get_node().print();
                                  break;
        case tc_light_atomic	: if (b) 
                                  {
                                      printf("type = light_atomic ");
                                      printf("xtype = %d ", xtype);
                                  }
                                  switch (xtype)
                                  {
                                      case xdt_untypedAtomic	: printf("\"%s\"", get_str_mem()); break;
                                      case xs_dateTime			: printf("[xs_dateTime]"); break;
                                      case xs_date				: get_xs_date().print(); break;
                                      case xs_time				: printf("[xs_time]"); break;
                                      case xs_duration			: printf("[xs_duration]"); break;
                                      case xdt_yearMonthDuration: printf("[xdt_yearMonthDuration]"); break;
                                      case xdt_dayTimeDuration	: printf("[xdt_dayTimeDuration]"); break;
                                      case xs_float				: printf("%g", get_xs_float()); break;
                                      case xs_double			: printf("%g", get_xs_double()); break;
                                      case xs_string			: printf("\"%s\"", get_str_mem()); break;
                                      case xs_normalizedString	: printf("\"%s\"", get_str_mem()); break;
                                      case xs_token				: printf("[xs_token]"); break;
                                      case xs_language			: printf("[xs_language]"); break;
                                      case xs_NMTOKEN			: printf("[xs_NMTOKEN]"); break;
                                      case xs_Name				: printf("\"%s\"", get_str_mem()); break;
                                      case xs_NCName			: printf("\"%s\"", get_str_mem()); break;
                                      case xs_ID				: printf("[xs_ID]"); break;
                                      case xs_IDREF				: printf("[xs_IDREF]"); break;
                                      case xs_ENTITY			: printf("[xs_ENTITY]"); break;
                                      case xs_decimal			: get_xs_decimal().print(); break;
                                      case xs_integer			: printf("%d", get_xs_integer()); break;
                                      case xs_gYearMonth		: printf("[xs_gYearMonth]"); break;
                                      case xs_gYear				: printf("[xs_gYear]"); break;
                                      case xs_gMonthDay			: printf("[xs_gMonthDay]"); break;
                                      case xs_gDay				: printf("[xs_gDay]"); break;
                                      case xs_gMonth			: printf("[xs_gMonth]"); break;
                                      case xs_boolean			: printf("%s", (get_xs_boolean() ? "true" : "false"));
                                      case xs_base64Binary		: printf("[xs_base64Binary]"); break;
                                      case xs_hexBinary			: printf("[xs_hexBinary]"); break;
                                      case xs_anyURI			: printf("\"%s\"", get_str_mem()); break;
                                      case xs_QName				: printf("\"%s\"", get_str_mem()); break;
                                      case xs_NOTATION			: printf("[xs_NOTATION]"); break;
                                      default					: printf("UNKNOWN");
                                  }
                                  break;
        case tc_heavy_atomic_estr:
                                  if (b)
                                  {
                                      printf("type = heavy_atomic_estr ");
                                      printf("xtype = %d\n", xtype);
                                  }
                                  else printf("heavy_atomic_estr");
                                  break;
        case tc_heavy_atomic_pstr_short	: 
                                  if (b)
                                  {
                                      printf("type = heavy_atomic_pstr ");
                                      printf("xtype = %d\n", xtype);
                                  }
                                  else printf("heavy_atomic_pstr");
                                  break;
        case tc_heavy_atomic_pstr_long	: 
                                  if (b)
                                  {
                                      printf("type = heavy_atomic_pstr_long ");
                                      printf("xtype = %d\n", xtype);
                                  }
                                  else printf("heavy_atomic_pstr_long");
                                  break;
        default					: printf("unknown type %d\n", type);
    }

    if (b) printf("\n");
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
    //printf("tuple::tuple(const tuple &t) begin\n");

    eos = t.eos;
    cells_number = t.cells_number; 
    if (t.cells == NULL) cells = NULL;
    else
    {
        cells = new tuple_cell[cells_number]; 
        for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }

    //printf("tuple::tuple(const tuple &t) end\n");
}

tuple& tuple::operator=(const tuple& t)
{
    //printf("tuple::operator=(const tuple& t) begin\n");

    clear();

    eos = t.eos;
    cells_number = t.cells_number; 
    if (t.cells == NULL) cells = NULL;
    else
    {
        cells = new tuple_cell[cells_number]; 
        for (int i = 0; i < cells_number; i++) cells[i] = t.cells[i];
    }

    //printf("tuple::operator=(const tuple& t) end\n");

    return *this;
}

void tuple::print() const
{
    printf("Tuple: eos = %d; cells_number = %d\n", eos, cells_number);
    for (int i = 0; i < cells_number; i++)
    {
        printf("tuple_cell[%d] ", i);
        cells[i].print();
    }
}

