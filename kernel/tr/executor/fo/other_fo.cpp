/*
 * File:  other_fo.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/fo/other_fo.h"


/*******************************************************************************
 * OPERATORS ON xs:NOTATION
 ******************************************************************************/
tuple_cell op_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2)
    // !!! FIX ME
    { throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented"); }

tuple_cell op_not_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2)
    // !!! FIX ME
    { throw USER_EXCEPTION2(SE1002, "XML Schema simple type is not implemented"); }


/*******************************************************************************
 * FUNCTIONS RELATED TO xs:QName
 ******************************************************************************/
tuple_cell op_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2)
{ 
    // xs:QNames are always light atomic
    U_ASSERT(a1.is_light_atomic());
    U_ASSERT(a2.is_light_atomic());

    char *qname1 = a1.get_str_mem();
    char *qname2 = a2.get_str_mem();
    const char *uri1 = xs_QName_get_uri(qname1);
    const char *uri2 = xs_QName_get_uri(qname2);
    const char *local1 = xs_QName_get_local_name(qname1);
    const char *local2 = xs_QName_get_local_name(qname2);

    return tuple_cell::atomic(_xs_QName_equal(uri1, local1, uri2, local2));
}

tuple_cell op_not_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2)
{ 
    // xs:QNames are always light atomic
    U_ASSERT(a1.is_light_atomic());
    U_ASSERT(a2.is_light_atomic());

    char *qname1 = a1.get_str_mem();
    char *qname2 = a2.get_str_mem();
    const char *uri1 = xs_QName_get_uri(qname1);
    const char *uri2 = xs_QName_get_uri(qname2);
    const char *local1 = xs_QName_get_local_name(qname1);
    const char *local2 = xs_QName_get_local_name(qname2);

    return tuple_cell::atomic(_xs_QName_not_equal(uri1, local1, uri2, local2));
}
