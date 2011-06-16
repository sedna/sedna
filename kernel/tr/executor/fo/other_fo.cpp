/*
 * File:  other_fo.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/fo/other_fo.h"
#include "tr/executor/base/xsd.h"


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

    xsd::QName qname1 = xsd::QName::deserialize(a1.get_str_mem());
    xsd::QName qname2 = xsd::QName::deserialize(a2.get_str_mem());

    return tuple_cell::atomic(qname1.equals(qname2));
}

tuple_cell op_not_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2)
{ 
    // xs:QNames are always light atomic
    U_ASSERT(a1.is_light_atomic());
    U_ASSERT(a2.is_light_atomic());

    xsd::QName qname1 = xsd::QName::deserialize(a1.get_str_mem());
    xsd::QName qname2 = xsd::QName::deserialize(a2.get_str_mem());

    return tuple_cell::atomic(!qname1.equals(qname2));
}
