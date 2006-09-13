/*
 * File:  other_fo.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OTHER_FO_H
#define _OTHER_FO_H

#include "sedna.h"
#include "PPBase.h"


/*******************************************************************************
 * OPERATORS ON xs:NOTATION
 ******************************************************************************/
tuple_cell op_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_not_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2);


/*******************************************************************************
 * OPERATORS ON xs:anyURI
 ******************************************************************************/
/* !!! FIX ME: Implement these functions here

   fn:resolve-uri($relative as xs:string?) as xs:anyURI?
   fn:resolve-uri($relative as xs:string?, $base as xs:string) as xs:anyURI?
*/

/*******************************************************************************
 * FUNCTIONS RELATED TO xs:QName
 ******************************************************************************/
tuple_cell op_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_not_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2);

/* !!! FIX ME: Implement other functions related to xs:QName here */


#endif
