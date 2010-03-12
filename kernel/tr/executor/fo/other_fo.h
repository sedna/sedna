/*
 * File:  other_fo.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OTHER_FO_H
#define _OTHER_FO_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


/*******************************************************************************
 * OPERATORS ON xs:NOTATION
 ******************************************************************************/
tuple_cell op_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_not_equal_xs_NOTATION(const tuple_cell &a1, const tuple_cell &a2);


/*******************************************************************************
 * FUNCTIONS RELATED TO xs:QName
 ******************************************************************************/
tuple_cell op_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_not_equal_xs_QName(const tuple_cell &a1, const tuple_cell &a2);


#endif
